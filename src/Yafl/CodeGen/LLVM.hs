module Yafl.CodeGen.LLVM where

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.List (find)
import qualified Data.Map as Map
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as AST.Const
import qualified LLVM.AST.IntegerPredicate as AST.IntegerPredicate
import qualified LLVM.AST.Type as AST.Type
import qualified LLVM.IRBuilder.Constant as Const
import qualified LLVM.IRBuilder.Instruction as Instr
import qualified LLVM.IRBuilder.Module as Mod
import qualified LLVM.IRBuilder.Monad as IR
import qualified Yafl.Core.Builtins as Core.Builtins
import qualified Yafl.Core.Syntax as C

toShortBS :: String -> ShortByteString
toShortBS = SBS.toShort . BSC.pack

data CodeGenEnv = CodeGenEnv
  { defaultSwitchLabel :: AST.Name,
    varTable :: Map.Map C.Var AST.Operand
  }

type ModuleGen = Mod.ModuleBuilder

type CodeGen = ReaderT CodeGenEnv (IR.IRBuilderT ModuleGen)

extendVarTable :: C.Var -> AST.Operand -> CodeGen a -> CodeGen a
extendVarTable x o = do
  local (\r -> r {varTable = Map.insert x o (varTable r)})

extendVarTableMany :: [(C.Var, AST.Operand)] -> CodeGen a -> CodeGen a
extendVarTableMany bs m = foldr (\(x, o) -> extendVarTable x o) m bs

externs :: [(String, [AST.Type], AST.Type)]
externs =
  [ ("malloc", [AST.Type.i32], AST.Type.ptr AST.Type.i8),
    ("match_error", [], AST.Type.void),
    ("division_error", [], AST.Type.void)
  ]
    ++ map
      (\(n, argTys, retTy) -> (n, map genLLVMType argTys, genLLVMType retTy))
      Core.Builtins.builtinFuns

ptrToFunctionType :: [AST.Type] -> AST.Type -> AST.Type
ptrToFunctionType ts rt =
  AST.Type.ptr (AST.FunctionType rt ts False)

functionPtrOperand :: String -> [AST.Type] -> AST.Type -> AST.Operand
functionPtrOperand n ts rt =
  AST.ConstantOperand $
    AST.Const.GlobalReference
      (ptrToFunctionType ts rt)
      (AST.mkName n)

externOperand :: String -> AST.Operand
externOperand x =
  case find (\(n, _, _) -> n == x) externs of
    Just (n, ts, t) -> functionPtrOperand n ts t
    Nothing -> error "interal error"

malloc, matchError, divisionError :: AST.Operand
malloc = externOperand "malloc"
matchError = externOperand "match_error"
divisionError = externOperand "division_error"

genLLVMType :: C.Type -> AST.Type
genLLVMType t =
  case t of
    C.TInt -> AST.Type.i64
    C.TBool -> AST.Type.i1
    C.TVoid -> AST.Type.i8
    C.TArrow ts t' -> AST.Type.FunctionType (genLLVMType t') (map genLLVMType ts) False
    C.TPointer t' -> AST.Type.ptr $ genLLVMType t'
    C.TStruct ts -> AST.Type.StructureType False $ map genLLVMType ts

codeGenValue :: C.Value -> CodeGen AST.Operand
codeGenValue v =
  case v of
    C.VInt n -> return $ Const.int64 (toInteger n)
    C.VLocalVar x -> do
      table <- reader varTable
      case Map.lookup x table of
        Just o -> return o
        Nothing -> error "internal error"
    C.VGlobalFun f ts t ->
      return $ functionPtrOperand f (map genLLVMType ts) $ genLLVMType t

freshBlockName :: CodeGen AST.Name
freshBlockName = IR.freshName (toShortBS "L")

codeGenExpr :: C.Expr -> CodeGen AST.Operand
codeGenExpr e =
  case e of
    C.EValue v -> codeGenValue v
    C.ELet x e1 e2 -> do
      o <- codeGenExpr e1
      extendVarTable x o $ codeGenExpr e2
    C.EEagerBinop op v1 v2 -> do
      o1 <- codeGenValue v1
      o2 <- codeGenValue v2
      case op of
        C.Add -> Instr.add o1 o2
        C.Sub -> Instr.sub o1 o2
        C.Mul -> Instr.mul o1 o2
        C.Div -> do
          errorBlock <- freshBlockName
          successBlock <- freshBlockName
          zeroCheck <- Instr.icmp AST.IntegerPredicate.EQ o2 (Const.int64 0)
          Instr.condBr zeroCheck errorBlock successBlock
          IR.emitBlockStart errorBlock
          Instr.call divisionError []
          Instr.unreachable
          IR.emitBlockStart successBlock
          Instr.sdiv o1 o2
        C.Lt -> Instr.icmp AST.IntegerPredicate.SLT o1 o2
        C.Le -> Instr.icmp AST.IntegerPredicate.SLE o1 o2
        C.Gt -> Instr.icmp AST.IntegerPredicate.SGT o1 o2
        C.Ge -> Instr.icmp AST.IntegerPredicate.SGE o1 o2
        C.Eq -> Instr.icmp AST.IntegerPredicate.EQ o1 o2
    C.EShortCircBinop op e1 e2 -> do
      rhsBlock <- freshBlockName
      mergeBlock <- freshBlockName
      o1 <- codeGenExpr e1
      b1 <- IR.currentBlock
      case op of
        C.Or -> Instr.condBr o1 mergeBlock rhsBlock
        C.And -> Instr.condBr o1 rhsBlock mergeBlock
      IR.emitBlockStart rhsBlock
      o2 <- codeGenExpr e2
      b2 <- IR.currentBlock
      Instr.br mergeBlock
      IR.emitBlockStart mergeBlock
      Instr.phi [(o1, b1), (o2, b2)]
    C.EApp v vs -> do
      o <- codeGenValue v
      os <- mapM codeGenValue vs
      let args = zip os $ repeat []
      Instr.call o args
    C.ESwitch v clauses -> do
      o <- codeGenValue v
      dftLabel <- reader defaultSwitchLabel
      blocks <- sequence $ replicate (length clauses) freshBlockName
      mergeBlock <- freshBlockName
      let (tags, es) = unzip clauses
      let consts = map (AST.Const.Int 64) tags
      Instr.switch o dftLabel (zip consts blocks)
      clauseOutputs <-
        mapM
          ( \(b, e) -> do
              IR.emitBlockStart b
              o <- codeGenExpr e
              b <- IR.currentBlock
              Instr.br mergeBlock
              return (o, b)
          )
          (zip blocks es)
      IR.emitBlockStart mergeBlock
      Instr.phi clauseOutputs
    C.EMatchSeq e1 e2 -> do
      failBlock <- freshBlockName
      mergeBlock <- freshBlockName
      o1 <- local (\r -> r {defaultSwitchLabel = failBlock}) $ codeGenExpr e1
      b1 <- IR.currentBlock
      Instr.br mergeBlock
      IR.emitBlockStart failBlock
      o2 <- codeGenExpr e2
      b2 <- IR.currentBlock
      Instr.br mergeBlock
      IR.emitBlockStart mergeBlock
      Instr.phi [(o1, b1), (o2, b2)]
    C.EMatchError ->
      Instr.call matchError []
    C.EAllocRecord t -> do
      let t' = genLLVMType t
      raw <- Instr.call malloc [(AST.ConstantOperand $ AST.Const.sizeof t', [])]
      Instr.bitcast raw $ AST.Type.ptr t'
    C.ESeq e1 e2 -> codeGenExpr e1 >> codeGenExpr e2
    C.EStore v1 i v2 -> do
      o1 <- codeGenValue v1
      o2 <- codeGenValue v2
      addr <- Instr.gep o1 [Const.int32 0, Const.int32 i]
      Instr.store addr 1 o2
      return undefined
    C.EFetch v i -> do
      o <- codeGenValue v
      addr <- Instr.gep o [Const.int32 0, Const.int32 i]
      Instr.load addr 1
    C.ECast t v -> do
      o <- codeGenValue v
      Instr.bitcast o $ genLLVMType t
    C.EIf e1 e2 e3 -> do
      thenBlock <- freshBlockName
      elseBlock <- freshBlockName
      mergeBlock <- freshBlockName
      o1 <- codeGenExpr e1
      Instr.condBr o1 thenBlock elseBlock
      IR.emitBlockStart thenBlock
      o2 <- codeGenExpr e2
      b2 <- IR.currentBlock
      Instr.br mergeBlock
      IR.emitBlockStart elseBlock
      o3 <- codeGenExpr e3
      b3 <- IR.currentBlock
      Instr.br mergeBlock
      IR.emitBlockStart mergeBlock
      Instr.phi [(o2, b2), (o3, b3)]

codeGenFunction :: [C.Var] -> C.Expr -> [AST.Operand] -> CodeGen ()
codeGenFunction paramNames body operands = do
  block <- freshBlockName
  IR.emitBlockStart block
  r <- extendVarTableMany (zip paramNames operands) (codeGenExpr body)
  Instr.ret r

moduleGenFunction :: C.TopLevelFun -> ModuleGen ()
moduleGenFunction
  C.TopLevelFun
    { C.name = f,
      C.params = params,
      C.returnType = returnType,
      C.body = body
    } = do
    let f' = AST.mkName f
        (paramNames, paramTypes) = unzip $ map (\(C.Param x t) -> (x, t)) params
        params' = zip (map genLLVMType paramTypes) (repeat Mod.NoParameterName)
        returnType' = genLLVMType returnType
        initEnv =
          CodeGenEnv
            { defaultSwitchLabel = undefined,
              varTable = Map.empty
            }
    void $
      Mod.function
        f'
        params'
        returnType'
        ((\m -> runReaderT m initEnv) . codeGenFunction paramNames body)

moduleGenExterns :: ModuleGen ()
moduleGenExterns = mapM_ (\(n, tys, rt) -> Mod.extern (AST.mkName n) tys rt) externs

moduleGenProgram :: C.Program -> ModuleGen ()
moduleGenProgram (C.Program fs) =
  moduleGenExterns >> mapM_ moduleGenFunction fs

codeGen :: C.Program -> AST.Module
codeGen program = Mod.buildModule (toShortBS "YaflModule") $ moduleGenProgram program
