module Yafl.Core.Pretty where

import Prettyprinter
import Yafl.Core.Syntax

instance Pretty Type where
  pretty TInt = pretty "int"
  pretty TBool = pretty "bool"
  pretty TVoid = pretty "void"
  pretty (TArrow ts rt) =
    parens (hsep (punctuate comma (map pretty ts))) <+> pretty "->" <+> pretty rt
  pretty (TStruct ts) =
    braces (hsep (punctuate comma (map pretty ts)))
  pretty (TPointer t) =
    pretty "*" <> pretty t

instance Pretty Value where
  pretty (VInt n) = pretty "Int" <+> pretty n
  pretty (VLocalVar v) = pretty "LocalVar" <+> pretty v
  pretty (VGlobalFun v ts rt) =
    pretty "GlobalFun" <+> pretty v <+> tupled (map pretty ts) <+> pretty "->" <+> pretty rt

instance Pretty Expr where
  pretty (EValue v) = pretty "Value" <+> pretty v
  pretty (ELet v e1 e2) =
    hang 0 $ pretty "Let" <+> pretty v <+> pretty "=" <+> pretty e1 <+> pretty "in" <+> line <> pretty e2
  pretty (EEagerBinop binop v1 v2) =
    pretty "EagerBinop" <+> pretty binop <+> pretty v1 <+> pretty v2
  pretty (EShortCircBinop binop e1 e2) =
    pretty "ShortCircBinop" <+> pretty binop <+> parens (pretty e1) <+> parens (pretty e2)
  pretty (EApp fun args) =
    pretty "App" <+> pretty fun <+> tupled (map pretty args)
  pretty (ESwitch cond cases) =
    pretty "Switch" <+> pretty cond <> line <> vsep (map casePretty cases)
    where
      casePretty (i, expr) = pretty "case" <+> pretty i <+> pretty "->" <+> pretty expr
  pretty (EMatchSeq e1 e2) =
    pretty "MatchSeq" <+> (line <> pretty "*" <+> pretty e1 <> line <> pretty "*" <+> pretty e2)
  pretty EMatchError = pretty "PatternMatchingError"
  pretty (EAllocRecord ty) = pretty "AllocRecord" <+> pretty ty
  pretty (ESeq e1 e2) =
    pretty "Seq" <+> pretty e1 <> line <> pretty e2
  pretty (EStore v1 n v2) =
    pretty "Store" <+> pretty v1 <+> pretty n <+> pretty v2
  pretty (EFetch v n) = pretty "Fetch" <+> pretty v <+> pretty n
  pretty (ECast ty v) = pretty "Cast" <+> pretty ty <+> pretty v
  pretty (EIf e1 e2 e3) =
    pretty "If"
      <+> pretty e1
      <+> (line <> pretty "then" <+> pretty e2)
      <+> (line <> pretty "else" <+> pretty e3)

instance Pretty Param where
  pretty (Param v ty) = pretty v <+> pretty ":" <+> pretty ty

instance Pretty TopLevelFun where
  pretty TopLevelFun {name = name, params = params, returnType = returnType, body = body} =
    pretty "TopLevelFun"
      <+> pretty name
      <+> tupled (map pretty params)
      <+> pretty ":"
      <+> pretty returnType
      <+> pretty "{"
        <> (nest 2 (line <> pretty body))
      <+> line <> pretty "} "

instance Pretty Program where
  pretty (Program fs) = vsep (map pretty fs)
