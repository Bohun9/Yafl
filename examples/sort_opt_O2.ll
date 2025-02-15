; ModuleID = 'examples/sort.ll'
source_filename = "examples/sort.ll"

declare ptr @malloc(i32) local_unnamed_addr

declare void @match_error() local_unnamed_addr

declare i64 @print_int(ptr, i64)

declare i64 @read_int(ptr, i64)

declare i64 @print_newline(ptr, i64)

declare i64 @print_space(ptr, i64)

define ptr @__anon15_8(ptr nocapture readonly %0, i64 %1) {
L_0:
  %2 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 1
  %4 = tail call ptr @malloc(i32 8)
  store ptr %3, ptr %4, align 1
  %5 = getelementptr { ptr, ptr, ptr }, ptr %3, i64 0, i32 1
  %6 = load ptr, ptr %5, align 1
  %7 = load i64, ptr %6, align 1
  switch i64 %7, label %L_1 [
    i64 0, label %L_3
    i64 1, label %L_4
  ]

L_3:                                              ; preds = %L_0
  %8 = tail call ptr @malloc(i32 8)
  store i64 0, ptr %8, align 1
  %9 = tail call ptr @malloc(i32 8)
  store i64 0, ptr %9, align 1
  %10 = tail call ptr @malloc(i32 32)
  store i64 1, ptr %10, align 1
  %11 = getelementptr { i64, ptr, i64, ptr }, ptr %10, i64 0, i32 1
  store ptr %8, ptr %11, align 1
  %12 = getelementptr { i64, ptr, i64, ptr }, ptr %10, i64 0, i32 2
  store i64 %1, ptr %12, align 1
  %13 = getelementptr { i64, ptr, i64, ptr }, ptr %10, i64 0, i32 3
  store ptr %9, ptr %13, align 1
  br label %L_2

L_4:                                              ; preds = %L_0
  %14 = getelementptr { i64, ptr, i64, ptr }, ptr %6, i64 0, i32 1
  %15 = load ptr, ptr %14, align 1
  %16 = getelementptr { i64, ptr, i64, ptr }, ptr %6, i64 0, i32 2
  %17 = load i64, ptr %16, align 1
  %18 = getelementptr { i64, ptr, i64, ptr }, ptr %6, i64 0, i32 3
  %19 = load ptr, ptr %18, align 1
  %20 = icmp sgt i64 %17, %1
  %21 = getelementptr { ptr, ptr, ptr }, ptr %3, i64 0, i32 2
  %22 = load ptr, ptr %21, align 1
  %23 = load ptr, ptr %22, align 1
  br i1 %20, label %L_6, label %L_7

L_6:                                              ; preds = %L_4
  %24 = tail call ptr %23(ptr nonnull %22, ptr %15)
  %25 = load ptr, ptr %24, align 1
  %26 = tail call ptr %25(ptr nonnull %24, i64 %1)
  %27 = tail call ptr @malloc(i32 32)
  store i64 1, ptr %27, align 1
  %28 = getelementptr { i64, ptr, i64, ptr }, ptr %27, i64 0, i32 1
  store ptr %26, ptr %28, align 1
  %29 = getelementptr { i64, ptr, i64, ptr }, ptr %27, i64 0, i32 2
  store i64 %17, ptr %29, align 1
  %30 = getelementptr { i64, ptr, i64, ptr }, ptr %27, i64 0, i32 3
  store ptr %19, ptr %30, align 1
  br label %L_2

L_7:                                              ; preds = %L_4
  %31 = tail call ptr %23(ptr nonnull %22, ptr %19)
  %32 = load ptr, ptr %31, align 1
  %33 = tail call ptr %32(ptr nonnull %31, i64 %1)
  %34 = tail call ptr @malloc(i32 32)
  store i64 1, ptr %34, align 1
  %35 = getelementptr { i64, ptr, i64, ptr }, ptr %34, i64 0, i32 1
  store ptr %15, ptr %35, align 1
  %36 = getelementptr { i64, ptr, i64, ptr }, ptr %34, i64 0, i32 2
  store i64 %17, ptr %36, align 1
  %37 = getelementptr { i64, ptr, i64, ptr }, ptr %34, i64 0, i32 3
  store ptr %33, ptr %37, align 1
  br label %L_2

L_1:                                              ; preds = %L_0
  tail call void @match_error()
  br label %L_2

L_2:                                              ; preds = %L_3, %L_7, %L_6, %L_1
  %38 = phi ptr [ undef, %L_1 ], [ %10, %L_3 ], [ %27, %L_6 ], [ %34, %L_7 ]
  ret ptr %38
}

define noundef ptr @_tree_insert_10(ptr %0, ptr %1) {
L_0:
  %2 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 1
  %4 = tail call ptr @malloc(i32 24)
  store ptr %3, ptr %4, align 1
  %5 = getelementptr { ptr, ptr, ptr }, ptr %4, i64 0, i32 2
  store ptr %0, ptr %5, align 1
  %6 = getelementptr { ptr, ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr %1, ptr %6, align 1
  %7 = tail call ptr @malloc(i32 16)
  store ptr @__anon15_8, ptr %7, align 1
  %8 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr %4, ptr %8, align 1
  ret ptr %7
}

define ptr @__anon14_16(ptr nocapture readonly %0, ptr %1) {
L_0:
  %2 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 1
  %4 = tail call ptr @malloc(i32 8)
  store ptr %3, ptr %4, align 1
  %5 = getelementptr { ptr, ptr, ptr }, ptr %3, i64 0, i32 1
  %6 = load ptr, ptr %5, align 1
  %7 = load i64, ptr %6, align 1
  switch i64 %7, label %L_1 [
    i64 0, label %L_2
    i64 1, label %L_4
  ]

L_4:                                              ; preds = %L_0
  %8 = getelementptr { i64, ptr, i64, ptr }, ptr %6, i64 0, i32 1
  %9 = load ptr, ptr %8, align 1
  %10 = getelementptr { i64, ptr, i64, ptr }, ptr %6, i64 0, i32 2
  %11 = load i64, ptr %10, align 1
  %12 = getelementptr { i64, ptr, i64, ptr }, ptr %6, i64 0, i32 3
  %13 = load ptr, ptr %12, align 1
  %14 = getelementptr { ptr, ptr, ptr }, ptr %3, i64 0, i32 2
  %15 = load ptr, ptr %14, align 1
  %16 = load ptr, ptr %15, align 1
  %17 = tail call ptr %16(ptr nonnull %15, ptr %13)
  %18 = load ptr, ptr %17, align 1
  %19 = tail call ptr %18(ptr nonnull %17, ptr %1)
  %20 = load ptr, ptr %14, align 1
  %21 = load ptr, ptr %20, align 1
  %22 = tail call ptr %21(ptr nonnull %20, ptr %9)
  %23 = tail call ptr @malloc(i32 24)
  store i64 1, ptr %23, align 1
  %24 = getelementptr { i64, i64, ptr }, ptr %23, i64 0, i32 1
  store i64 %11, ptr %24, align 1
  %25 = getelementptr { i64, i64, ptr }, ptr %23, i64 0, i32 2
  store ptr %19, ptr %25, align 1
  %26 = load ptr, ptr %22, align 1
  %27 = tail call ptr %26(ptr nonnull %22, ptr nonnull %23)
  br label %L_2

L_1:                                              ; preds = %L_0
  tail call void @match_error()
  br label %L_2

L_2:                                              ; preds = %L_0, %L_4, %L_1
  %28 = phi ptr [ undef, %L_1 ], [ %27, %L_4 ], [ %1, %L_0 ]
  ret ptr %28
}

define noundef ptr @_tree_to_list_fast_18(ptr %0, ptr %1) {
L_0:
  %2 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 1
  %4 = tail call ptr @malloc(i32 24)
  store ptr %3, ptr %4, align 1
  %5 = getelementptr { ptr, ptr, ptr }, ptr %4, i64 0, i32 2
  store ptr %0, ptr %5, align 1
  %6 = getelementptr { ptr, ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr %1, ptr %6, align 1
  %7 = tail call ptr @malloc(i32 16)
  store ptr @__anon14_16, ptr %7, align 1
  %8 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr %4, ptr %8, align 1
  ret ptr %7
}

define ptr @_tree_to_list_22(ptr nocapture readonly %0, ptr %1) {
L_0:
  %2 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 1
  %4 = tail call ptr @malloc(i32 8)
  store ptr %3, ptr %4, align 1
  %5 = getelementptr { ptr, ptr, ptr }, ptr %3, i64 0, i32 1
  %6 = load ptr, ptr %5, align 1
  %7 = load ptr, ptr %6, align 1
  %8 = tail call ptr %7(ptr nonnull %6, ptr %1)
  %9 = tail call ptr @malloc(i32 8)
  store i64 0, ptr %9, align 1
  %10 = load ptr, ptr %8, align 1
  %11 = tail call ptr %10(ptr nonnull %8, ptr nonnull %9)
  ret ptr %11
}

define ptr @_list_to_tree_26(ptr %0, ptr nocapture readonly %1) {
L_0:
  %2 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 1
  %4 = tail call ptr @malloc(i32 8)
  store ptr %3, ptr %4, align 1
  %5 = load i64, ptr %1, align 1
  switch i64 %5, label %L_1 [
    i64 0, label %L_3
    i64 1, label %L_4
  ]

L_3:                                              ; preds = %L_0
  %6 = tail call ptr @malloc(i32 8)
  store i64 0, ptr %6, align 1
  br label %L_2

L_4:                                              ; preds = %L_0
  %7 = getelementptr { i64, i64, ptr }, ptr %1, i64 0, i32 1
  %8 = load i64, ptr %7, align 1
  %9 = getelementptr { i64, i64, ptr }, ptr %1, i64 0, i32 2
  %10 = load ptr, ptr %9, align 1
  %11 = load ptr, ptr %0, align 1
  %12 = tail call ptr %11(ptr nonnull %0, ptr %10)
  %13 = getelementptr { ptr, ptr, ptr }, ptr %3, i64 0, i32 2
  %14 = load ptr, ptr %13, align 1
  %15 = load ptr, ptr %14, align 1
  %16 = tail call ptr %15(ptr nonnull %14, ptr %12)
  %17 = load ptr, ptr %16, align 1
  %18 = tail call ptr %17(ptr nonnull %16, i64 %8)
  br label %L_2

L_1:                                              ; preds = %L_0
  tail call void @match_error()
  br label %L_2

L_2:                                              ; preds = %L_3, %L_4, %L_1
  %19 = phi ptr [ undef, %L_1 ], [ %6, %L_3 ], [ %18, %L_4 ]
  ret ptr %19
}

define noundef ptr @_read_list_32(ptr %0, i64 %1) {
L_0:
  %2 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 1
  %4 = tail call ptr @malloc(i32 8)
  store ptr %3, ptr %4, align 1
  %5 = icmp eq i64 %1, 0
  br i1 %5, label %L_1, label %L_2

L_1:                                              ; preds = %L_0
  %6 = tail call ptr @malloc(i32 8)
  store i64 0, ptr %6, align 1
  br label %L_3

L_2:                                              ; preds = %L_0
  %7 = tail call ptr @malloc(i32 16)
  store ptr @read_int, ptr %7, align 1
  %8 = tail call i64 @read_int(ptr nonnull %7, i64 0)
  %9 = add i64 %1, -1
  %10 = load ptr, ptr %0, align 1
  %11 = tail call ptr %10(ptr nonnull %0, i64 %9)
  %12 = tail call ptr @malloc(i32 24)
  store i64 1, ptr %12, align 1
  %13 = getelementptr { i64, i64, ptr }, ptr %12, i64 0, i32 1
  store i64 %8, ptr %13, align 1
  %14 = getelementptr { i64, i64, ptr }, ptr %12, i64 0, i32 2
  store ptr %11, ptr %14, align 1
  br label %L_3

L_3:                                              ; preds = %L_2, %L_1
  %15 = phi ptr [ %6, %L_1 ], [ %12, %L_2 ]
  ret ptr %15
}

define i64 @_print_list_37(ptr %0, ptr nocapture readonly %1) {
L_0:
  %2 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 1
  %4 = tail call ptr @malloc(i32 8)
  store ptr %3, ptr %4, align 1
  %5 = load i64, ptr %1, align 1
  switch i64 %5, label %L_1 [
    i64 0, label %L_3
    i64 1, label %L_4
  ]

L_3:                                              ; preds = %L_0
  %6 = tail call ptr @malloc(i32 16)
  store ptr @print_newline, ptr %6, align 1
  %7 = tail call i64 @print_newline(ptr nonnull %6, i64 0)
  br label %L_2

L_4:                                              ; preds = %L_0
  %8 = getelementptr { i64, i64, ptr }, ptr %1, i64 0, i32 1
  %9 = load i64, ptr %8, align 1
  %10 = getelementptr { i64, i64, ptr }, ptr %1, i64 0, i32 2
  %11 = load ptr, ptr %10, align 1
  %12 = tail call ptr @malloc(i32 16)
  store ptr @print_int, ptr %12, align 1
  %13 = tail call i64 @print_int(ptr nonnull %12, i64 %9)
  %14 = tail call ptr @malloc(i32 16)
  store ptr @print_space, ptr %14, align 1
  %15 = tail call i64 @print_space(ptr nonnull %14, i64 0)
  %16 = load ptr, ptr %0, align 1
  %17 = tail call i64 %16(ptr nonnull %0, ptr %11)
  br label %L_2

L_1:                                              ; preds = %L_0
  tail call void @match_error()
  br label %L_2

L_2:                                              ; preds = %L_3, %L_4, %L_1
  %18 = phi i64 [ undef, %L_1 ], [ %7, %L_3 ], [ %17, %L_4 ]
  ret i64 %18
}

define i64 @__yafl_toplevel(ptr nocapture readonly %0, i64 %1) local_unnamed_addr {
L_0:
  %2 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 1
  %4 = tail call ptr @malloc(i32 24)
  store ptr %3, ptr %4, align 1
  %5 = tail call ptr @malloc(i32 16)
  store ptr @_tree_insert_10, ptr %5, align 1
  %6 = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr %4, ptr %6, align 1
  %7 = getelementptr { ptr, ptr, ptr }, ptr %4, i64 0, i32 2
  store ptr %5, ptr %7, align 1
  %8 = tail call ptr @malloc(i32 16)
  store ptr @_tree_to_list_fast_18, ptr %8, align 1
  %9 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr %4, ptr %9, align 1
  %10 = getelementptr { ptr, ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr %8, ptr %10, align 1
  %11 = tail call ptr @malloc(i32 16)
  store ptr @_tree_to_list_22, ptr %11, align 1
  %12 = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr %4, ptr %12, align 1
  %13 = tail call ptr @malloc(i32 16)
  store ptr @_list_to_tree_26, ptr %13, align 1
  %14 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr %4, ptr %14, align 1
  %15 = tail call ptr @malloc(i32 16)
  store ptr @read_int, ptr %15, align 1
  %16 = tail call i64 @read_int(ptr nonnull %15, i64 0)
  %17 = tail call ptr @malloc(i32 16)
  store ptr @_read_list_32, ptr %17, align 1
  %18 = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr %4, ptr %18, align 1
  %19 = tail call ptr @malloc(i32 8)
  store ptr %4, ptr %19, align 1
  %20 = icmp eq i64 %16, 0
  br i1 %20, label %L_1.i, label %L_2.i

L_1.i:                                            ; preds = %L_0
  %21 = tail call ptr @malloc(i32 8)
  store i64 0, ptr %21, align 1
  br label %_read_list_32.exit

L_2.i:                                            ; preds = %L_0
  %22 = tail call ptr @malloc(i32 16)
  store ptr @read_int, ptr %22, align 1
  %23 = tail call i64 @read_int(ptr nonnull %22, i64 0)
  %24 = add i64 %16, -1
  %25 = load ptr, ptr %17, align 1
  %26 = tail call ptr %25(ptr nonnull %17, i64 %24)
  %27 = tail call ptr @malloc(i32 24)
  store i64 1, ptr %27, align 1
  %28 = getelementptr { i64, i64, ptr }, ptr %27, i64 0, i32 1
  store i64 %23, ptr %28, align 1
  %29 = getelementptr { i64, i64, ptr }, ptr %27, i64 0, i32 2
  store ptr %26, ptr %29, align 1
  br label %_read_list_32.exit

_read_list_32.exit:                               ; preds = %L_1.i, %L_2.i
  %30 = phi ptr [ %21, %L_1.i ], [ %27, %L_2.i ]
  %31 = tail call ptr @malloc(i32 16)
  store ptr @_print_list_37, ptr %31, align 1
  %32 = getelementptr { ptr, ptr }, ptr %31, i64 0, i32 1
  store ptr %4, ptr %32, align 1
  %33 = load ptr, ptr %13, align 1
  %34 = tail call ptr %33(ptr nonnull %13, ptr nonnull %30)
  %35 = load ptr, ptr %11, align 1
  %36 = tail call ptr %35(ptr nonnull %11, ptr %34)
  %37 = load ptr, ptr %31, align 1
  %38 = tail call i64 %37(ptr nonnull %31, ptr %36)
  ret i64 %38
}
