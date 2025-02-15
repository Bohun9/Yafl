; ModuleID = 'examples/sort.ll'
source_filename = "examples/sort.ll"

declare ptr @malloc(i32) local_unnamed_addr

declare void @match_error() local_unnamed_addr

declare i64 @print_int(ptr, i64)

declare i64 @read_int(ptr, i64)

declare i64 @print_newline(ptr, i64)

declare i64 @print_space(ptr, i64)

define ptr @__anon15_8(ptr nocapture readonly %0, i64 %1) {
  %3 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %4 = load ptr, ptr %3, align 1
  %5 = tail call ptr @malloc(i32 8)
  store ptr %4, ptr %5, align 1
  %6 = getelementptr { ptr, ptr, ptr }, ptr %4, i64 0, i32 1
  %7 = load ptr, ptr %6, align 1
  %8 = load i64, ptr %7, align 1
  switch i64 %8, label %L_0 [
    i64 0, label %L_2
    i64 1, label %L_3
  ]

L_2:                                              ; preds = %2
  %9 = tail call ptr @malloc(i32 8)
  store i64 0, ptr %9, align 1
  %10 = tail call ptr @malloc(i32 8)
  store i64 0, ptr %10, align 1
  %11 = tail call ptr @malloc(i32 32)
  store i64 1, ptr %11, align 1
  %12 = getelementptr { i64, ptr, i64, ptr }, ptr %11, i64 0, i32 1
  store ptr %9, ptr %12, align 1
  %13 = getelementptr { i64, ptr, i64, ptr }, ptr %11, i64 0, i32 2
  store i64 %1, ptr %13, align 1
  %14 = getelementptr { i64, ptr, i64, ptr }, ptr %11, i64 0, i32 3
  store ptr %10, ptr %14, align 1
  br label %L_1

L_3:                                              ; preds = %2
  %15 = getelementptr { i64, ptr, i64, ptr }, ptr %7, i64 0, i32 1
  %16 = load ptr, ptr %15, align 1
  %17 = getelementptr { i64, ptr, i64, ptr }, ptr %7, i64 0, i32 2
  %18 = load i64, ptr %17, align 1
  %19 = getelementptr { i64, ptr, i64, ptr }, ptr %7, i64 0, i32 3
  %20 = load ptr, ptr %19, align 1
  %21 = icmp sgt i64 %18, %1
  %22 = getelementptr { ptr, ptr, ptr }, ptr %4, i64 0, i32 2
  %23 = load ptr, ptr %22, align 1
  %24 = load ptr, ptr %23, align 1
  br i1 %21, label %L_5, label %L_6

L_5:                                              ; preds = %L_3
  %25 = tail call ptr %24(ptr nonnull %23, ptr %16)
  %26 = load ptr, ptr %25, align 1
  %27 = tail call ptr %26(ptr nonnull %25, i64 %1)
  %28 = tail call ptr @malloc(i32 32)
  store i64 1, ptr %28, align 1
  %29 = getelementptr { i64, ptr, i64, ptr }, ptr %28, i64 0, i32 1
  store ptr %27, ptr %29, align 1
  %30 = getelementptr { i64, ptr, i64, ptr }, ptr %28, i64 0, i32 2
  store i64 %18, ptr %30, align 1
  %31 = getelementptr { i64, ptr, i64, ptr }, ptr %28, i64 0, i32 3
  store ptr %20, ptr %31, align 1
  br label %L_1

L_6:                                              ; preds = %L_3
  %32 = tail call ptr %24(ptr nonnull %23, ptr %20)
  %33 = load ptr, ptr %32, align 1
  %34 = tail call ptr %33(ptr nonnull %32, i64 %1)
  %35 = tail call ptr @malloc(i32 32)
  store i64 1, ptr %35, align 1
  %36 = getelementptr { i64, ptr, i64, ptr }, ptr %35, i64 0, i32 1
  store ptr %16, ptr %36, align 1
  %37 = getelementptr { i64, ptr, i64, ptr }, ptr %35, i64 0, i32 2
  store i64 %18, ptr %37, align 1
  %38 = getelementptr { i64, ptr, i64, ptr }, ptr %35, i64 0, i32 3
  store ptr %34, ptr %38, align 1
  br label %L_1

L_0:                                              ; preds = %2
  tail call void @match_error()
  br label %L_1

L_1:                                              ; preds = %L_2, %L_6, %L_5, %L_0
  %39 = phi ptr [ undef, %L_0 ], [ %11, %L_2 ], [ %28, %L_5 ], [ %35, %L_6 ]
  ret ptr %39
}

define noundef ptr @_tree_insert_10(ptr %0, ptr %1) {
  %3 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %4 = load ptr, ptr %3, align 1
  %5 = tail call ptr @malloc(i32 24)
  store ptr %4, ptr %5, align 1
  %6 = getelementptr { ptr, ptr, ptr }, ptr %5, i64 0, i32 2
  store ptr %0, ptr %6, align 1
  %7 = getelementptr { ptr, ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr %1, ptr %7, align 1
  %8 = tail call ptr @malloc(i32 16)
  store ptr @__anon15_8, ptr %8, align 1
  %9 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr %5, ptr %9, align 1
  ret ptr %8
}

define ptr @__anon14_16(ptr nocapture readonly %0, ptr %1) {
  %3 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %4 = load ptr, ptr %3, align 1
  %5 = tail call ptr @malloc(i32 8)
  store ptr %4, ptr %5, align 1
  %6 = getelementptr { ptr, ptr, ptr }, ptr %4, i64 0, i32 1
  %7 = load ptr, ptr %6, align 1
  %8 = load i64, ptr %7, align 1
  switch i64 %8, label %L_0 [
    i64 0, label %L_1
    i64 1, label %L_3
  ]

L_3:                                              ; preds = %2
  %9 = getelementptr { i64, ptr, i64, ptr }, ptr %7, i64 0, i32 1
  %10 = load ptr, ptr %9, align 1
  %11 = getelementptr { i64, ptr, i64, ptr }, ptr %7, i64 0, i32 2
  %12 = load i64, ptr %11, align 1
  %13 = getelementptr { i64, ptr, i64, ptr }, ptr %7, i64 0, i32 3
  %14 = load ptr, ptr %13, align 1
  %15 = getelementptr { ptr, ptr, ptr }, ptr %4, i64 0, i32 2
  %16 = load ptr, ptr %15, align 1
  %17 = load ptr, ptr %16, align 1
  %18 = tail call ptr %17(ptr nonnull %16, ptr %14)
  %19 = load ptr, ptr %18, align 1
  %20 = tail call ptr %19(ptr nonnull %18, ptr %1)
  %21 = load ptr, ptr %15, align 1
  %22 = load ptr, ptr %21, align 1
  %23 = tail call ptr %22(ptr nonnull %21, ptr %10)
  %24 = tail call ptr @malloc(i32 24)
  store i64 1, ptr %24, align 1
  %25 = getelementptr { i64, i64, ptr }, ptr %24, i64 0, i32 1
  store i64 %12, ptr %25, align 1
  %26 = getelementptr { i64, i64, ptr }, ptr %24, i64 0, i32 2
  store ptr %20, ptr %26, align 1
  %27 = load ptr, ptr %23, align 1
  %28 = tail call ptr %27(ptr nonnull %23, ptr nonnull %24)
  br label %L_1

L_0:                                              ; preds = %2
  tail call void @match_error()
  br label %L_1

L_1:                                              ; preds = %2, %L_3, %L_0
  %29 = phi ptr [ undef, %L_0 ], [ %28, %L_3 ], [ %1, %2 ]
  ret ptr %29
}

define noundef ptr @_tree_to_list_fast_18(ptr %0, ptr %1) {
  %3 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %4 = load ptr, ptr %3, align 1
  %5 = tail call ptr @malloc(i32 24)
  store ptr %4, ptr %5, align 1
  %6 = getelementptr { ptr, ptr, ptr }, ptr %5, i64 0, i32 2
  store ptr %0, ptr %6, align 1
  %7 = getelementptr { ptr, ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr %1, ptr %7, align 1
  %8 = tail call ptr @malloc(i32 16)
  store ptr @__anon14_16, ptr %8, align 1
  %9 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr %5, ptr %9, align 1
  ret ptr %8
}

define ptr @_tree_to_list_22(ptr nocapture readonly %0, ptr %1) {
  %3 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %4 = load ptr, ptr %3, align 1
  %5 = tail call ptr @malloc(i32 8)
  store ptr %4, ptr %5, align 1
  %6 = getelementptr { ptr, ptr, ptr }, ptr %4, i64 0, i32 1
  %7 = load ptr, ptr %6, align 1
  %8 = load ptr, ptr %7, align 1
  %9 = tail call ptr %8(ptr nonnull %7, ptr %1)
  %10 = tail call ptr @malloc(i32 8)
  store i64 0, ptr %10, align 1
  %11 = load ptr, ptr %9, align 1
  %12 = tail call ptr %11(ptr nonnull %9, ptr nonnull %10)
  ret ptr %12
}

define ptr @_list_to_tree_26(ptr %0, ptr nocapture readonly %1) {
  %3 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %4 = load ptr, ptr %3, align 1
  %5 = tail call ptr @malloc(i32 8)
  store ptr %4, ptr %5, align 1
  %6 = load i64, ptr %1, align 1
  switch i64 %6, label %L_0 [
    i64 0, label %L_2
    i64 1, label %L_3
  ]

L_2:                                              ; preds = %2
  %7 = tail call ptr @malloc(i32 8)
  store i64 0, ptr %7, align 1
  br label %L_1

L_3:                                              ; preds = %2
  %8 = getelementptr { i64, i64, ptr }, ptr %1, i64 0, i32 1
  %9 = load i64, ptr %8, align 1
  %10 = getelementptr { i64, i64, ptr }, ptr %1, i64 0, i32 2
  %11 = load ptr, ptr %10, align 1
  %12 = load ptr, ptr %0, align 1
  %13 = tail call ptr %12(ptr nonnull %0, ptr %11)
  %14 = getelementptr { ptr, ptr, ptr }, ptr %4, i64 0, i32 2
  %15 = load ptr, ptr %14, align 1
  %16 = load ptr, ptr %15, align 1
  %17 = tail call ptr %16(ptr nonnull %15, ptr %13)
  %18 = load ptr, ptr %17, align 1
  %19 = tail call ptr %18(ptr nonnull %17, i64 %9)
  br label %L_1

L_0:                                              ; preds = %2
  tail call void @match_error()
  br label %L_1

L_1:                                              ; preds = %L_2, %L_3, %L_0
  %20 = phi ptr [ undef, %L_0 ], [ %7, %L_2 ], [ %19, %L_3 ]
  ret ptr %20
}

define noundef ptr @_read_list_32(ptr %0, i64 %1) {
  %3 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %4 = load ptr, ptr %3, align 1
  %5 = tail call ptr @malloc(i32 8)
  store ptr %4, ptr %5, align 1
  %6 = icmp eq i64 %1, 0
  br i1 %6, label %L_0, label %L_1

L_0:                                              ; preds = %2
  %7 = tail call ptr @malloc(i32 8)
  store i64 0, ptr %7, align 1
  br label %L_2

L_1:                                              ; preds = %2
  %8 = tail call ptr @malloc(i32 16)
  store ptr @read_int, ptr %8, align 1
  %9 = tail call i64 @read_int(ptr nonnull %8, i64 0)
  %10 = add i64 %1, -1
  %11 = load ptr, ptr %0, align 1
  %12 = tail call ptr %11(ptr nonnull %0, i64 %10)
  %13 = tail call ptr @malloc(i32 24)
  store i64 1, ptr %13, align 1
  %14 = getelementptr { i64, i64, ptr }, ptr %13, i64 0, i32 1
  store i64 %9, ptr %14, align 1
  %15 = getelementptr { i64, i64, ptr }, ptr %13, i64 0, i32 2
  store ptr %12, ptr %15, align 1
  br label %L_2

L_2:                                              ; preds = %L_1, %L_0
  %16 = phi ptr [ %7, %L_0 ], [ %13, %L_1 ]
  ret ptr %16
}

define i64 @_print_list_37(ptr %0, ptr nocapture readonly %1) {
  %3 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %4 = load ptr, ptr %3, align 1
  %5 = tail call ptr @malloc(i32 8)
  store ptr %4, ptr %5, align 1
  %6 = load i64, ptr %1, align 1
  switch i64 %6, label %L_0 [
    i64 0, label %L_2
    i64 1, label %L_3
  ]

L_2:                                              ; preds = %2
  %7 = tail call ptr @malloc(i32 16)
  store ptr @print_newline, ptr %7, align 1
  %8 = tail call i64 @print_newline(ptr nonnull %7, i64 0)
  br label %L_1

L_3:                                              ; preds = %2
  %9 = getelementptr { i64, i64, ptr }, ptr %1, i64 0, i32 1
  %10 = load i64, ptr %9, align 1
  %11 = getelementptr { i64, i64, ptr }, ptr %1, i64 0, i32 2
  %12 = load ptr, ptr %11, align 1
  %13 = tail call ptr @malloc(i32 16)
  store ptr @print_int, ptr %13, align 1
  %14 = tail call i64 @print_int(ptr nonnull %13, i64 %10)
  %15 = tail call ptr @malloc(i32 16)
  store ptr @print_space, ptr %15, align 1
  %16 = tail call i64 @print_space(ptr nonnull %15, i64 0)
  %17 = load ptr, ptr %0, align 1
  %18 = tail call i64 %17(ptr nonnull %0, ptr %12)
  br label %L_1

L_0:                                              ; preds = %2
  tail call void @match_error()
  br label %L_1

L_1:                                              ; preds = %L_2, %L_3, %L_0
  %19 = phi i64 [ undef, %L_0 ], [ %8, %L_2 ], [ %18, %L_3 ]
  ret i64 %19
}

define i64 @__yafl_toplevel(ptr nocapture readonly %0, i64 %1) local_unnamed_addr {
  %3 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %4 = load ptr, ptr %3, align 1
  %5 = tail call ptr @malloc(i32 24)
  store ptr %4, ptr %5, align 1
  %6 = tail call ptr @malloc(i32 16)
  store ptr @_tree_insert_10, ptr %6, align 1
  %7 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr %5, ptr %7, align 1
  %8 = getelementptr { ptr, ptr, ptr }, ptr %5, i64 0, i32 2
  store ptr %6, ptr %8, align 1
  %9 = tail call ptr @malloc(i32 16)
  store ptr @_tree_to_list_fast_18, ptr %9, align 1
  %10 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr %5, ptr %10, align 1
  %11 = getelementptr { ptr, ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr %9, ptr %11, align 1
  %12 = tail call ptr @malloc(i32 16)
  store ptr @_tree_to_list_22, ptr %12, align 1
  %13 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr %5, ptr %13, align 1
  %14 = tail call ptr @malloc(i32 16)
  store ptr @_list_to_tree_26, ptr %14, align 1
  %15 = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr %5, ptr %15, align 1
  %16 = tail call ptr @malloc(i32 16)
  store ptr @read_int, ptr %16, align 1
  %17 = tail call i64 @read_int(ptr nonnull %16, i64 0)
  %18 = tail call ptr @malloc(i32 16)
  store ptr @_read_list_32, ptr %18, align 1
  %19 = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr %5, ptr %19, align 1
  %20 = tail call ptr @malloc(i32 8)
  store ptr %5, ptr %20, align 1
  %21 = icmp eq i64 %17, 0
  br i1 %21, label %L_0.i, label %L_1.i

L_0.i:                                            ; preds = %2
  %22 = tail call ptr @malloc(i32 8)
  store i64 0, ptr %22, align 1
  br label %_read_list_32.exit

L_1.i:                                            ; preds = %2
  %23 = tail call ptr @malloc(i32 16)
  store ptr @read_int, ptr %23, align 1
  %24 = tail call i64 @read_int(ptr nonnull %23, i64 0)
  %25 = add i64 %17, -1
  %26 = load ptr, ptr %18, align 1
  %27 = tail call ptr %26(ptr nonnull %18, i64 %25)
  %28 = tail call ptr @malloc(i32 24)
  store i64 1, ptr %28, align 1
  %29 = getelementptr { i64, i64, ptr }, ptr %28, i64 0, i32 1
  store i64 %24, ptr %29, align 1
  %30 = getelementptr { i64, i64, ptr }, ptr %28, i64 0, i32 2
  store ptr %27, ptr %30, align 1
  br label %_read_list_32.exit

_read_list_32.exit:                               ; preds = %L_0.i, %L_1.i
  %31 = phi ptr [ %22, %L_0.i ], [ %28, %L_1.i ]
  %32 = tail call ptr @malloc(i32 16)
  store ptr @_print_list_37, ptr %32, align 1
  %33 = getelementptr { ptr, ptr }, ptr %32, i64 0, i32 1
  store ptr %5, ptr %33, align 1
  %34 = load ptr, ptr %14, align 1
  %35 = tail call ptr %34(ptr nonnull %14, ptr nonnull %31)
  %36 = load ptr, ptr %12, align 1
  %37 = tail call ptr %36(ptr nonnull %12, ptr %35)
  %38 = load ptr, ptr %32, align 1
  %39 = tail call i64 %38(ptr nonnull %32, ptr %37)
  ret i64 %39
}
