let rec fib (n : int) : int = if n <= 1 then n else fib (n - 1) + fib (n - 2)
let _ = print_int (fib 40)
