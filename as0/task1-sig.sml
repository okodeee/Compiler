(* CSE322 Compiler Assignment 0 - Task 1 *)

signature TASK1 =
sig
  exception NotImplemented

  val sum: int -> int
  val fac: int -> int
  val fib: int -> int
  val pow: (int * int) -> int
  val gcd: (int * int) -> int
  val palindrome: string -> bool

  val max: int list -> int
  val exist: ''a list -> ''a -> bool
  val count: ''a list -> ''a -> int
  val reverse: ''a list -> ''a list
  val find: ''a list -> ''a -> int
  val findr: ''a list -> ''a -> int

end
