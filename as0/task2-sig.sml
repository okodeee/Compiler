(* CSE322 Compiler Assignment 0 - Task 2 *)

signature TASK2 =
sig
  exception NotImplemented

  datatype 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

  val sum: int tree -> int
  val exist: int tree -> int -> bool
  val count: int tree -> int -> int
  val inorder: int tree -> int list
  val depth: int tree -> int
  val max: int tree -> int

end
