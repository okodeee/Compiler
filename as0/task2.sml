(* CSE322 Compiler Assignment 0 - Task 2 *)

structure Task2 :> TASK2 = 
struct
  exception NotImplemented

  datatype 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

  (* sumTree t: return the sum of all values *)
  fun sum (Leaf x) = x
    | sum (Node (t1,x,t2)) = (sum t1) + x + (sum t2)

  (* exist t n: return true if n exists in a tree *)
  fun exist (Leaf x) n = if x = n then true else false
    | exist (Node (t1, x, t2)) n = (exist t1 n) orelse x = n orelse (exist t2 n)

  (* count t n: count n in a tree *)
  fun count (Leaf x) n = if x = n then 1 else 0
    | count (Node (t1, x, t2)) n =
    (count t1 n) + (count t2 n) + (if x = n then 1 else 0)

  (* inorder t: return the list of values using inorder tree traversal *)
  fun inorder (Leaf x) = [x]
    | inorder (Node (t1, x, t2)) = (inorder t1) @ [x] @ (inorder t2)
  
  (* depth t: return the depth of a tree*)
  fun depth (Leaf x) = 0
    | depth (Node (t1, x, t2)) = 1 + Int.max(depth t1, depth t2)

  (* max t: return the maximum value in a tree*)
  fun max (Leaf x) = x
    | max (Node (t1, x, t2)) =
    if max t1 > max t2 andalso max t1 > x then max t1
    else if max t2 > max t1 andalso max t2 > x then max t2
    else x

end
