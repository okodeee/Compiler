(* CSE322 Compiler Assignment 0 - Task 1 *)

structure Task1 :> TASK1 =
struct
  exception NotImplemented

  (* 1. Basic Recursion *)

  (* sum n: calculate 1 + 2 + ... + n *)
  fun sum 1 = 1
    | sum n = n + sum (n-1)
 
  (* fac n: calculate 1 * 2 * ... * n *)
  fun fac 1 = 1
    | fac n = n * fac (n-1)

  (* fib n: return the n-th fibonacci number *)
  fun fib 0 = 0
    | fib 1 = 1
    | fib n = fib (n-1) + fib (n-2)

  (* pow (x, y): calculate x to the power of y *)
  fun pow (x, 0) = 1
    | pow (x, y) = pow(x, y-1) * x

  (* gcd (x, y): find the great common divisor of x and y *)
  fun gcd (x, y) = if x = y then x
                   else if x > y then gcd (x-y, y)
                   else gcd (x, y-x)

  (* palindrome s: return true if s is a palindrome *)
  fun palindrome s = 
  let
    val reverse = implode (foldl op:: [] (explode s))
  in
    if s = reverse then
      true
    else
      false
  end

  (* 2. List *)

  (* max l: return the maximum value in l *)
  fun max [] = 0
    | max [x] = x
    | max (x::xs) = if x > max xs then x else max xs

  (* exist l x: check if x exists in l *)
  fun exist [] x = false
    | exist [y] x = if y = x then true else false
    | exist (y::ys) x = if y = x then true else exist ys x

  (* count l x: count the number of x in l *)
  fun count [] x = 0
    | count l x =
    foldl (fn (value, count) => if value = x then count+1 else count) 0 l

  (* reverse l: return the reversed l *)
  fun reverse [] = []
    | reverse l = foldl (fn (value, result) => [value] @ result) [] l

  (* find l x: return the index of the first x in l
  *                   ~1 if x does not exist in l *)
  fun find l item =
  let
    fun find' [] m = ~1
      | find' (x::xs) m = if x = item then m else find' xs m+1
  in
    find' l 0
  end

  (* findr l x: return the index of the last x in l
  *                    ~1 if x does not exist in l *)
  fun findr l item = 
  let
    fun findr' [] (current, last) = last
      | findr' (x::xs) (current, last) =
      if x = item then findr' xs (current+1, current)
      else findr' xs (current+1, last)
  in findr' l (0, ~1)
  end
  

end
