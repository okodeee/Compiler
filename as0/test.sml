structure Tester = struct

  fun test1 f (x, y) = if (f x) = y then true else false
  fun test2 f (x1, x2, y) = if (f x1 x2) = y then true else false

  fun testFac() = map (test1 Task1.fac) [(1,1), (3,6), (5,120)]
  fun testFib() = map (test1 Task1.fib) [(1,0), (2,1), (10,34)]
  fun testPow() = map (test1 Task1.pow) [((5,0),1), ((2,2),4), ((3,3),27)]
  fun testGCD() = map (test1 Task1.gcd) [((2,3),1), ((24,4),4), ((15,6), 3)]
  fun testPLD() = map (test1 Task1.palindrome) [("aba",true), ("aa", true), ("abcde", false)]

  (* List *)
  val list1 = []
  val list2 = [1,2,3,4,5]
  val list3 = [10,2,0,~3,0,8,0]

  fun testListExist() = map (test2 Task1.exist) [(list1,0,false), (list2,3,true), (list3,0,true)]
  fun testListCount() = map (test2 Task1.count) [(list1,0,0), (list2,4,1), (list3,0,3)]
  fun testListRever() = map (test1 Task1.reverse) [(list1,[]),
    (list2,[5,4,3,2,1]), (list3,[0,8,0,~3,0,2,10])]
  fun testListFindf() = map (test2 Task1.find) [(list1,0,~1), (list2,3,2), (list3,0,2)]
  fun testListFindr() = map (test2 Task1.findr) [(list1,0,~1), (list2,3,2), (list3,0,6)]

  (* Tree *)
  val tree1 = Task2.Leaf 1
  val tree2 = Task2.Node (Task2.Leaf 1, 3, Task2.Leaf 10)
  val tree3 = Task2.Node (Task2.Node (Task2.Leaf 1, 2, Task2.Leaf 10), 3, Task2.Leaf 3)

  fun testTreeExist() = map (test2 Task2.exist) [(tree1, 0, false), (tree2, 3, true), (tree3, 9, false)]
  fun testTreeCount() = map (test2 Task2.count) [(tree1,1,1), (tree2,7,0), (tree3,3,2)]
  fun testTreeInorder() = map (test1 Task2.inorder) [(tree1,[1]), (tree2,[1,3,10]), (tree3,[1,2,10,3,3])]
  fun testTreeDepth() = map (test1 Task2.depth) [(tree1,0), (tree2,1), (tree3,2)]
  fun testTreeMax() = map (test1 Task2.max) [(tree1,1), (tree2,10), (tree3,10)]

  fun runTest (s, f) =
    let
      val result = (print ("Testing "^s^"\n"); f())
        handle Task1.NotImplemented => [false]
             | Task2.NotImplemented => [false]
    in
      if List.all (fn x => x) result then print "  Passed.\n" else print "  Failed.\n"
    end

  fun runAll() = (
    runTest("Task1.fac", testFac);
    runTest("Task1.fib", testFib);
    runTest("Task1.pow", testPow);
    runTest("Task1.gcd", testGCD);
    runTest("Task1.palindrom", testPLD);
    runTest("Task1.exist", testListExist);
    runTest("Task1.count", testListCount);
    runTest("Task1.reverse", testListRever);
    runTest("Task1.find", testListFindf);
    runTest("Task1.findr", testListFindr);
    runTest("Task2.exist", testTreeExist);
    runTest("Task2.count", testTreeCount);
    runTest("Task2.inorder", testTreeInorder);
    runTest("Task2.depth", testTreeDepth);
    runTest("Task2.max", testTreeMax);
    ())

end 
