open Proc.Ds
open Test_lib

(* Tree expressions for complex test cases *)

let caseT_testcase1 = "
  let t = emptytree
  in
  caseT t of {
    emptytree -> zero?(0),
    node(a,l,r) -> zero?(1)
  }
"

let caseT_testcase2 = "
  let t = node(1, node(2, emptytree, emptytree), emptytree)
  in
  caseT t of {
    emptytree -> 1,
    node(a,l,r) -> if empty?(r) then 12 else 13
  }
"

let caseT_testcase3 = "
  let t = node(zero?(9), emptytree, node(2, emptytree, emptytree))
  in
  caseT t of {
    emptytree -> 1,
    node(a,l,r) -> caseT r of {
      emptytree -> 66,
      node(a,l,r) -> a
     }
  }
"

let caseT_testcase4 = "
  let t = node(emptylist,
              node(cons(5, cons(2, cons(1, emptylist))),
                    emptytree,
                    node(emptylist,
                        emptytree,
                        emptytree
                    )
              ),
              node(tl(cons(5, cons(6, emptylist))),
                    node(cons(10, cons(9, cons(8, emptylist))),
                        emptytree,
                        emptytree
                    ),
                    node(emptylist,
                        node(cons(9, emptylist),
                              emptytree,
                              emptytree
                        ),
                        emptytree
                    )
              )
          )
  in
  caseT t of {
    emptytree -> 10,
    node(a,l,r) ->
      caseT r of {
        emptytree -> 13,
        node(b,ll,rr) -> if empty?(b)
                        then 4
                        else if empty?(rr)
                              then 7
                              else 1
      }
}"

let caseT_testcase5 = "
  let t = node(emptylist,
              node(cons(5, cons(2, cons(1, emptylist))),
                    emptytree,
                    node(emptylist,
                        emptytree,
                        emptytree
                    )
              ),
              node(tl(cons(5, emptylist)),
                    node(cons(10, cons(9, cons(8, emptylist))),
                        emptytree,
                        emptytree
                    ),
                    node(emptylist,
                        node(cons(9, emptylist),
                              emptytree,
                              emptytree
                        ),
                        emptytree
                    )
              )
          )
  in
  caseT t of {
    emptytree -> 10,
    node(a,l,r) ->
      if empty?(a)
      then caseT l of {
              emptytree -> 21,
              node(b,ll,rr) -> if empty?(b)
                              then 4
                              else if zero?(hd(b))
                                    then 22
                                    else 99
          }
      else 5
}"

(* ----------------------------------------------------
 * Define the test suite below and assign point values.
 *             Points should sum to 100.
 * ---------------------------------------------------- *)

(* Total pts = 20 *)
let abs_tests : unit_test list = [
  Interp ("abs-of-positive", "abs(2)", Ok (NumVal 2), 2);
  Interp ("abs-of-negative", "abs((-2))", Ok (NumVal 2), 2);
  Interp ("abs-of-positive-arith", "abs(7 - 3) - 1", Ok (NumVal 3), 2);
  Interp ("abs-of-negative-arith", "abs((-5) + (-3)) - 9", Ok (NumVal (-1)), 4);
  Interp ("abs-of-var", "let y = (-3) in abs(y)", Ok (NumVal 3), 4);
  Interp ("abs-of-bool", "abs(zero?(0))", generic_err, 2);
  Interp ("abs-of-let", "abs(let y = abs((-1)) + 1 in 2*y)", Ok (NumVal 4), 4);
]

(* Total pts = 30 *)
let list_tests : unit_test list = [
  Interp ("empty-list", "emptylist", Ok (ListVal []), 2);

  Interp ("cons-1", "cons(1, emptylist)", Ok (ListVal [NumVal 1]), 2);
  Interp ("cons-2",
    "cons(2, cons(1, emptylist))",
    Ok (ListVal [NumVal 2; NumVal 1]),
    2);
  Interp ("cons-3",
    "cons(zero?(0), cons(3, emptylist))",
    Ok (ListVal [BoolVal true; NumVal 3]),
    2);
  Interp ("cons-4", "cons(5, 6)", generic_err, 1);

  Interp ("hd-1", "hd(cons(7, emptylist))", Ok (NumVal 7), 2);
  Interp ("hd-2", "hd(cons(abs((-2)), cons(1, emptylist)))", Ok (NumVal 2), 2);
  Interp ("hd-3", "hd(cons(zero?(1), emptylist))", Ok (BoolVal false), 2);
  Interp ("hd-4", "hd(emptylist)", generic_err, 1);

  Interp ("tl-1", "tl(cons(7, emptylist))", Ok (ListVal []), 2);
  Interp ("tl-2",
    "tl(cons(abs((-2)), cons(1, cons(0, emptylist))))",
    Ok (ListVal [NumVal 1; NumVal 0]),
    2);
  Interp ("tl-3",
    "tl(cons(zero?(1), cons(zero?(0), emptylist)))",
    Ok (ListVal [BoolVal true]),
    2);
  Interp ("tl-4", "tl(emptylist)", generic_err, 1);

  Interp ("empty-1", "empty?(emptylist)", Ok (BoolVal true), 2);
  Interp ("empty-2", "empty?(emptytree)", Ok (BoolVal true), 2);
  Interp ("empty-3",
    "empty?(node(9, emptytree, emptytree))",
    Ok (BoolVal false),
    2);
  Interp ("empty-4", "empty?(9)", generic_err, 1);
]

(* Total pts = 50 *)
let btrees_tests : unit_test list = [
  Interp ("emptytree", "emptytree", Ok (TreeVal Empty), 5);

  Interp ("btree-1",
    "node(3, emptytree, emptytree)",
    Ok (TreeVal (Node (NumVal 3, Empty, Empty))),
    3);
  Interp ("btree-2",
    "node(5, node(6, emptytree, emptytree), emptytree)",
    Ok (TreeVal (Node (NumVal 5, Node (NumVal 6, Empty, Empty), Empty))),
    4);
  Interp ("btree-3",
    "node(zero?(9), node(16, emptytree, emptytree), emptytree)",
    Ok (TreeVal (
      Node (BoolVal false,
        Node (NumVal 16, Empty, Empty),
        Empty))),
    5);
  Interp ("btree-4",
    "node(abs((-22)), \
          node(0, emptytree, emptytree), \
          node(0, emptytree, emptytree))",
    Ok (TreeVal (
      Node (NumVal 22,
        Node (NumVal 0, Empty, Empty),
        Node (NumVal 0, Empty, Empty)))),
    5);
  Interp ("btree-5",
    "node(cons(25, cons(50, emptylist)), \
      emptytree, \
      node(7, node(emptylist, emptytree, emptytree), emptytree))",
    Ok (TreeVal (
      Node (ListVal [NumVal 25; NumVal 50],
        Empty,
        Node (NumVal 7, Node (ListVal [], Empty, Empty), Empty)))),
    4);
  Interp ("btree-6",
    "node( \
      node(let z=74 in z-10, \
        emptytree, \
        node(hd(cons(4, emptylist)), emptytree, emptytree)), \
      node(let z=71 in z+10, emptytree, node(2, emptytree, emptytree)), \
      emptytree)",
    Ok (TreeVal (
      Node (TreeVal (
            Node (NumVal 64, Empty, Node (NumVal 4, Empty, Empty))),
        Node(NumVal 81, Empty, Node(NumVal 2, Empty, Empty)),
        Empty))),
    4);

  Interp ("caseT-1", caseT_testcase1, Ok (BoolVal true), 4);
  Interp ("caseT-2", caseT_testcase2, Ok (NumVal 12), 4);
  Interp ("caseT-3", caseT_testcase3, Ok (NumVal 2), 4);
  Interp ("caseT-4", caseT_testcase4, Ok (NumVal 1), 4);
  Interp ("caseT-5", caseT_testcase5, Ok (NumVal 99), 4);
]

let _ = run_suite "graded test suite" [
  abs_tests;
  list_tests;
  btrees_tests;
]
