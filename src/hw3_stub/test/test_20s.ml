open OUnit2
open Proc.Ds
open Proc.Interp

(* 
(* A few test cases *)
let tests_let = [
  "int"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "22"));
  "add"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "11+11"));
  "adds" >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "(10+1)+(5+6)"));
  "let"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "let x=22 in x"));
  "lets" >:: (fun _ -> assert_equal (Ok (NumVal 22))
                 (interp "let x = 0 in let x = 22 in x"));
    "add"  >:: (fun _ -> assert_equal
                   (*                 ~printer:string_of_expval *)
                 (Ok (NumVal 22))
                 (interp "11+11"));

  "adds" >:: (fun _ -> assert_equal
                 (Ok (NumVal 22))
                 (interp "(10+1)+(5+6)"));

  "subs" >:: (fun _ -> assert_equal
                 (Ok (NumVal 20))
                 (interp "(10-1)+(5+6)"));

  "mults" >:: (fun _ -> assert_equal
                 (Ok (NumVal 21))
                 (interp "(10*1)+(5+6)"));

  "divs" >:: (fun _ -> assert_equal
                 (Ok (NumVal 16))
                 (interp "(10/2)+(5+6)"));

  "let"  >:: (fun _ -> assert_equal
                 (Ok (NumVal 44))
                 (interp "let x=22 in x+x"));

  "lets" >:: (fun _ -> assert_equal
                 (Ok (NumVal 22))
                 (interp "let x = 0 in let x = 22 in (x+x)/2"));
]


let tests_proc = [
  "int"  >:: (fun _ -> assert_equal (Ok (NumVal 3))
                 (interp "(proc (x) { x+1 } 2)"))
]


let tests_extensions = [
  "abs_0"  >:: (fun _ -> assert_equal
                 (Ok (NumVal 0))
                 (interp "abs(0)"));

  "abs_pos"  >:: (fun _ -> assert_equal
                 (Ok (NumVal 22))
                 (interp "abs(22)"));

  "abs_neg"  >:: (fun _ -> assert_equal
                 (Ok (NumVal 22))
                 (interp "abs((-22))"));

  "emptylist" >:: (fun _ -> assert_equal
                 (Ok (ListVal []))
                 (interp "emptylist"));

  "cons_singleton" >:: (fun _ -> assert_equal                
                 (Ok (ListVal [NumVal 1]))
                 (interp "cons(1, emptylist)"));

  "cons_list" >:: (fun _ -> assert_equal                 
                 (Ok (ListVal [NumVal 3; NumVal 2; NumVal 1]))
                 (interp "cons(3, cons(2, cons(1, emptylist)))"));

  "hd_singleton" >:: (fun _ -> assert_equal                 
                 (Ok (NumVal 1))
                 (interp "hd(cons(1, emptylist))"));

  "hd_list" >:: (fun _ -> assert_equal               
                 (Ok (NumVal 3))
                 (interp "hd(cons(3, cons(2, cons(1, emptylist))))"));

  "tl_singleton" >:: (fun _ -> assert_equal                 
                 (Ok (ListVal []))
                 (interp "tl(cons(1, emptylist))"));

  "tl_list1" >:: (fun _ -> assert_equal                 
                 (Ok (ListVal [NumVal 1]))
                 (interp "tl(cons(3, cons(1, emptylist)))"));

  "tl_list1" >:: (fun _ -> assert_equal                 
                 (Ok (ListVal [NumVal 2; NumVal 1]))
                 (interp "tl(cons(3, cons(2, cons(1, emptylist))))"));

  "null_true" >:: (fun _ -> assert_equal                 
                 (Ok (BoolVal true))
                 (interp "empty?(tl(cons(1, emptylist)))"));

  "null_false" >:: (fun _ -> assert_equal               
                 (Ok (BoolVal false))
                 (interp "empty?(tl(cons(2, cons(1, emptylist))))"));
]

let _ = run_test_tt_main ("suite" >::: (tests_let @ tests_proc @ tests_extensions))
*)
     
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

(* Total pts = 20 *)
let tests_abs = [
  (* Pts = 4, 4, 4, 4, 4 *)
  "test01" >:: (fun _ -> assert_equal (Ok (NumVal (-1))) (interp "abs((-5)) - 6"));
  "test02" >:: (fun _ -> assert_equal (Ok (NumVal 1)) (interp "abs(7) - 6"));
  "test03" >:: (fun _ -> assert_equal (Ok (NumVal 2)) (interp "abs((-2))"));
  "test04" >:: (fun _ -> assert_equal (Ok (NumVal 2)) (interp "abs(2)"));
  "test05" >:: (fun _ -> assert_equal (Ok (NumVal 3)) (interp "let y = (-3) in abs(y)"));
]

(* Total pts = 30 *)
let tests_lists = [
  (* Pts = 2 *)
  "test06" >:: (fun _ -> assert_equal (Ok (ListVal [])) (interp "emptylist"));

  (* Pts = 2, 2, 2, 1 *)
  "test07" >:: (fun _ -> assert_equal (Ok (ListVal [NumVal 1])) (interp "cons(1, emptylist)"));
  "test08" >:: (fun _ -> assert_equal (Ok (ListVal [NumVal 2; NumVal 1])) (interp "cons(2, cons(1, emptylist))"));
  "test09" >:: (fun _ -> assert_equal (Ok (ListVal [BoolVal true; NumVal 3])) (interp "cons(zero?(0), cons(3, emptylist))"));
  "test10" >:: (fun _ -> assert (let x = interp "cons(5, 6)" in match x with Error _ -> true | _ -> false));

  (* Pts = 2, 2, 2, 1 *)
  "test11" >:: (fun _ -> assert_equal (Ok (NumVal 7)) (interp "hd(cons(7, emptylist))"));
  "test12" >:: (fun _ -> assert_equal (Ok (NumVal 2)) (interp "hd(cons(abs((-2)), cons(1, emptylist)))"));
  "test13" >:: (fun _ -> assert_equal (Ok (BoolVal false)) (interp "hd(cons(zero?(1), emptylist))"));
  "test14" >:: (fun _ -> assert (let x = interp "hd(emptylist)" in match x with Error _ -> true | _ -> false));

  (* Pts = 2, 2, 2, 1 *)
  "test15" >:: (fun _ -> assert_equal (Ok (ListVal [])) (interp "tl(cons(7, emptylist))"));
  "test16" >:: (fun _ -> assert_equal (Ok (ListVal [NumVal 1; NumVal 0])) (interp "tl(cons(abs((-2)), cons(1, cons(0, emptylist))))"));
  "test17" >:: (fun _ -> assert_equal (Ok (ListVal [BoolVal true])) (interp "tl(cons(zero?(1), cons(zero?(0), emptylist)))"));
  "test18" >:: (fun _ -> assert (let x = interp "tl(emptylist)" in match x with Error _ -> true | _ -> false));

  (* Pts = 2, 2, 2, 1 *)
  "test19" >:: (fun _ -> assert_equal (Ok (BoolVal true)) (interp "empty?(emptylist)"));
  "test20" >:: (fun _ -> assert_equal (Ok (BoolVal true)) (interp "empty?(emptytree)"));
  "test21" >:: (fun _ -> assert_equal (Ok (BoolVal false)) (interp "empty?(node(9, emptytree, emptytree))"));
  "test22" >:: (fun _ -> assert (let x = interp "empty?(9)" in match x with Error _ -> true | _ -> false));

]

(* Total pts = 50 *)
let tests_btrees = [
  (* Pts = 5 *)
  "test23" >:: (fun _ -> assert_equal (Ok (TreeVal Empty)) (interp "emptytree"));

  (* Pts = 3, 4, 5, 5, 4, 4 *)
  "test24" >:: (fun _ -> assert_equal (Ok (TreeVal(Node(NumVal 3, Empty, Empty)))) (interp "node(3, emptytree, emptytree)"));
  "test25" >:: (fun _ -> assert_equal (Ok (TreeVal(Node(NumVal 5, Node(NumVal 6, Empty, Empty), Empty)))) (interp "node(5, node(6, emptytree, emptytree), emptytree)"));
  "test26" >:: (fun _ -> assert_equal (Ok (TreeVal(Node(BoolVal false, Node(NumVal 16, Empty, Empty), Empty)))) (interp "node(zero?(9), node(16, emptytree, emptytree), emptytree)"));
  "test27" >:: (fun _ -> assert_equal (Ok (TreeVal(Node(NumVal 22, Node(NumVal 0, Empty, Empty), Node(NumVal 0, Empty, Empty))))) 
    (interp "node(abs((-22)), node(0, emptytree, emptytree), node(0, emptytree, emptytree))"));
  "test28" >:: (fun _ -> assert_equal (Ok (TreeVal(
                                            Node(ListVal [NumVal 25; NumVal 50], 
                                            Empty, 
                                            Node(NumVal 7, Node(ListVal [], Empty, Empty), Empty))))) 
    (interp "node(cons(25, cons(50, emptylist)), emptytree, node(7, node(emptylist, emptytree, emptytree), emptytree))"));
  "test29" >:: (fun _ -> assert_equal (Ok (TreeVal(
                                            Node(TreeVal(Node(NumVal 64, Empty, Node(NumVal 4, Empty, Empty))), 
                                            Node(NumVal 81, Empty, Node(NumVal 2, Empty, Empty)), 
                                            Empty))))
    (interp "node(node(let z=74 in z-10, emptytree, node(hd(cons(4, emptylist)), emptytree, emptytree)), node(let z=71 in z+10, emptytree, node(2, emptytree, emptytree)), emptytree)"));

  (* Pts = 4, 4, 4, 4, 4 *)
  "test30" >:: (fun _ -> assert_equal (Ok (BoolVal true)) (interp caseT_testcase1));
  "test31" >:: (fun _ -> assert_equal (Ok (NumVal 12)) (interp caseT_testcase2));
  "test32" >:: (fun _ -> assert_equal (Ok (NumVal 2)) (interp caseT_testcase3));
  "test33" >:: (fun _ -> assert_equal (Ok (NumVal 1)) (interp caseT_testcase4));
  "test34" >:: (fun _ -> assert_equal (Ok (NumVal 99)) (interp caseT_testcase5));

]

let _ = run_test_tt_main ("suite" >::: (tests_abs @ tests_lists @ tests_btrees))
