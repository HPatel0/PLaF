open OUnit2
open Ast
open Ds
open Interp

let caseT_testcase = "
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
}
";;

(* A few test cases *)
let tests = [
  "abs_0"  >:: (fun _ -> assert_equal
                   ~printer:string_of_expval
                   (NumVal 0)
                   (interp "abs(0)"));

  "abs_pos"  >:: (fun _ -> assert_equal
                     ~printer:string_of_expval
                     (NumVal 22)
                     (interp "abs(22)"));

  "abs_neg"  >:: (fun _ -> assert_equal
                     ~printer:string_of_expval
                     (NumVal 22)
                     (interp "abs((-22))"));

  "add"  >:: (fun _ -> assert_equal
                 ~printer:string_of_expval
                 (NumVal 22)
                 (interp "11+11"));

  "adds" >:: (fun _ -> assert_equal
                 ~printer:string_of_expval
                 (NumVal 22)
                 (interp "(10+1)+(5+6)"));

  "mults" >:: (fun _ -> assert_equal
                  ~printer:string_of_expval
                  (NumVal 21)
                  (interp "(10*1)+(5+6)"));

  "divs" >:: (fun _ -> assert_equal
                 ~printer:string_of_expval
                 (NumVal 16)
                 (interp "(10/2)+(5+6)"));

  "let"  >:: (fun _ -> assert_equal
                 ~printer:string_of_expval
                 (NumVal 44)
                 (interp "let x=22 in x+x"));

  "lets" >:: (fun _ -> assert_equal
                 ~printer:string_of_expval
                 (NumVal 22)
                 (interp "let x = 0 in let x = 22 in (x+x)/2"));

  "emptylist" >:: (fun _ -> assert_equal
                      ~printer:string_of_expval
                      (ListVal [])
                      (interp "emptylist"));

  "cons_singleton" >:: (fun _ -> assert_equal
                           ~printer:string_of_expval
                           (ListVal [NumVal 1])
                           (interp "cons(1, emptylist)"));

  "cons_list" >:: (fun _ -> assert_equal
                      ~printer:string_of_expval
                      (ListVal [NumVal 3; NumVal 2; NumVal 1])
                      (interp "cons(3, cons(2, cons(1, emptylist)))"));

  "hd_singleton" >:: (fun _ -> assert_equal
                         ~printer:string_of_expval
                         (NumVal 1)
                         (interp "hd(cons(1, emptylist))"));

  "hd_list" >:: (fun _ -> assert_equal
                    ~printer:string_of_expval
                    (NumVal 3)
                    (interp "hd(cons(3, cons(2, cons(1, emptylist))))"));

  "tl_singleton" >:: (fun _ -> assert_equal
                         ~printer:string_of_expval
                         (ListVal [])
                         (interp "tl(cons(1, emptylist))"));

  "tl_list1" >:: (fun _ -> assert_equal
                     ~printer:string_of_expval
                     (ListVal [NumVal 1])
                     (interp "tl(cons(3, cons(1, emptylist)))"));

  "tl_list1" >:: (fun _ -> assert_equal
                     ~printer:string_of_expval
                     (ListVal [NumVal 2; NumVal 1])
                     (interp "tl(cons(3, cons(2, cons(1, emptylist))))"));

  "null_true" >:: (fun _ -> assert_equal
                      ~printer:string_of_expval
                      (BoolVal true)
                      (interp "empty?(tl(cons(1, emptylist)))"));

  "null_false" >:: (fun _ -> assert_equal
                       ~printer:string_of_expval
                       (BoolVal false)
                       (interp "empty?(tl(cons(2, cons(1, emptylist))))"));

  "node constructor 1" >:: (fun _ -> assert_equal
                               ~printer:string_of_expval
                               (TreeVal (Node(NumVal(1), Empty, Empty)))
                               (interp "node(1,emptytree,emptytree)"));

  "node constructor 2" >:: (fun _ -> assert_equal
                               ~printer:string_of_expval
                               (TreeVal (Node(
                                    ListVal([NumVal(1);NumVal(2);NumVal(3)]),
                                    Node(
                                      ListVal([]),
                                      Empty,
                                      Empty),
                                    Empty)))
                               (interp "node(
                            cons(1, cons(2, cons(3, emptylist))),
                            node(emptylist, emptytree, emptytree),
                            emptytree
                          )"));

  "caseT 1" >:: (fun _ -> assert_equal
                    ~printer:string_of_expval
                    (NumVal 99)
                    (interp caseT_testcase)
  );

    (*emptytree test*)

    "emptytree" >:: (fun _ -> assert_equal
                        ~printer:string_of_expval
                        (TreeVal Empty)
                        (interp "emptytree"));

  (*empty? tests*)

  "empty? 1" >:: (fun _ -> assert_equal
                     ~printer:string_of_expval
                     (BoolVal true)
                     (interp "empty?(emptytree)"));

  "empty? 2" >:: (fun _ -> assert_equal
                     ~printer:string_of_expval
                     (BoolVal false)
                     (interp "empty?(node(1,emptytree,emptytree))"));

]

let _ = run_test_tt_main ("suite" >::: tests)
