(*Hari Patel*)
(*I pledge my honor that I have abided by the Stevens Honor System*)

open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds


let rec eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) ->
    return @@ NumVal n
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return @@ NumVal (n1/n2)
  | Let(id,def,body) ->
    eval_expr def >>=
    extend_env id >>+
    eval_expr body
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return @@ BoolVal (n = 0)
  | Proc(id,_,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  -> 
    eval_expr e1 >>= 
    clos_of_procVal >>= fun (id,e,en) ->
    eval_expr e2 >>= fun ev ->
    return en >>+
    extend_env id ev >>+
    eval_expr e
  | Abs(e1)      ->
    eval_expr e1  >>=
    int_of_numVal >>= fun n ->
    return @@ NumVal (abs n)

  | Record(fs) -> (*DONE*)
    sequence (List.map (fun (id, e) ->
      eval_expr e >>= fun ev ->
      return (id, ev)) fs) >>= fun fields ->
    let check_duplicates l =
      let rec helper = function
        | [] -> false
        | x::xs -> List.mem x xs || helper xs
      in
      helper l
    in
    if check_duplicates (List.map fst fields)
    then error "Record: duplicate field names"
    else return @@ RecordVal fields


  | Proj(e,id) -> (*DONE*)
    eval_expr e >>=
    fields_of_recordVal >>= fun fs ->
    (match List.assoc_opt id fs with
    | None -> error "Proj : field does not exist"
    | Some ev -> return ev)

  | Cons(e1, e2) ->
    failwith "implement me"
  | Hd(e1) ->
    failwith "implement me"
  | Tl(e1) ->
    failwith "implement me"

  | IsEmpty(e1)  -> (*DONE*)
    eval_expr e1 >>=
    list_of_listVal >>= fun n ->
    if is_listVal @@ ListVal(n) 
    then (
      if (n = [])
      then return @@ BoolVal true 
      else return @@ BoolVal false
    )
    else return (BoolVal (n=[]))

  | EmptyList    ->
    failwith "implement me" 

  | EmptyTree -> (*DONE*)
    return @@ TreeVal(Empty)

  | Node(e1,lte,rte) -> (*DONE*)
    eval_expr e1 >>= fun x ->
    eval_expr lte >>=
    tree_of_treeVal >>= fun left ->
    eval_expr rte >>=
    tree_of_treeVal >>= fun right ->
    return (TreeVal(Node(x,left,right)))

  | CaseT(target,emptycase,id1,id2,id3,nodecase) -> (*DONE*)
    eval_expr target >>= 
    tree_of_treeVal >>= fun x ->
    (match x with
    | Empty -> eval_expr emptycase
    | Node(data, left, right) -> 
      extend_env id1 data >>+
      extend_env id2 (TreeVal left) >>+
      extend_env id3 (TreeVal right) >>+
      eval_expr nodecase)

  | Tuple(es) ->
    failwith "implement me"
  | Untuple(ids,e1,e2) ->
    failwith "implement me"
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"
  | _ -> failwith "Not implemented yet!"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c

