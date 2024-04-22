open Ast
open Printf
open List


let arg_n (i:int) : string =
  sprintf "__arg%d" i

let rec id_list (i:int) : expr list =
  if i > 0
    then [Id (arg_n (i-1))] @ (id_list (i-1))
    else []


let lamArg_n (n:int) :string =
  sprintf "__lambda_arg%d" n
let rec lamId_list (i:int) : expr list=
  if i > 0 
    then [Id (lamArg_n (i-1))] @ (lamId_list (i-1))
    else []


(* Normalizes a function application. Expects normalized arglist *)
let rec norm_apply (arglist: expr list) (i : int) (fname : string) (ids : expr list) : expr =
  match arglist with
  | [] -> Apply (fname, ids)
  | arg::rest -> (Let ((arg_n i), arg, norm_apply rest (i+1) fname ids))

(* Normalizes a lambda function application. Expects normalized arglist and lambda *)
let rec norm_lamApply (arglist: expr list) (i : int) (ids : expr list) : expr =
  match arglist with
  | [] -> LamApply (Empty, ids)
  | arg::rest -> (Let ((lamArg_n i), arg, norm_lamApply rest (i+1) ids))

let rec norm_tuple_elems (elems : expr list) (i : int) (len : int) : expr = 
  match elems with 
  | elem::rest ->
    Let (sprintf "__elem%d" i, elem, (norm_tuple_elems rest (i + 1) len))
  | [] -> let elemIds = List.init len (fun x->Id (sprintf "__elem%d" x)) in
    Tuple (elemIds)

  (*
Converts expression into A-Normal Form.
AND and OR are special cases, in order to be able to add evaluation shortcuts   
*)
let rec normalize (e: expr): expr =
  match e with
  | Prim1 (op, e1) -> (Let ("__arg1", (normalize e1), Prim1 (op, (Id "__arg1"))))
  | Prim2 (op, e1, e2) -> 
    (match op with 
    | And -> Prim2 (And, normalize e1, normalize e2)
    | Or -> Prim2 (Or, normalize e1, normalize e2)
    | _ -> (Let ("__arg1", (normalize e1), 
            (Let ("__arg2", (normalize e2), 
              Prim2 (op, (Id "__arg1"), (Id "__arg2")))))))
  | Let (id, value, body) -> Let (id, normalize value, normalize body)
  | If (cond, eThen,  eElse) -> If (normalize cond, normalize eThen, normalize eElse)
  | Apply (fname, arglist) -> norm_apply (map normalize arglist) 0 fname (id_list (length arglist))
  | Tuple (exprs) -> (norm_tuple_elems (map normalize exprs) 0 (List.length exprs))
  | Set (t, p, v) -> 
    (Let ("__pos", (normalize p), 
      Let ("__val", (normalize v),
        Let ("__tup", (normalize t),
          Set (Id "__tup", Id "__pos", Id "__val")))))
  | Lambda (argnames, body) -> (Lambda (argnames, normalize body))
  | LamApply (lmbd, arglist) ->
    let n_lambda = (normalize lmbd)  in
    Let ("__closure", n_lambda, 
    norm_lamApply (map normalize arglist) 0 (lamId_list (length arglist)))
  | LetRec (recs, body) -> 
    LetRec (List.map (
      fun (name, params, b) -> (name, params, normalize b)) recs, normalize body)
  | Empty -> failwith "Null Expression normalize"
  | e -> e




  (* | LetRec (recs, body) -> sprintf "(letrec (%s) %s)" (String.concat " " (List.map (
    fun (name, params, body) -> 
      sprintf "(%s %s)" name (string_of_expr (Lambda (params, body)))
      ) recs
    )) (string_of_expr body) *)
