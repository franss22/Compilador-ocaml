open Ast
open Util

            (* arity, label, N of free vars, free vars (values)  *)
type closure = Clos of int * string * int * expr list


let rec tfreeVarsUtil (ex: tagExpr) (closed: string list): (string list) =
  match ex with
  | TNum _ | TBool _ -> []
  | TId (name, _) -> if (member name closed)
                then []
                else [name]
  | TPrim1 (_, ex1, _) -> (tfreeVarsUtil ex1 closed)
  | TPrim2 (_, ex1, ex2, _) -> (tfreeVarsUtil ex1 closed)@(tfreeVarsUtil ex2 closed)
  | TLet (id, value, body, _) -> (tfreeVarsUtil value closed)@(tfreeVarsUtil body (id::closed))
  | TIf (cond, tb, fb, _) -> (tfreeVarsUtil cond closed)@(tfreeVarsUtil tb closed)@(tfreeVarsUtil fb closed)
  | TApply (_, args, _) -> List.fold_left (fun fr ex -> (tfreeVarsUtil ex closed)@fr) [] args
  | TTuple (vals, _) -> List.fold_left (fun fr ex -> (tfreeVarsUtil ex closed)@fr) [] vals
  | TSet (ex1, ex2, ex3, _) ->(tfreeVarsUtil ex1 closed)@(tfreeVarsUtil ex2 closed)@(tfreeVarsUtil ex3 closed)
  | TLambda (argnames, body, _, _) -> tfreeVarsUtil body (argnames@closed)
  (* | TLamApply (_, args, _) -> List.fold_left (fun fr ex -> (tfreeVarsUtil ex closed)) [] args *)
  | TLamApply (_, args, _) -> List.fold_left (fun fr ex -> (tfreeVarsUtil ex closed)@fr) [] args
  | TLetRec (decs, _(*body*), _) ->
    let nClosed = List.fold_left (fun cl (name, _, _) -> name::cl) closed decs in
    nClosed
    (* let bodyFree = tfreeVarsUtil body nClosed in *)
    (* List.fold_left (fun fr (_, argnames, bd) -> (tfreeVarsUtil (TLambda (argnames, bd, 1, 1)) nClosed)@fr) bodyFree decs *)
  | TEmpty -> failwith "Null Expression tfreeVarsUtil"


let tfreeVars (ex:tagExpr):(string list) =
  remove_duplicates (tfreeVarsUtil ex [])

(* let rec freeVarsUtil (ex: expr) (closed: string list): (string list) =
  match ex with
  | Num _ | Bool _ -> []
  | Id name -> if (member name closed)
                then []
                else [name]
  | Prim1 (_, ex1) -> (freeVarsUtil ex1 closed)
  | Prim2 (_, ex1, ex2) -> (freeVarsUtil ex1 closed)@(freeVarsUtil ex2 closed)
  | Let (id, value, body) -> (freeVarsUtil value closed)@(freeVarsUtil body (id::closed))
  | If (cond, tb, fb) -> (freeVarsUtil cond closed)@(freeVarsUtil tb closed)@(freeVarsUtil fb closed)
  | Apply (_, args) -> List.fold_left (fun fr ex -> (freeVarsUtil ex closed)@fr) [] args
  | Tuple (vals) -> List.fold_left (fun fr ex -> (freeVarsUtil ex closed)@fr) [] vals
  | Set (ex1, ex2, ex3) ->(freeVarsUtil ex1 closed)@(freeVarsUtil ex2 closed)@(freeVarsUtil ex3 closed)
  | Lambda (argnames, body) -> freeVarsUtil body (argnames@closed)
  | LamApply (fn, args) -> (freeVarsUtil fn closed)@List.fold_left (fun fr ex -> (freeVarsUtil ex closed)@fr) [] args
  | LetRec (decs, body) ->
    let nClosed = List.fold_left (fun cl (name, _, _) -> name::cl) closed decs in
    let bodyFree = freeVarsUtil body nClosed in
    List.fold_left (fun fr (_, argnames, bd) -> (freeVarsUtil (Lambda (argnames, bd)) nClosed)@fr) bodyFree decs

let freeVars (ex:expr):(string list) =
  remove_duplicates (freeVarsUtil ex []) *)