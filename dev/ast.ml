(** AST **) 
open Printf

(* primitive operators *)
type prim1 = Add1 | Sub1 | Not | Print
type prim2 = Add | Sub | Mul | Div | Mod | And | Or | Xor | Lt | Lte | Gt | Gte | Eq | Neq | Get
(* comment out this line if providing print via the sys interface *)

(* Algebraic datatype for expressions *)
type expr = 
  | Empty
  | Num of int64
  | Bool of bool
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | Id of string
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Apply of string * expr list
  | Tuple of expr list 
  | Set of expr * expr * expr 
  | Lambda of string list * expr
  | LamApply of expr * expr list
  | LetRec of (string * string list * expr) list * expr

(* C function argument types *)
type ctype =
  | CAny
  | CInt
  | CBool
  | CTuple of ctype list

(* Function definitions *)
type fundef =
  | RecordInit of string * string list (* record id, record fields*)
  | RecordGetter of string * int (* record-field, field pos *)
  | DefFun of string * string list * expr
  | DefSys of string * ctype list * ctype

let fundef_name(f : fundef) : string =
  match f with
  | RecordInit   (n, _) -> n
  | RecordGetter (n, _) -> n
  | DefFun (n, _, _) -> n
  | DefSys (n, _, _) -> n

(* Program including definitions and a body *)
type prog = fundef list * expr


(* Pretty printing expressions - used by testing framework *)

(* Type for tagged expressions *)
type tagExpr = 
  | TNum of int64 * int
  | TBool of bool * int
  | TPrim1 of prim1 * tagExpr * int
  | TPrim2 of prim2 * tagExpr * tagExpr * int
  | TId of string * int
  | TLet of string * tagExpr * tagExpr * int
  | TIf of tagExpr * tagExpr * tagExpr * int
  | TApply of string * tagExpr list * int
  | TTuple of tagExpr list * int
  | TSet of tagExpr * tagExpr * tagExpr * int
  | TLambda of string list * tagExpr * int * int
  | TLamApply of tagExpr * tagExpr list * int
  | TLetRec of (string * string list * tagExpr) list * tagExpr * int
  | TEmpty
(* Pretty printing - used by testing framework *)
let rec string_of_expr(e : expr) : string = 
  match e with
  | Num n -> Int64.to_string n
  | Bool b -> if b then "true" else "false"
  | Id s -> s
  | Prim1 (op, e) -> sprintf "(%s %s)" 
    (match op with
    | Not -> "not"
    | Add1 -> "add1"
    | Sub1 -> "sub1"
    | Print -> "print") (string_of_expr e) (* remove the print case when providing the sys interface *)
  | Prim2 (op, e1, e2) -> sprintf "(%s %s %s)" 
    (match op with 
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Lt  -> "<"
    | Lte -> "<="
    | Gt  -> ">"
    | Gte -> ">="
    | Eq  -> "=="
    | Neq -> "!="
    | And -> "and"
    | Or  -> "or"
    | Xor -> "xor"
    | Get -> "get"
    ) (string_of_expr e1) (string_of_expr e2)
  | Let (x, e1, e2) -> sprintf "(let (%s %s) %s)" x (string_of_expr e1) (string_of_expr e2) 
  | If (e1, e2, e3) -> sprintf "(if %s %s %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Apply (fe, ael) -> sprintf "(%s %s)" fe (String.concat " " (List.map string_of_expr ael))
  | Tuple (exprs) -> sprintf "(tup %s)" (string_of_exprs exprs) 
  | Set (e, k, v) -> sprintf "(set %s %s %s)" (string_of_expr e) (string_of_expr k) (string_of_expr v) 
  | Lambda (params, body) -> sprintf "(lambda (%s) %s)" (String.concat " " params) (string_of_expr body)
  | LamApply (fe, ael) -> sprintf "(%s %s)" (string_of_expr fe) (String.concat " " (List.map string_of_expr ael))
  | LetRec (recs, body) -> sprintf "(letrec (%s) %s)" (String.concat " " (List.map (
      fun (name, params, body) -> 
        sprintf "(%s %s)" name (string_of_expr (Lambda (params, body)))
        ) recs
      )) (string_of_expr body)
  | Empty -> failwith "Null Expression string_of_expr"
  and string_of_exprs (e: expr list) : string = 
      match e with
      | [] -> ""
      | h :: t -> " " ^ (string_of_expr h) ^ (string_of_exprs t) 


(** functions below are not used, would be used if testing the parser on defs **)

(* Pretty printing C types - used by testing framework *)
let rec string_of_ctype(t : ctype) : string =
match t with
| CAny -> "any"
| CInt -> "int"
| CBool -> "bool"
| CTuple types -> 
        let rec string_of_types =
            fun ls -> (match ls with
            | [] -> ""
            | e::l -> e ^ "," ^ string_of_types l) in
        "("^string_of_types (List.map string_of_ctype types)^")"


(* Pretty printing function definitions - used by testing framework *)
let string_of_fundef(d : fundef) : string =
  match d with
  | RecordGetter (name, _) -> sprintf "(%s)" name
  | RecordInit (name, fields) -> sprintf "(%s %s)" name (String.concat " " fields)
  | DefFun (name, arg_ids, body) -> sprintf "(def (%s %s) %s)" name (String.concat " " arg_ids) (string_of_expr body)
  | DefSys (name, arg_types, ret_type) -> sprintf "(defsys %s %s -> %s)" name (String.concat " " (List.map string_of_ctype arg_types)) (string_of_ctype ret_type)

(* Pretty printing a program - used by testing framework *)
let string_of_prog(p : prog) : string =
  let fundefs, body = p in
  String.concat "\n" ((List.map string_of_fundef fundefs) @ [string_of_expr body])
