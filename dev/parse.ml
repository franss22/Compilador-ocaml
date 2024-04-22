(** Parser **)
open Ast
open Printf
open CCSexp
open Util

exception CTError of string

let parse_arg_name (sexp : sexp) : string =
  match sexp with
  | `Atom name -> name
  | _ -> raise (CTError (sprintf "Not a valid argument name: %s" (to_string sexp)))

let rec encapsulate_fun_args (args : string list) (body : expr) : expr =
    match args with
    | arg::rest -> Let (arg, Id arg, encapsulate_fun_args rest body)
    | [] -> body

let rec parse_exp (sexp : sexp) : expr =
  match sexp with
  | `Atom "true" -> Bool true
  | `Atom "false" -> Bool false
  | `Atom s -> (
    match Int64.of_string_opt s with Some n -> Num n | None -> Id s )
  | `List (`Atom "tup" :: exprs) -> Tuple (List.map parse_exp exprs)
  | `List (`Atom "@" :: e1 :: exprs) -> LamApply (parse_exp e1, List.map parse_exp exprs)
  | `List [eop; e] -> (
    match eop with 
    | `Atom "not" -> Prim1 (Not, parse_exp e)
    | `Atom "add1" -> Prim1 (Add1, parse_exp e)
    | `Atom "sub1" -> Prim1 (Sub1, parse_exp e)
    | `Atom "print" -> Prim1 (Print, parse_exp e)  (* comment out this line if providing print via the sys interface *)
    | `Atom name -> Apply (name, [parse_exp e])
    | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp)))
    )
  | `List [eop; e1; e2] -> (
    match eop with
    | `Atom "let" -> (
      match e1 with
      | `List [`Atom id; e] -> (match id with
        | "add1" | "sub1" | "+" | "-" | "*" | "/" | "%" 
        | "not" | "and" | "or" | "xor" | "true" | "false"
        | "<" | ">" | "<=" | ">=" | "==" | "!=" | "if" | "let" -> failwith (sprintf "cannot override '%s'" id)
        | _ -> (match Int64.of_string_opt id with 
          | Some _ -> failwith "variable name can't be a number"
          | None -> Let (id, parse_exp e, parse_exp e2) ))
      | _ -> failwith "parse error in let" )
    | `Atom "letrec" -> (
      match e1 with
      | `List recs -> LetRec (parse_recs recs, parse_exp e2)
      | _ -> raise (CTError (sprintf "Not a valid letrec assignment: %s" (to_string e1)))
      )
    | `Atom "+"   -> Prim2 (Add, parse_exp e1, parse_exp e2)
    | `Atom "-"   -> Prim2 (Sub, parse_exp e1, parse_exp e2)
    | `Atom "*"   -> Prim2 (Mul, parse_exp e1, parse_exp e2)
    | `Atom "/"   -> Prim2 (Div, parse_exp e1, parse_exp e2)
    | `Atom "%"   -> Prim2 (Mod, parse_exp e1, parse_exp e2)
    | `Atom "and" -> Prim2 (And, parse_exp e1, parse_exp e2)
    | `Atom "or"  -> Prim2 (Or , parse_exp e1, parse_exp e2)
    | `Atom "xor" -> Prim2 (Xor, parse_exp e1, parse_exp e2)
    | `Atom "<"   -> Prim2 (Lt , parse_exp e1, parse_exp e2)
    | `Atom "<="  -> Prim2 (Lte, parse_exp e1, parse_exp e2)
    | `Atom ">"   -> Prim2 (Gt , parse_exp e1, parse_exp e2)
    | `Atom ">="  -> Prim2 (Gte, parse_exp e1, parse_exp e2)
    | `Atom "=="  -> Prim2 (Eq , parse_exp e1, parse_exp e2)
    | `Atom "!="  -> Prim2 (Neq, parse_exp e1, parse_exp e2)
    | `Atom "get" -> Prim2 (Get, parse_exp e1, parse_exp e2)
    | `Atom "lambda" -> (
      match e1 with
      | `List params -> 
        let args = List.map parse_arg_name params in
        let pbody = parse_exp e2 in
        let ebody = encapsulate_fun_args args pbody in
        Lambda (args, ebody)
      | _ -> raise (CTError (sprintf "Not a valid lambda: %s" (to_string sexp)))
    )
    | `Atom name -> Apply (name, [parse_exp e1 ; parse_exp e2])
    | _ -> failwith (sprintf "Not a valid expr: %s" (to_string sexp)) )
  | `List [`Atom "if"; e1; e2; e3] -> If (parse_exp e1, parse_exp e2, parse_exp e3)
  | `List [ `Atom "set"; e; k; v ] -> Set (parse_exp e, parse_exp k, parse_exp v)
  | `List (`Atom name :: e2) -> Apply (name, List.map parse_exp e2)
  | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp)))




and parse_recs (recs : sexp list) : (string * string list * expr) list =
  List.map (
    fun sexp -> (
      match sexp with
      | `List [`Atom name ; r] ->
        let e = parse_exp r in
        (match e with
        | Lambda (params, body) -> name, params, body
        | _ -> raise (CTError (sprintf "Not a valid letrec assignment: %s" (string_of_expr e)))
        )
      | _ -> raise (CTError (sprintf "Not a valid letrec assignment: %s" (to_string sexp)))
    )
  ) recs


(* Records are syntactic sugar for functions that make generic tuples and get the nth field of any tuple*)
let parse_record (id:string) (fields:string list): fundef list =
  let init = RecordInit (id, fields) in
  let num_flds = List.combine fields  (range 0 (List.length fields)) in
  let getters = (List.map (fun (fld, pos) -> RecordGetter (id^"-"^fld, pos)) num_flds) in
  [init] @ getters


(* Parses the fields of a record, making sure there are no duplicates *)
let fieldStrings (fields: sexp list) (record: string) : string list = 
  if dup_exist fields 
    then raise (CTError (sprintf "Duplicate field in record %s" record))
    else
      List.map (fun x -> 
          match x with 
          | `Atom a -> a 
          | _ -> raise (CTError (sprintf "Not a valid field: %s" (to_string x)))) 
      fields
let parse_arg_name (sexp : sexp) : string =
  match sexp with
  | `Atom name -> name
  | _ -> raise (CTError (sprintf "Not a valid argument name: %s" (to_string sexp)))

let parse_c_type (sexp : sexp) : ctype =
  match sexp with
  | `Atom "any" -> CAny
  | `Atom "int" -> CInt
  | `Atom "bool" -> CBool
  | _ -> raise (CTError (sprintf "Not a valid type declaration: %s" (to_string sexp)))

let rec parse_prog_check_records (sexp : sexp) (records: string list): prog =
  match sexp with
  | `List (hd :: tl) -> (
    match hd, tl with
    | `List (`Atom "record":: `Atom recordId :: fieldIds), _ ->
      if not (List.mem recordId records)
        then
          let (funcdefs, expr) = parse_prog_check_records (`List tl) ([recordId] @ records) in
          let record_funcs = parse_record recordId (fieldStrings fieldIds recordId) in
          record_funcs @ funcdefs, expr
        else raise (CTError (sprintf "Record %s is declared more than once" recordId))

    | `List [`Atom "def" ; `List (`Atom name :: args) ; body], _ ->
      let (funcdefs, expr) = parse_prog_check_records (`List tl) records in
      let arg_names = List.map parse_arg_name args in
      let fbody = parse_exp body in
      let ebody = encapsulate_fun_args arg_names fbody in
      [ DefFun (name, arg_names, ebody) ] @ funcdefs, expr

    | `List (`Atom "defsys" :: `Atom name :: arg_spec), _ -> (
      match List.rev arg_spec with
      | (ret :: `Atom "->" :: args) -> 
        let (funcdefs, expr) = parse_prog_check_records (`List tl) records in
        let arg_types = List.map parse_c_type (List.rev args) in
        let ret_type = parse_c_type ret in
        [ DefSys (name, arg_types, ret_type) ] @ funcdefs, expr
      | _ -> raise (CTError (sprintf "Not a valid type declaration: %s" (to_string (`List arg_spec))))
      )
    | _, [] -> [], parse_exp hd
    | _ -> [], parse_exp sexp
  )
  | _ -> [], parse_exp sexp

  
  let parse_prog (sexp: sexp) = parse_prog_check_records sexp []


let sexp_from_file : string -> CCSexp.sexp =
 fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> raise (CTError (sprintf "Unable to parse file %s: %s" filename msg))

let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> raise (CTError (sprintf "Unable to parse string %s: %s" src msg))
