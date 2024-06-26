(** Interpreter **)
open Ast
open Printf
open Parse

exception RTError of string

(** Values **)
type value = 
  | NumV of int64
  | BoolV of bool
  | TupleV of value ref list
  | ClosureV of int * (value list -> value)

(* Pretty printing *)
let rec string_of_val(v : value) : string =
match v with
| NumV n -> Int64.to_string n
| BoolV b -> if b then "true" else "false"
| TupleV vals -> 
        let rec string_of_val_list =
            fun ls -> (match ls with
            | [] -> ""
            | e::l -> " " ^ string_of_val !e ^ string_of_val_list l) in 
        "(tup"^(string_of_val_list vals)^")"
| ClosureV (arity, _) -> sprintf "<clos:%d>" arity

(* Lexical Environment *)
type env = (string * value) list
let empty_env : env = []

let extend_env (names : string list) (vals : value list) (env : env) : env =
  let param_vals = List.combine names vals in
  List.fold_left (fun env p -> p :: env) env param_vals

let lookup_env : string -> env -> value =
  fun s env ->
    match List.assoc_opt s env with
    | Some v -> v
    | None -> raise (CTError (sprintf "Unbound identifier: %s" s))

(* Function Environment *)
type fenv = fundef list
let empty_fenv : fenv = []
let rec lookup_fenv : string -> fenv -> fundef =
  fun s fenv -> 
    match fenv with
    | [] -> raise (CTError (sprintf "Undefined function: %s" s))
    | (f::fs) -> if fundef_name f = s then f else lookup_fenv s fs

(* Type checks *)
let raise_type_error (expected_type_s : string) (ill_value : value) : 'a =
  raise (RTError (sprintf "Type error: Expected %s but got %s" expected_type_s (string_of_val ill_value)))

let bool_of_value (v : value) : bool =
  match v with
  | BoolV b -> b
  | _ -> raise_type_error "boolean" v

(* Lifting functions on OCaml primitive types to operate on language values
   They throw a runtime type error if any of the received arguments is ill-typed,
   reporting the first infringing value (from left to right) if there are multiple.
*)
let liftIII : (int64 -> int64 -> int64) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> NumV (op n1 n2)
    | NumV _, _ -> raise_type_error "integer" e2
    | _ -> raise_type_error "integer" e1

let liftBBB : (bool -> bool -> bool) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | BoolV b1, BoolV b2 -> BoolV (op b1 b2)
    | BoolV _, _ -> raise_type_error "boolean" e2
    | _ -> raise_type_error "boolean" e1

let liftIIB : (int64 -> int64 -> bool) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> BoolV (op n1 n2)
    | NumV _, _ -> raise_type_error "integer" e2
    | _ -> raise_type_error "integer" e1

let liftBB : (bool -> bool) -> value -> value =
  fun op e1 ->
    match e1 with
    | BoolV b -> BoolV (op b)
    | _ -> failwith "runtime type error"

let raise_out_of_bounds_error (index : int) (tuple : value) : 'a =
  raise (RTError (sprintf "Index out of bounds: Tried to access index %d of %s"
                          index (string_of_val tuple)))

let get_ref : 'a list -> int64 -> 'a =
  fun ts n ->
    let (index, tup_len) = ((Int64.to_int n), List.length ts) in
    if index >= 0 && index < tup_len then
      List.nth ts index else
        raise_out_of_bounds_error index (TupleV ts)

let get_elem : value -> value -> value =
    fun v1 v2 ->
      match v1, v2 with
      | TupleV ts, NumV n -> !(get_ref ts n)
      | TupleV _, _ -> raise_type_error "integer" v2
      | _ -> raise_type_error "tuple" v1
  
let set_elem : value -> value -> value -> value = 
    fun v1 v2 v3 -> 
      match v1,v2 with
      | TupleV ts, NumV n ->
        let elem = get_ref ts n in
        elem := v3 ; TupleV ts
      | TupleV _, _ -> raise_type_error "integer" v2
      | _ -> raise_type_error "tuple" v1
  
(* Sys functions *)
let defs_prelude : fundef list = [
  DefSys ("print", [CAny], CAny) ;
  DefSys ("max", [CInt ; CInt], CInt) ;
  DefSys ("xor", [CBool ; CBool], CBool) ;
]


(* check that the value is of the given type, return the value if ok *)
let rec check_type (t : ctype) (v : value) : value =
  match v, t with
  | NumV _, CInt | BoolV _, CBool | _, CAny -> v
  | TupleV vals, CTuple types -> 
          let _ = List.map2 (fun x y -> (check_type x !y)) types vals in
          v
  | _, CInt -> raise_type_error "integer" v
  | _, CBool -> raise_type_error "boolean" v
  | _, CTuple _ -> raise_type_error "tuple" v            

(* Arity checks *)
let raise_arity_mismatch (fun_id : string) (expected : int) (received : int) (is_runtime_exn : bool) : 'a =
  let err_msg = (sprintf "Arity mismatch: %s expected %d arguments but got %d" fun_id expected received) in
  raise (if is_runtime_exn then (RTError err_msg) else (CTError err_msg))

let check_arity (fun_id : string) (expected : int) (received : int) (is_runtime_exn : bool) : unit =
  if expected == received then () else raise_arity_mismatch fun_id expected received is_runtime_exn

(* provide a dummy (non-C) interpretation of sys functions print and max *)
let interp_sys name vals = 
  let arg_count = List.length vals in
  match name with
  | "print" -> (match vals with 
                | v :: [] -> Printf.printf "> %s\n" (string_of_val v) ; v
                | _ -> raise_arity_mismatch name 1 arg_count false)
  | "max" -> (match vals with
              | NumV n1 :: NumV n2 :: [] -> NumV (if n1 >= n2 then n1 else n2)
              | _ -> raise_arity_mismatch name 2 arg_count false)
  | "xor" -> (match vals with
              | BoolV b1 :: BoolV b2 :: [] -> BoolV (b1 <> b2)
              | _ -> raise_arity_mismatch name 2 arg_count false)
  | _ -> raise (RTError (sprintf "Undefined function: %s" name))

(* interpreter *)
let rec interp expr env fenv =
  match expr with
  | Id x -> lookup_env x env
  | Num n -> NumV n
  | Bool b -> BoolV b
  | Prim1 (op, e) ->
    (match op with 
    | Not -> liftBB (not) (interp e env fenv)
    | Add1 -> liftIII ( Int64.add ) (interp e env fenv) (NumV 1L)
    | Sub1 -> liftIII ( Int64.sub ) (interp e env fenv) (NumV 1L)
    | Print -> (interp (Apply ("print", [e])) env fenv)) (* re-route interp of print to the fake sys interface *)
    
  | Prim2 (op, e1, e2) -> 
    (match op with
    | Add -> liftIII ( Int64.add ) 
    | Sub -> liftIII ( Int64.sub ) 
    | Mul -> liftIII ( Int64.mul ) 
    | Div -> liftIII ( Int64.div ) 
    | Mod -> liftIII ( Int64.rem ) 
    | Or  -> liftBBB ( || )
    | And -> liftBBB ( && ) 
    | Xor -> liftBBB ( <> ) 
    | Lt  -> liftIIB ( < )
    | Lte -> liftIIB ( <= )
    | Gt  -> liftIIB ( > )
    | Gte -> liftIIB ( >= )
    | Eq  -> liftIIB ( = )
    | Neq -> liftIIB ( <> )
    | Get -> get_elem
    ) (interp e1 env fenv) (interp e2 env fenv)
  | Let (x, e , b) -> interp b (extend_env [x] [(interp e env fenv)] env)  fenv
  (*| Let (x, e , b) -> interp b (extend_env [x] [(interp e env fenv)] env) fenv*)
  | If (e1, e2, e3) -> 
    let b = bool_of_value (interp e1 env fenv) in
    interp (if b then e2 else e3) env fenv
  | Apply (name, args) -> 
    let vals = List.map (fun e -> interp e env fenv) args in
    let received_count = List.length vals in
    (match lookup_fenv name fenv with
    | RecordInit (_, fields) -> 
      check_arity name (List.length fields) received_count false ;
      TupleV (List.map (fun v -> ref v) vals)
    | RecordGetter (_, pos) -> 
      check_arity name 1 received_count false ;
      get_elem (List.hd vals) (NumV (Int64.of_int pos))
    | DefFun (_, params, body) -> 
      check_arity name (List.length params) received_count false ;
      interp body (extend_env params vals env) fenv
    | DefSys (_, arg_types, ret_type) ->
      check_arity name (List.length arg_types) received_count false ;
      check_type ret_type @@ interp_sys name (List.map2 check_type arg_types vals))
  | Tuple exprs -> 
          TupleV (List.map (fun e -> (ref (interp e env fenv))) exprs)
  | Set (e,k,v) ->
          let t = (interp e env fenv) in
          let i = (interp k env fenv) in
          set_elem t i (interp v env fenv)
  | Lambda (params, body) -> ClosureV (List.length params,
    (fun vals -> 
      let env = extend_env params vals env in
      interp body env fenv
      )
    )
  | LamApply (fun_exp, args) ->
    let fun_val = interp fun_exp env fenv in
    (
      match fun_val with
      | ClosureV (arity, closure) -> 
        let vals = List.map (fun e -> interp e env fenv) args in
        let received_count = List.length vals in
        check_arity "closure" arity received_count true ;
        closure vals
      | _ -> raise_type_error "closure" fun_val
    )
  | LetRec (recs, body) -> 
    let env_box = ref [] in
    let names_n_closures = List.map (
      fun (name, params, body) ->
        name, ClosureV (List.length params, (fun vals -> 
          let env = extend_env params vals !env_box in
          interp body env fenv
          )
        )
    ) recs in
    let names, closures = List.split names_n_closures in
    let env = extend_env names closures env in
    env_box := env ;
    interp body env fenv
  | Empty -> failwith "Null Expression"
  

let interp_prog prog env =
  let defs, expr = prog in
  let fenv = defs_prelude @ defs in
  interp expr env fenv

let run (p : string) : value =
  interp_prog (parse_prog (sexp_from_string p)) []