open Asm
open Parse
open Printf
open Util
open List
open Fun
(* Variables can be local variables, or function parameters, which can be saved in a register or on the stack *)

type slot =
| LocalPos of int
| RegPos of reg
| StackPos of int
| ClosPos of int

type funType = 
| FunDeclared
| FunSys
| FunRecordInit
| FunRecordGetter of int

type regEnv = (string * slot ) list

type funEnv = (string * int * int * funType) list (*fname, arity, local_space, function type*)
  

let rec lookupFenv (name : string) (env : funEnv) : (int * int * funType) =
  match env with
  | [] -> raise (CTError (sprintf "Undefined function: %s" name))
  | (n, arity, lspace, ftype)::rest ->
    if n = name then (arity, lspace, ftype) else (lookupFenv name rest)

let addFenv (added: (string * int * int * funType))(env : funEnv) : funEnv =
  added::env

let rec lookupEnv (name : string) (env : regEnv) : slot =
  match env with
  | [] -> raise (CTError (sprintf "Identifier %s not found in environment" name))
  | (n, i)::rest ->
    if n = name then i else (lookupEnv name rest)

let rec countLocalPos (env : regEnv) : int =
  match env with
  | [] -> 0
  | (_, LocalPos _)::rest -> 1 + (countLocalPos rest)
  | _::rest -> countLocalPos rest

let addEnv (name : string) (env : regEnv) : (regEnv * int) =
  let slot = 1 + (countLocalPos env) in
  ((name, LocalPos slot)::env, slot)

(* Instruction for accessing a value in the stack *)
let local_access (slot: int) : arg = 
  RegOffset (RBP, slot)
(* Instruction for accessing a functoin argument saved in the stack *)
let stack_argument_access (slot:int) : arg =
  RegOffset (RBP, -(slot+1))
let lookupEnvLocal (name :string) (env : regEnv): int =
  let slot = lookupEnv name env in
  match slot with
  | LocalPos i -> i
  | _ -> raise (CTError (sprintf "%s is not a local variable" name))


(* Adds a function's arguments and adds it to the variable environment *)
let add_args_to_env (argnames: string list) (env : regEnv) : regEnv =
  let args_slots  (argnames : string list) : (string * slot) list =
    let regs = take param_regs (length argnames) in
    combine argnames (map (fun a ->RegPos a) regs) in
    
  let stack_slots (argnames : string list) : (string * slot) list =
    let n = length argnames in
    let rec hlp num = 
      match num with
      | 0 -> []
      | a -> (StackPos a)::hlp (a-1)
    in combine argnames (hlp n) in

  let pairs =
    if (length argnames) <= 6 
      then args_slots argnames
      else match argnames with 
      | a::b::c::d::e::f::rest -> (stack_slots rest) @ (args_slots [a;b;c;d;e;f])
      | _ -> failwith "Supposeddly unreachable error"
  in fold_left (fun env pr -> pr::env) env pairs

let rec build_lambda_env (freeVars: string list): regEnv =
  match freeVars with
    | (var::tail) -> 
      let sl = ClosPos (List.length tail) in
      (var, sl)::build_lambda_env tail
    | [] -> []

let getClosureValue (valPos:int) : instruction list =
  [IComment (sprintf "Getting free variable in pos %d from closure" valPos);
  IMov (Reg RAX, RegOffsetExact (R12, ((valPos+3)*8-5)))
  ]
  (* Get the closure-tuple pointer (first argument of a lambda func (RDI)) *)
  (* Get the nth (+3) value of the closure-tuple into RAX *)
  (* failwith "getclosureValue To Do" *)
  
