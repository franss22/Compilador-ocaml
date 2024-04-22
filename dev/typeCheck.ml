open Ast
open Asm


(* type checks available *)
type expected_type =
  | CheckNumber
  | CheckBool
  | CheckTuple
  | CheckClosure

(* type error constants *)
let err_not_number_flag = 1
let err_not_boolean_flag = 2
let err_not_tuple_flag = 3
let err_not_closure_flag = 4
let err_arity_flag = 5
(* type check labels *)
let type_check_num_label = "type_check_num"
let type_check_bool_label = "type_check_bool"
let type_check_tuple_label = "type_check_tuple"
let type_check_closure_label = "type_check_closure"

(* type error labels *)
let err_not_number_label = "err_not_number"
let err_not_boolean_label = "err_not_boolean"
let err_not_tuple_label = "err_not_tuple"
let err_not_closure_label = "err_not_closure"
let err_arity_label = "err_arity"



let err_index_out_of_bounds_label = "err_index_out_of_bounds"

let pre_func = [
]
  
let post_func = [
  IRet
]

let check_index (tup : arg) (index : arg) = [
  IComment ("Checking if index is inside bounds") ;
  ISub (tup, LitConst 3) ;
  ISar (index, LitConst 1) ;
  ICmp (index, LitConst 0) ;
  IJl (err_index_out_of_bounds_label) ;
  ICmp (index, RegOffsetH (reg_of_arg tup, 0)) ;
  IJge (err_index_out_of_bounds_label)]

let type_check_base (label : string) (body : instruction list) : instruction list = 
  [ILabel (label)] @
  (* pre_func @ *)
  body @
  [IRet]

let type_check_num_body = [
  IComment ("Checking if type is Number"); 
  ITest (Reg RSI, Bool false) ; 
  IJnz (err_not_number_label) ;
]

let type_check_bool_body = [
  IComment ("Checking if type is Bool") ;  
  ITest (Reg RSI, Bool false) ; 
  IJz (err_not_boolean_label)
]

let type_check_tuple_body = [
  IComment ("Checking if type is Tuple") ; 
  IPush (Reg RAX) ;
  IMov (Reg RAX, Reg RSI);
  IAnd (Reg RAX, LitConst 7) ; (* mask 0b111 *)
  ICmp (Reg RAX, LitConst 3) ; 
  IPop RAX;
  IJne (err_not_tuple_label)
]

let type_check_closure_body = [
  IComment ("Checking if type is Closure") ; 
    IPush (Reg RAX) ;
  IMov (Reg RAX, Reg RSI);
  IAnd (Reg RAX, LitConst 7) ; (* mask 0b111 *)
  ICmp (Reg RAX, LitConst 5) ; 
  IPop RAX;
  IJne (err_not_closure_label)
]

let type_check_num = (type_check_base type_check_num_label type_check_num_body)
let type_check_bool = (type_check_base type_check_bool_label type_check_bool_body)
let type_check_tuple = (type_check_base type_check_tuple_label type_check_tuple_body)
let type_check_closure = (type_check_base type_check_closure_label type_check_closure_body)


let check_type (reg: arg) (expected: expected_type) : instruction list =
  (* The value is first moved to RSI since typeError takes its value argument in that register *)
  (* this way in case of an error the value will be ready and we won't need to know wether *)
  (* the value on RAX or R11 triggered the type error *)
  match expected with
  | CheckNumber -> [IPush (Reg RSI) ; IMov (Reg RSI, reg) ; ICall type_check_num_label ; IPop (RSI)]
  | CheckBool   -> [IPush (Reg RSI) ; IMov (Reg RSI, reg) ; ICall type_check_bool_label ; IPop (RSI)]
  | CheckTuple  -> [IPush (Reg RSI) ; IMov (Reg RSI, reg) ; ICall type_check_tuple_label ; IPop (RSI) ; IPush (Reg RSI) ] @ (check_index reg (Reg R11)) @ [IPop (RSI)]
  | CheckClosure  -> [IPush (Reg RSI) ; IMov (Reg RSI, reg) ; ICall type_check_closure_label ; IPop (RSI)]
  
let type_check_unop (op : prim1) : instruction list = 
  match op with
  | Add1 | Sub1 -> (check_type (Reg RAX) CheckNumber)
  | Not -> (check_type (Reg RAX) CheckBool)
  | _ -> []

let type_check_binop (op : prim2) : instruction list =
  match op with
  | Add | Sub | Mul | Div | Mod | Lte | Lt | Gte | Gt | Eq | Neq ->
    (check_type (Reg RAX) CheckNumber) @ (check_type (Reg R11) CheckNumber)
  | Xor -> (* And and Or use shortcuts so the type checking is made in a different way *)
    (check_type (Reg RAX) CheckBool) @ (check_type (Reg R11) CheckBool)
  | Get -> (check_type (Reg R11) CheckNumber) @ (check_type (Reg RAX) CheckTuple)
  | _ -> []

(* type error handlers *)

let err_not_number_handler = [
  ILabel (err_not_number_label) ;
  (* only the flag is passed here because the value is already on RSI *)
  IMov (Reg RDI, LitConst err_not_number_flag) ;
  ICall ("typeError")
]

let err_not_boolean_handler = [
  ILabel (err_not_boolean_label) ;
  IMov (Reg RDI, LitConst err_not_boolean_flag) ;
  ICall ("typeError")
]

let err_not_tuple_handler = [
  ILabel (err_not_tuple_label) ;
  IMov (Reg RDI, LitConst err_not_tuple_flag) ;
  ICall ("typeError")
]

let err_not_closure_handler = [
  ILabel (err_not_closure_label) ;
  IMov (Reg RDI, LitConst err_not_closure_flag) ;
  ICall ("typeError")
]

let err_arity_handler = [
  ILabel (err_arity_label) ;
  ICall ("arityError")
]

let err_index_out_of_bounds_handler = [
  ILabel (err_index_out_of_bounds_label) ;
  IPop (RDI) ;
  IMov (Reg RSI, Reg RAX) ;
  IAdd (Reg RSI, LitConst 3) ;
  IMov (Reg RDI, Reg R11) ;
  ICall ("indexError")
]


let type_check_handlers = type_check_num @ type_check_bool @ type_check_tuple @ type_check_closure
let index_error_handlers = err_index_out_of_bounds_handler @ err_arity_handler
let type_error_handlers = type_check_handlers @ err_not_number_handler @ err_not_boolean_handler @ err_not_tuple_handler @ err_not_closure_handler @ index_error_handlers 