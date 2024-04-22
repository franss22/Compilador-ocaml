open Ast
open Asm

(* this variable determines if the arithmetic checks will be generated *)
(* this should be set through an environment variable later *)
let safe = true

(**** Arithmetic errors ****)

(* constants to identify operations *)
let op_add_constant = 0
let op_sub_constant = 1
let op_mul_constant = 2
let op_div_constant = 3
let op_mod_constant = 4


(** overflow/underflow **)

(* labels *)
let err_overflow_add_label = "err_overflow_add"
let err_overflow_sub_label = "err_overflow_sub"
let err_overflow_mul_label = "err_overflow_mul"

(* mask used to check multiplication overflow *)
let sign_mask = 4611686018427387904L

(* since multiplication produces a result of twice the size, *)
(* overflow is checked manually comparing the sign of the factors *)
(* with the sign of the product. This part takes the xor of the *)
(* factors to know wether they have the same sign or not *)
let compile_overflow_preamble (op : prim2) : instruction list =
  match op with
  | Mul -> [IMov (Reg R10, Reg RAX) ; IXor (Reg R10, Reg R11)]
  | _ -> failwith "Expected Mul"

let compile_overflow_check (op : prim2) : instruction list =
  if safe then
    (match op with
    | Add -> [IJo (err_overflow_add_label)]
    | Sub -> [IJo (err_overflow_sub_label)]
    | Mul -> [
      IXor (Reg R10, Reg RAX) ; 
      IMov (Reg R11, Const sign_mask) ; 
      IAnd (Reg R10, Reg R11) ; 
      ICmp (Reg R10, Reg R11) ; 
      IJe (err_overflow_mul_label)]
    | _ -> failwith "Expected Add, Mul or Sub")
  else []

(* Add1 and Sub1 use the overflow checker of their corresponding binary operation *)
let compile_overflow_check_unary(op : prim1) : instruction list = 
  if safe then
    (match op with
    | Add1 -> compile_overflow_check Add
    | Sub1 -> compile_overflow_check Sub
    | _ -> failwith "Expected Add1 or Sub1")
  else []

(* overflow error instructions *)
let overflow_handler (label : string) (op_const : int) : instruction list = [
  ILabel (label) ;
  IMov (Reg RDI, LitConst op_const) ;
  ICall ("overflowError")
]

let add_overflow_handler = overflow_handler err_overflow_add_label op_add_constant
let sub_overflow_handler = overflow_handler err_overflow_sub_label op_sub_constant
let mul_overflow_handler = overflow_handler err_overflow_mul_label op_mul_constant

let overflow_handlers = add_overflow_handler @ sub_overflow_handler @ mul_overflow_handler

(** division by zero **)

(* labels *)
let div_by_zero_label = "division_by_zero"
let mod_by_zero_label = "modulo_by_zero"

let compile_zero_check (op : prim2) : instruction list =
  if safe then
    (match op with
    | Div -> [ ICmp (Reg R11, Const 0L) ; IJe (div_by_zero_label)]
    | Mod -> [ ICmp (Reg R11, Const 0L) ; IJe (mod_by_zero_label)]
    | _ -> failwith "Expected Div or Mod")
  else []

(* division by zero error instructions *)
let div_by_zero_handler (label : string) (op_const : int) : instruction list = [
  ILabel (label) ;
  IMov (Reg RDI, LitConst op_const) ; 
  ICall ("divByZeroError")
]

let div_by_zero = div_by_zero_handler div_by_zero_label op_div_constant
let mod_by_zero = div_by_zero_handler mod_by_zero_label op_mod_constant

let div_by_zero_handlers = div_by_zero @ mod_by_zero

(* the instructions for all the arithmetic errors are stored here *)
let arithmetic_safety_handlers = overflow_handlers @ div_by_zero_handlers
