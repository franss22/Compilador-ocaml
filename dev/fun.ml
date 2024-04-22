open Ast
open Asm
(* open Printf *)
open List


let param_regs = [RDI; RSI; RDX; RCX; R8; R9]
(* %rdi​, ​%rsi​, ​%rdx​, ​%rcx​, ​%r8​, %r9 ​then push to stack last first *)


(* Given a list of compiled expressions, saves each of them in a different register (for the first 6 int or pointer arguments of a function)*)
let save_in_reg : instruction list list -> instruction list =
  fun params ->
    let rec f : instruction list list -> reg list -> instruction list -> instruction list = 
      fun params regs ret ->
        match params with
        | [] -> ret
        | p::params_rest -> 
          (match regs with 
          | [] -> failwith "More parameters than registers in save_in_reg"
          | rg::regs_rest ->(f params_rest regs_rest (p @ [IMov (Reg rg, Reg RAX)] @ ret))
          )
    in f params param_regs []

let rec save_in_stack : instruction list list -> instruction list = 
  fun params ->
    match params with 
    | [] -> []
    | p::rest -> (save_in_stack rest) @ p @ [IPush (Reg RAX)]

let alineate_stack_start : instruction list list -> instruction list =
  fun ls ->
    let len = length ls in
    if len mod 2 == 0
      then []
      else [ISub (Reg RSP, LitConst 8)]

let alineate_stack_end : instruction list list -> instruction list =
  fun args ->
    let arity = length args in
    let needs_alignment = ((arity > 6) && ((arity mod 2) > 0)) in
    if needs_alignment
      then [IAdd (Reg RSP, LitConst 8)]
      else []

let save_params : instruction list list -> instruction list = 
  fun params ->
    if (length params) <= 6 
      then save_in_reg params
      else match params with 
      | a::b::c::d::e::f::rest -> (alineate_stack_start rest) @ (save_in_stack (rev rest)) @ (save_in_reg [a;b;c;d;e;f])
      | _ -> failwith "Supposeddly unreachable error"

(* Round a number to its next multiple of 2 *)
let round_to_2 : int -> int =
  fun x ->
  if (x mod 2) == 0 
    then x
    else x+1
    
(* Counts the maximum amount of concurrent local variables in the code. expects normalized expr *)
let count_local_space : expr -> int =
  fun expr ->
    let rec f : expr -> int -> int = 
      fun expr i ->
        match expr with
          | Num _ | Bool _ | Id _ | Lambda  (_, _) -> i
          | Tuple   (exprs) -> let h (e : expr) : int = (f e i) in ((List.length exprs) + (List.fold_left max 0 (map h exprs)))
          | Set   (t, p, v) -> (max (f t i) (max (f p i) (f v i)))
          | Prim1   (_, a1) -> f a1 i
          | Apply    (_, _) | LamApply (_, _) -> i (* fold_left (max) i (map (fun a -> f a i) exprs) *)
          | LetRec   (recs, body) -> let l = length recs in (f body (i+l))
          | Prim2 (_, a1, a2) -> (max (f a1 i) (f a2 i))
          | Let (_, value, body) -> (max (f value i)(f body (i+1)))
          | If (a1, a2, a3) -> (max (f a1 i) (max (f a2 i)(f a3 i)))
          | Empty -> failwith "Null Expression count_local_space"

    in round_to_2 (f expr 1)


let comp_call_func (fname: string) (cargs: instruction list list)  : instruction list =
  let prep_args = save_params (rev cargs) in
  let alignment = alineate_stack_end cargs in
  prep_args @ [ICall fname] @ alignment
