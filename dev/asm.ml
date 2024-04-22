open Printf
open Int64
(* registers *)
type reg = 
| RAX (* Return value; caller *)
| RCX (* 4th parameter; caller *)
| RDX (* 3rd parameter; caller *)
| RBX (* call-ee *)
| RDI (* 1st parameter; caller *)
| RSI (* 2nd parameter; caller *)
| RSP (* Stack Pointer; caller *)
| RBP (* Base Pointer; call-ee *)
| R8  (* 5th parameters; caller *)
| R9  (* 6th parameters; caller *)
| R10 (* caller *)
| R11 (* caller *)
| R12 (* call-ee *) (* Closure pointer *)
| R13 (* call-ee *)
| R14 (* call-ee *)
| R15 (* call-ee *)

let pp_reg reg : string =
  match reg with
  | RAX -> "RAX"
  | RCX -> "RCX"
  | RDX -> "RDX"
  | RBX -> "RBX"
  | RDI -> "RDI"
  | RSI -> "RSI"
  | RSP -> "RSP"
  | RBP -> "RBP"
  | R8  -> "R8"
  | R9  -> "R9"
  | R10 -> "R10"
  | R11 -> "R11"
  | R12 -> "R12"
  | R13 -> "R13"
  | R14 -> "R14"
  | R15 -> "R15"

let true64  = "0x8000000000000001"
let false64 = "0x0000000000000001"

(* arguments for instructions *)
type arg =
| Const of int64          (* Number, represented with a 0 in LSB*)
| Bool of bool            (* Bool, represented with a 1 in LSB*)
| LitConst of int         (* Literal Value that doesnt get multiplied in compiling (Const 1) would get compiled to 0x2, (LitConst 1) to 0x1*)
| LitConst64 of int64         (* Literal 64 bit Value that doesnt get multiplied in compiling *)
| LabelArg of string      (* Label *)
| Reg of reg              (* Registry *)
| RegOffsetExact of reg * int (*exact memory acces*)
| RegOffset of reg * int  (* Stack memory access *)
| RegOffsetH of reg * int  (* Heap memory access *)
| RegOffsetReg of reg * reg  (* Stack memory access *)

let reg_of_arg (reg : arg) : reg =
  match reg with
  | Reg r -> r
  | _ -> failwith "Expected Reg"

(* asm instructions *)
type instruction =
| IRet
| IMov of arg * arg
| IAdd of arg * arg
| ISub of arg * arg
| IMul of arg * arg
| IDiv of arg
| IDec of arg
| IInc of arg
| IAnd of arg * arg
| ILte of arg * arg
| IOr of arg * arg
| ICmp of arg * arg
| IXor of arg * arg
| ISar of arg * arg
| ISal of arg * arg
| IPush of arg
| IPop of reg
| IJmp of string
| IJe of string
| IJne of string
| IJl of string
| IJle of string
| IJg of string
| IJge of string
| IJnz of string
| IJz of string
| IJo of string
| ILabel of string
| IRegCall of arg
| ICall of string
| ITest of arg * arg
| IComment of string
(* | IPrint of arg *)


(* TO BE COMPLETED *)



let pp_arg arg : string =
  match arg with
  | Const n -> sprintf "%#Lx" (mul n 2L)
  | LitConst n -> sprintf "%d" n
  | LitConst64 n -> sprintf "%#Lx" n
  | Bool b -> (match b with | true -> true64 | false -> false64 )
  | LabelArg s -> s
  | Reg r -> pp_reg r
  | RegOffset (reg, offset) -> sprintf "[ %s - 8*%d]" (pp_reg reg) offset
  | RegOffsetH (reg, offset) -> sprintf "[ %s + 8*%d]" (pp_reg reg) offset
  | RegOffsetReg (reg1, reg2) -> sprintf "[ %s + 8*%s ]" (pp_reg reg1) (pp_reg reg2)
  | RegOffsetExact (reg, offset) -> 
    if offset < 0 
      then sprintf "[ %s - %d]" (pp_reg reg) (-offset)
      else sprintf "[ %s + %d]" (pp_reg reg) offset
  (* | Tuple t ->  *)

let pp_instr instr : string =
  match instr with
  | IRet -> "  ret" 
  | IMov (a1, a2) -> sprintf "  mov %s, %s" (pp_arg a1) (pp_arg a2)
  | IInc (a1)     -> sprintf "  add %s, 0x2" (pp_arg a1)
  | IDec (a1)     -> sprintf "  sub %s, 0x2" (pp_arg a1)
  | IPop (a1)     -> sprintf "  pop %s" (pp_reg a1)
  | IAdd (a1, a2) -> sprintf "  add %s, %s" (pp_arg a1) (pp_arg a2)
  | ISub (a1, a2) -> sprintf "  sub %s, %s" (pp_arg a1) (pp_arg a2)
  | IMul (a1, a2) -> sprintf "  imul %s, %s" (pp_arg a1) (pp_arg a2)
  | IDiv (a1)     -> sprintf "  idiv %s" (pp_arg a1)
  | IAnd (a1, a2) -> sprintf "  and %s, %s" (pp_arg a1) (pp_arg a2)
  | IOr  (a1, a2) -> sprintf "  or  %s, %s" (pp_arg a1) (pp_arg a2)
  | IXor (a1, a2) -> sprintf "  xor %s, %s" (pp_arg a1) (pp_arg a2)
  | ICmp (a1, a2) -> sprintf "  cmp %s, %s" (pp_arg a1) (pp_arg a2)
  | ISar (a1, a2) -> sprintf "  sar %s, %s" (pp_arg a1) (pp_arg a2)
  | ISal (a1, a2) -> sprintf "  sal %s, %s" (pp_arg a1) (pp_arg a2)
  | ILte (a1, a2) -> sprintf "  lte %s %s" (pp_arg a1) (pp_arg a2)
  | ITest (a1, a2)-> sprintf "  test %s, %s" (pp_arg a1) (pp_arg a2)
  | IPush (a1)    -> sprintf "  push %s" (pp_arg a1)
  | IJmp (a1)     -> sprintf "  jmp %s" a1
  | IJe  (a1)     -> sprintf "  je  %s" a1
  | IJne (a1)     -> sprintf "  jne %s" a1
  | IJl  (a1)     -> sprintf "  jl  %s" a1
  | IJle (a1)     -> sprintf "  jle %s" a1
  | IJg  (a1)     -> sprintf "  jg  %s" a1
  | IJge (a1)     -> sprintf "  jge %s" a1
  | IJnz (a1)     -> sprintf "  jnz %s" a1
  | IJz  (a1)     -> sprintf "  jz  %s" a1
  | IJo  (a1)     -> sprintf "  jo  %s" a1
  | ILabel (a1)   -> sprintf "%s:" a1
  | ICall (a1)    -> sprintf "  call %s" a1
  | IRegCall (a1) -> sprintf "  call %s" (pp_arg a1)
  | IComment (a1) -> sprintf "  ; %s" a1
  (* | IPrint (a1) *)
  (* _ -> "Error" *)



let pp_instrs (instrs : instruction list) : string =
  List.fold_left (fun res i -> res ^ "\n" ^ (pp_instr i)) "" instrs
