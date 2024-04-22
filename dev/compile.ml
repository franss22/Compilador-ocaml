open Ast
open Asm
open Printf
open Anf
open List
open Fun
open TypeCheck
open SafetyCheck
open Env
open Lambda
let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L
let not_mask : int64 = -4611686018427387904L
let heap_align_mask = -8L

type tag = int

let call_print_stack : instruction list =
  [IMov (Reg RDI, Reg RBP);
  IMov (Reg RSI, Reg RSP);
  ICall "print_stack";
  ]

(* Tags the expression, giving each nested expression a different integer tag *)
let rec tag (e : expr) (cur : tag) : (tagExpr * tag) =
  match e with
  | Num n   -> (TNum(n, cur), (cur + 1))
  | Bool b  -> (TBool(b, cur), (cur + 1))
  | Id name -> (TId(name, cur), (cur + 1))
  | Prim1(op, e) -> 
    let (tag_e, next_tag) = tag e (cur + 1) in
    (TPrim1(op, tag_e, cur), next_tag)
  | Prim2 (op, e1, e2) -> (*With ANF we can be sure e1 and e2 are imm values*)
    let (tag_e1, next_tag) = tag e1 (cur + 1) in
    let (tag_e2, next_tag) = tag e2 (next_tag + 1) in
    (TPrim2(op, tag_e1, tag_e2, cur), next_tag) 
  | Let(id, value, body) ->
    let (tag_val, next_tag) = tag value (cur + 1) in
    let (tag_body, next_tag) = tag body (next_tag + 1) in
    (TLet(id, tag_val, tag_body, cur), next_tag) 
  | If (cond, btrue, bfalse) ->
    let (tag_cond, next_tag) = tag cond (cur + 1) in
    let (tag_btrue, next_tag) = tag btrue (next_tag + 1) in
    let (tag_bfalse, next_tag) = tag bfalse (next_tag + 1) in
    (TIf(tag_cond, tag_btrue, tag_bfalse, cur), next_tag)
  | Apply (fname, args) -> 
    let targs, next_tag = tag_list args (cur+1) in
    (TApply (fname, targs, cur), next_tag)
  | Tuple exprs -> 
    let texprs, next_tag = tag_list exprs (cur+1) in
    (TTuple (texprs, cur), next_tag)
  | Set (tup, pos, value) -> 
    let (ttup, next_tag) = tag tup (cur + 1) in 
    let (tpos, next_tag) = tag pos (next_tag + 1) in
    let (tval, next_tag) = tag value (next_tag + 1) in
    (TSet (ttup, tpos, tval, cur), next_tag)
  | Lambda (args, body)   ->
    let tbody, next_tag = tag body (cur+1) in
    (TLambda (args, tbody, cur, (count_local_space body)), next_tag)
  | LamApply (_, args) -> 
    let targs, next_tag = tag_list args (cur+1) in
    (TLamApply (TEmpty, targs, cur), next_tag)
  | LetRec (recs, body)   -> 
    let trecs, next_tag = tag_recs recs (cur+1) in
    let tbody, next_tag2 = tag body (next_tag+1) in
    (TLetRec (trecs, tbody, cur), next_tag2)
  | Empty -> failwith "Null Expression Tag"
and tag_list (le : expr list) (cur: tag): (tagExpr list * tag) =
  match le with
  | [] -> ([], cur)
  | ex::rest -> 
    let (texp, nt1) = tag ex cur in 
    let (trest, nt2) = tag_list rest nt1 in 
    (texp::trest, nt2)
and tag_recs (recs : (string * string list * expr) list) (cur: tag) =
  match recs with
  | [] -> ([], cur)
  | (recname, args, body)::rest -> 
    let (tbody, nt1) = tag body cur in 
    let (trest, nt2) = tag_recs rest nt1 in 
    ((recname, args, tbody)::trest, nt2)

let garbage_collection (needed_space :int):(instruction list) =
  [
    ISub (Reg RSP, LitConst 8);
    IPush (Reg R12);
    IMov (Reg RDI, Reg R15);
    IMov (Reg RSI, LitConst needed_space);
    IMov (Reg RDX, Reg RBP);
    IMov (Reg RCX, Reg RSP);
    ICall "try_gc";
    IPop R12;
    IAdd (Reg RSP, LitConst 8);
    IMov (Reg R15, Reg RAX);
  ]


let compile_unop (op : prim1) : instruction list =
  (type_check_unop op) @
  (match op with
  | Add1 -> [IInc (Reg RAX)] @ (compile_overflow_check_unary op)
  | Sub1 -> [IDec (Reg RAX)] @ (compile_overflow_check_unary op)
  | Not  -> [IMov (Reg R11, Const not_mask); IXor (Reg RAX, Reg R11)]
  | Print -> [IMov (Reg RDI, Reg RAX) ; ICall ("print")])

let compile_comparison (op : prim2) (label: string) : instruction list = [
  ICmp (Reg RAX, Reg R11);
  IMov (Reg RAX, Bool true);
  (match op with
  | Lte -> IJle label
  | Lt  -> IJl label
  | Gte -> IJge label
  | Gt  -> IJg label
  | Eq  -> IJe label
  | Neq | Xor -> IJne label
  | _ -> failwith "Not a comparison");
  IMov (Reg RAX, Bool false);
  ILabel label]

let compile_get (pos:arg) : instruction list = (*Gets the pos of the tuple in RAX *)
  [IComment "Getting the field in pos RDI from the tuple RAX and saving it in RAX"; 
  IPush (Reg RDI) ; 
  IMov (Reg RDI, pos) ; 
  IAdd (Reg RDI, LitConst 1) ; 
  IMov (Reg RAX, RegOffsetReg (RAX, RDI)) ; 
  IPop (RDI)]

let compile_binop (op : prim2) (tag : tag) : instruction list =
  (type_check_binop op) @
  (match op with
  | Add -> [IAdd (Reg RAX, Reg R11)] @ (compile_overflow_check op)
  | Sub -> [ISub (Reg RAX, Reg R11)] @ (compile_overflow_check op)
  | Mul -> (compile_overflow_preamble op) @ [IMul (Reg RAX, Reg R11); ISar (Reg RAX, LitConst 1)] @ (compile_overflow_check op)
  | Div -> (compile_zero_check op) @ [IMov (Reg RDX, LitConst 0) ; IDiv (Reg R11) ; ISal (Reg RAX, LitConst 1)] 
  | Mod -> (compile_zero_check op) @ [IMov (Reg RDX, LitConst 0) ; IDiv (Reg R11) ; IMov (Reg RAX, Reg RDX)] 
  | Lte -> (compile_comparison op (sprintf "less_or_equal_%d" tag))
  | Lt  -> (compile_comparison op (sprintf "less_%d" tag))
  | Gte -> (compile_comparison op (sprintf "greater_or_equal_%d" tag))
  | Gt  -> (compile_comparison op (sprintf "greater_%d" tag))
  | Eq  -> (compile_comparison op (sprintf "equal_%d" tag))
  | Neq -> (compile_comparison op (sprintf "not_equal_%d" tag))
  | Xor -> (compile_comparison op (sprintf "xor_%d" tag))
  | And | Or -> failwith "And and OR should not be in ANF"
  | Get -> compile_get (Reg R11)
  )

let compile_boolop_shortcut_comp (op: prim2) : instruction list =
  (check_type (Reg RAX) CheckBool) @
  (match op with
  | And -> [ICmp (Reg RAX, Bool false)] (* if RAX = false, the and expr is false, return RAX*)
  | Or  -> [  (*set R11 to true because its a 64 bti literal and cannot be used in cmp directly*)
    IMov (Reg R11, Bool true); ICmp (Reg RAX, Reg R11);]
  | _ -> failwith "Only And and Or have evaluation shortcuts")

let get_bool_shortcut_label (op : prim2) (tag : tag) : string =
  match op with 
  | And -> sprintf "false_and_shortcut_%d" tag
  | Or -> sprintf "true_or_shortcut_%d" tag
  | _ -> failwith "Expected And or Or operation"

(* let rec compile_closure_closed_variables (freevarIds: string list) (recFuncsLabels : string list) (env : regEnv) (i : int) : instruction list =  *)
let rec compile_closure_closed_variables (freevarIds: string list) (env : regEnv) (i : int) : instruction list = 
  match freevarIds with
  | argname::rest -> 
    (* let comp_exprs = (map (fun argname -> RegOffset (RBP, (lookupEnvLocal argname lambda_env))) argnames) in *)
    let valSlot = lookupEnv argname env in
    let valAccess = 
      (match valSlot with
      | LocalPos slot -> [IComment (sprintf "Id local val: %s" argname);IMov (Reg RAX, local_access slot)]
      | StackPos slot -> [IComment (sprintf "Id stack arg: %s" argname);IMov (Reg RAX, stack_argument_access slot)]
      | RegPos r      -> [IComment (sprintf "Id reg varg: %s" argname);IMov (Reg RAX, Reg r)]
      | ClosPos slot  -> [IComment (sprintf "Id free var: %s" argname)] @ getClosureValue slot
      ) in
    [IComment (sprintf "retrieving element number %i for tuple" i)] @ valAccess @
    [IComment (sprintf "saving element number %i in tuple" i) ; IMov (RegOffsetH (R15, 3 + i), Reg RAX)] @
    (compile_closure_closed_variables rest env (i + 1))
    (* (compile_closure_closed_variables rest recFuncsLabels env (i + 1)) *)
  | [] -> []


(* let rec compile_rec_closure_funcs (labels : string list) (i : int) =
  match labels with
  | s::rest -> [
    IMov (Reg RAX, RegOffset (RBP, i)) ; 
    (* IMov (Reg RAX, LabelArg s) ;  *)
    IAdd (Reg RAX, LitConst 5) ; 
    IMov (RegOffsetH (R15, 4+i), Reg RAX) ] @
    (compile_rec_closure_funcs rest (i+1))
  | [] -> [] *)

(* let rec compile_closure_tuple (arity:int) (label: string) (freevarIds: string list) : instruction list = *)
let compile_closure_tuple (arity:int) (label: string) (freevarIds: string list) (*(recFuncsLabels : string list)*) (env : regEnv) : instruction list =
  
  garbage_collection (3 + (List.length freevarIds)) @
  [
  
  IMov (Reg RAX, LitConst arity) ; 
  IMov (RegOffsetH (R15, 0), Reg RAX) ; 
  IMov (Reg RAX, LabelArg label) ;
  IMov (RegOffsetH (R15, 1), Reg RAX) ; 
  IMov (Reg RAX, LitConst (List.length freevarIds)) ; 
  IMov (RegOffsetH (R15, 2), Reg RAX) ;] @
  (* IMov (Reg RAX, LitConst (List.length recFuncsLabels)) ; 
  IMov (RegOffsetH (R15, 3), Reg RAX) ;] @ *)
  (* (compile_rec_closure_funcs recFuncsLabels 0) @ *)
  (* (compile_closure_closed_variables freevarIds recFuncsLabels env (List.length recFuncsLabels)) @ *)
  (compile_closure_closed_variables freevarIds env 0) @
  [IMov (Reg RAX, Reg R15) ; IAdd (Reg RAX, LitConst 5); IAdd (Reg R15, LitConst ((8 * 3) + (8 * (List.length freevarIds)))) ;]

let rec compile_tuple_elements (comp_exprs : arg list)  (i : int) : instruction list =
  match comp_exprs with
  | elem::rest -> 
    (* let offsetElem = RegOffset (RBP, elem) in *)
    [IComment (sprintf "retrieving element number %i for tuple" i) ; IMov (Reg RAX, elem)] @
    [IComment (sprintf "saving element number %i in tuple" i) ; IMov (RegOffsetH (R15, i + 1), Reg RAX)] @
    (compile_tuple_elements rest (i + 1))
  | [] -> [
    IComment "Saving tuple";
    IMov (Reg RAX, LitConst i) ;
    IMov (RegOffsetH (R15, 0), Reg RAX) ; 
    IMov (Reg RAX, Reg R15) ; 
    IAdd (Reg RAX, LitConst64 3L) ; 
    IAdd (Reg R15, LitConst (8 * (i + 1)))]

(* Makes an environment with the name, arity and local space needed for each function defined *)
let rec build_fenv (fundefs : fundef list) (fenv : funEnv): funEnv =
  match fundefs with
  | [] -> []
  | fundef::rest -> 
      let inner_fenv = build_fenv rest fenv in
      let func = (match fundef with
      | DefFun (fname, argnames, body) -> (fname, length argnames, count_local_space body, FunDeclared)
      | DefSys (fname, argtypes, _) -> (fname, length argtypes, 1, FunSys)
      | RecordGetter (name, pos) -> (name, 1, 1, FunRecordGetter pos)
      | RecordInit (name, fields) -> (name, length fields, 1, FunRecordInit)
      ) in
      addFenv func inner_fenv

let wrap_lambda (compiled_lambda : instruction list) (name : string) (tag: int): instruction list =
  let end_label  = sprintf "__lambda_end_%s_%d" name tag in
  [IJmp end_label;] @ compiled_lambda @ [ILabel end_label]



let rec compile_expr (e : tagExpr) (env : regEnv) (fenv : funEnv): instruction list =
  match e with 
  | TNum (n, _) -> 
    if n > max_int || n < min_int 
      then failwith ("Integer overflow: " ^ (Int64.to_string n))
      else [ IMov (Reg RAX, Const n) ] 
  | TBool (b, _) -> [ IMov (Reg RAX, Bool b)]
  | TId (name, _) -> 
    let pos = lookupEnv name env in
    (match pos with
    | LocalPos slot -> [IComment (sprintf "Id local val: %s" name);IMov (Reg RAX, local_access slot)]
    | StackPos slot -> [IComment (sprintf "Id stack arg: %s" name);IMov (Reg RAX, stack_argument_access slot)]
    | RegPos r      -> [IComment (sprintf "Id reg varg: %s" name);IMov (Reg RAX, Reg r)]
    | ClosPos slot  -> [IComment (sprintf "Id free var: %s" name)] @ getClosureValue slot
    )  
  | TPrim1 (op, e, _) -> (*With ANF we can be sure e is an imm value*)
    (compile_expr e env fenv) @ (compile_unop op)
  | TPrim2 (op, e1, e2, tag) -> 
    (match op with 
    | And | Or  -> 
      (compile_expr e1 env fenv) @ 
      (compile_boolop_shortcut_comp op) @ 
      (let label = (get_bool_shortcut_label op tag) in
        [IJe label;] @ 
        (compile_expr e2 env fenv) @ 
        (check_type (Reg RAX) CheckBool) @ 
        [ILabel label])
    | _ -> (*the rest of binary operations are in ANF *)
      let offset1 = RegOffset (RBP, lookupEnvLocal "__arg1" env) in (* thanks to ANF we can be sure __arg1 and _arg2 are local variables *)
      let offset2 = RegOffset (RBP, lookupEnvLocal "__arg2" env) in
        (*With ANF we can be sure e1 and e2 are imm values, and move them directly from the stack into the registers*)
        [IComment "Binary operation in ANF"; IMov (Reg RAX, offset1); IMov (Reg R11, offset2)] @ (compile_binop op tag))
  | TLet(id, value, body, _) ->
    let (env', slot) = addEnv id env in
      (* Compile the binding, and get the result into RAX *)
      (compile_expr value env fenv)
      (* Copy the result in RAX into the appropriate stack slot *)
    @ [IComment (sprintf "let %s = RAX" id); IMov(RegOffset(RBP, slot), Reg(RAX)) ]
      (* Compile the body, given that x is in the correct slot when it's needed *)
    @ (compile_expr body env' fenv)
  | TIf (cond, btrue, bfalse, tag) ->
    let else_label = sprintf "if_false_%d" tag in
    let done_label = sprintf "done_%d" tag in
    (compile_expr cond env fenv) @
    (check_type (Reg RAX) CheckBool) @
    [
      ICmp(Reg(RAX), Bool(false));
      IJe(else_label)
    ]
    @ (compile_expr btrue env fenv)
    @ [ IJmp(done_label); ILabel(else_label) ]
    @ (compile_expr bfalse env fenv)
    @ [ ILabel(done_label) ]
  | TApply (fname, args, _) ->
    let arity, _, ftype = lookupFenv fname fenv in
    if (length args) != arity
      then failwith (sprintf "Arity mismatch: %s expected %d arguments but got %d" fname arity (length args))
      else 
        let comp_args = (map (fun e -> compile_expr e env fenv) args) in
        (match ftype with 
        | FunDeclared -> comp_call_func fname comp_args
        | FunSys -> failwith "System functions not implemented"
        | FunRecordGetter pos -> 
          [IComment (sprintf "Record getter: %s (pos %i)" fname pos)] @
          (hd comp_args) @ (*El unico argumento debiera ser una tupla*)
          [IMov (Reg R11, Const (Int64.of_int pos))] @
          (type_check_binop Get) @
          compile_get (Reg R11)
        | FunRecordInit   -> 
          let comp_args = (map (fun e -> RegOffset (RBP, (match e with | TId (elem, _) -> (lookupEnvLocal elem env) | _ -> failwith "Expected TId"))) args) in
          (compile_tuple_elements (rev comp_args) 0)
        )
  | TTuple (exprs, _) -> 
    let comp_exprs = 
      (map 
      (fun e -> 
        RegOffset (RBP, 
        (match e with 
        | TId (elem, _) -> (lookupEnvLocal elem env) 
        | _ -> failwith "Expected TId"))) 
      exprs) in
    (* let comp_exprs = (map (fun e -> compile_expr e env fenv) exprs) in *)
    garbage_collection (1 + (List.length exprs)) @

    (compile_tuple_elements comp_exprs 0) 
  | TSet (_, _, _, _) -> (*values are compiled because of ANF*)
        call_print_stack @
        
    let offsetTup = RegOffset (RBP, lookupEnvLocal "__tup" env) in
    let offsetPos = RegOffset (RBP, lookupEnvLocal "__pos" env) in
    let offsetVal = RegOffset (RBP, lookupEnvLocal "__val" env) in
    [IComment "Retrieving and checking tuple position"; IMov (Reg R11, offsetPos)] @ (check_type (Reg R11) CheckNumber) @
    [IComment "Retrieving and checking tuple"; IMov (Reg RAX, offsetTup)] @ (check_type (Reg RAX) CheckTuple) @
    [IComment "Retrieving value"; IMov (Reg R10, offsetVal)] @
    [IComment "Setting the field in pos RDI from the tuple RAX to value R11"] @
    [IMov (Reg RDI, Reg R11) ; IAdd (Reg RDI, LitConst 1) ; IMov (RegOffsetReg (RAX, RDI), Reg R10) ] @
    [IComment "Retagging tuple" ; IAdd (Reg RAX, LitConst 3)]
  | TLambda   (argnames, body, tag, lspace) -> 
    let freeVariables = tfreeVars e in
    let func_label = sprintf "__lambda_fun_%d" tag in
    let lambda_env = build_lambda_env freeVariables in
    let lambda_env = add_args_to_env (["__closure"] @ argnames) lambda_env in
    (* let comp_exprs = (map (fun argname -> RegOffset (RBP, (lookupEnvLocal argname lambda_env))) argnames) in *)
    let compiled_closure = compile_closure_tuple (length argnames) func_label freeVariables (*[]*) env in
    let compiled_body = compile_lambda_definition body lambda_env fenv lspace func_label in 
    let compiled_lambda = wrap_lambda compiled_body "" tag in
    compiled_lambda @ compiled_closure
  | TLamApply (_, args_expr, _) -> (*Lambda expr is already compiled because of ANF*)
  (* closure is in __closure in env *)
    let offsetClos = RegOffset (RBP, lookupEnvLocal "__closure" env) in
    let closure_to_RAX = [IMov (Reg RAX, offsetClos)] in
    let comp_args = (map (fun e -> compile_expr e env fenv) (rev args_expr)) in
    let arg_amt = length args_expr in
    let lambda_type_check = check_type (Reg RAX) CheckClosure in
    let lambda_arity_check = 
      [IPush (Reg RDI) ;
      IPush (Reg RSI) ;
      IMov (Reg RDI, RegOffsetExact (RAX, -5)) ;
      IMov (Reg RSI, LitConst arg_amt);
      ICmp (Reg RDI, Reg RSI) ;
      IJne err_arity_label ; (*alignment error?*)
      IPop RSI ;
      IPop (RDI)] in
    let args_instr = [[IComment "Closure pointer is already in RAX"]] @ comp_args in
    closure_to_RAX @
    lambda_type_check @
    lambda_arity_check @
    (save_params args_instr) @
    closure_to_RAX @
    [IMov (Reg RDI, Reg RAX);
      
    IMov (Reg RAX, RegOffsetExact (RAX, (8-5)));
      
    IRegCall (Reg RAX)] @
    (alineate_stack_end args_instr)
  | TLetRec (_(*trecs*), _(*tbody*), _(*tag*)) -> 
    failwith "let rec not implemented"
    (* let recFuncs = (List.map tfreeVars (List.map (fun (n, p, b) -> b) trecs)) in *)
    (* let recFuncs = tfreeVars e in *)
    (* let _ = print_endline (List.fold_left (fun a b -> a ^ b) "" recFuncs) in *)
    (* let recFuncEnv = build_lambda_env (List.fold_left (fun a b -> a @ b) [] recFuncs) in *)
    (* let recFuncEnv = build_lambda_env recFuncs in
    let recFuncLabels = (List.map (fun name -> sprintf "__lambda_fun_%s_%d" name tag) recFuncs) in
    let rec compile_rec_lambdas (rec_lambdas : (string * string list * tagExpr) list) (i : int) =
      (match rec_lambdas with
      | elem::rest -> (
        match elem with
        | (name, params, b) -> 
          let freeVariables = (Util.remove_duplicates ((tfreeVars (TLambda (params, b, tag, 0))) @ recFuncs)) in
          let _ = print_endline (List.fold_left (fun a b -> a ^ b) "" freeVariables) in
          let func_label = sprintf "__lambda_fun_%s_%d" name tag in *)
          (* let lambda_env = add_args_to_env freeVariables recFuncEnv in *)
          (* let lambda_env = build_lambda_env freeVariables in
          let lambda_env = add_args_to_env params lambda_env in *)
          (* let _ = print_endline "a" in *)
          (* let comp_exprs = (map (fun argname -> RegOffset (RBP, (lookupEnvLocal argname lambda_env))) argnames) in *)
          (* let compiled_closure = compile_closure_tuple (length params) func_label freeVariables recFuncLabels lambda_env in *)
          (* let _ = print_endline "b" in *)
          (* let compiled_body = compile_lambda_definition b lambda_env fenv 0 func_label in  *)
          (* let _ = print_endline "c" in *)
          (* let compiled_lambda = wrap_lambda compiled_body name tag in
          compiled_lambda @ compiled_closure @ (compile_rec_lambdas rest (i+1))
        | _ -> failwith "a")
      | [] -> []) in
      let extendedLambdas = (compile_rec_lambdas trecs 0) in *)
    (* let extendedLambdas = (List.fold_left (fun a b -> a @ b) [] (List.map 
      (fun (name, params, b) -> (
        let freeVariables = (Util.remove_duplicates ((tfreeVars (TLambda (params, b, tag, 0))) @ recFuncs)) in
        let _ = print_endline (List.fold_left (fun a b -> a ^ b) "" freeVariables) in
        let func_label = sprintf "__lambda_fun_%s_%d" name tag in
        (* let lambda_env = add_args_to_env freeVariables recFuncEnv in *)
        let lambda_env = build_lambda_env freeVariables in
        let lambda_env = add_args_to_env params lambda_env in
        let _ = print_endline "a" in
        (* let comp_exprs = (map (fun argname -> RegOffset (RBP, (lookupEnvLocal argname lambda_env))) argnames) in *)
        let compiled_closure = compile_closure_tuple (length params) func_label freeVariables recFuncLabels lambda_env in
        let _ = print_endline "b" in
        let compiled_body = compile_lambda_definition b lambda_env fenv 0 func_label in 
        let _ = print_endline "c" in
        let compiled_lambda = wrap_lambda compiled_body name tag in
        compiled_lambda @ compiled_closure @ []
        (* [IAdd (Reg RAX, LitConst 5)] *)
      )) trecs)) in  *)
      (* let _ = print_endline extendedLambdas in *)
      (* extendedLambdas @ (compile_expr tbody recFuncEnv fenv) *)


  | TEmpty -> failwith "Null Expression Compile"

(* compiles a function definition, leaving enough space for its local variables, making a label, and managing the RSP and RBP registers *)
and compile_function_definition (e : tagExpr) (env : regEnv) (fenv : funEnv) (lspace : int) (fname : string) : instruction list = 
  [
  ILabel fname;
  IPush (Reg RBP);
  IMov (Reg RBP, Reg RSP);
  ISub (Reg RSP, LitConst (8 * lspace));
  IPush (Reg R12);
  ISub (Reg RSP, LitConst 8);] @
  compile_expr e env fenv @
  [
  IAdd (Reg RSP, LitConst 8);
  IPop R12;
  IMov (Reg RSP, Reg RBP);   
  IPop RBP;
  IRet]
and compile_lambda_definition (e : tagExpr) (env : regEnv) (fenv : funEnv) (lspace : int) (fname : string) : instruction list = 
  [
  ILabel fname;
  IPush (Reg RBP);
  IMov (Reg RBP, Reg RSP);
  ISub (Reg RSP, LitConst (8 * lspace));
  IPush (Reg R12);
  ISub (Reg RSP, LitConst 8);] @
  [IMov (Reg R12, Reg RDI)] @ (* Closure is in RDI, gets saved in R12*)
  compile_expr e env fenv @
  [
  IAdd (Reg RSP, LitConst 8);
  IPop R12;
  IMov (Reg RSP, Reg RBP);   
  IPop RBP;
  IRet]

let align_heap : instruction list = [
  IComment "Aligning heap";
  IMov (Reg R15, Reg RDI) ;
  IAdd (Reg R15, LitConst 7) ;
  IMov (Reg R11, LitConst64 heap_align_mask) ;
  IAnd (Reg R15, Reg R11)
]

(* compiles the main function definition, leaving enough space for its local variables, aligning the heap pointer, making a label, and managing he RSP and RBP registers *)
let compile_main_function_definition (e : tagExpr) (env : regEnv) (fenv : funEnv) (lspace : int) (fname : string) : instruction list = 
  [
  ILabel fname;
  IPush (Reg RBP);
  IMov (Reg RBP, Reg RSP);
  ISub (Reg RSP, LitConst (8 * lspace));
  IPush (Reg R12);
  ISub (Reg RSP, LitConst 8);] @
  align_heap @
  [IMov (Reg RDI, Reg RBP);
  ICall ("set_stack_bottom");]@
  compile_expr e env fenv @
  [IAdd (Reg RSP, LitConst 8);
  IPop R12; 
  IMov (Reg RSP, Reg RBP); 
  IPop RBP;
  IRet]

(* Compiles a lsit of normalized function definitions. Keeps track of the tag so that we can have a unique tag in each function and the program body*)
let rec compile_fundef_list (fundefs : fundef list) (currTag: int) (fenv : funEnv): (instruction list * int) =
  match fundefs with
  | [] -> ([], currTag)
  | fundef::rest -> 
    (match fundef with
    | DefFun (fname, arguments, body) -> 
      let _, lspace, _ = lookupFenv fname fenv in (* Space for local variables *)
      let taggedExpr, nextTag = (tag body currTag) in (* *)
      (*let arguments =
        (match arguments with
        | a::b::c::d::e::f::rest -> f::e::d::c::b::a::rest
        | _ -> rev arguments) in*)
      let argEnv = add_args_to_env arguments [] in
      let compiledDef = compile_function_definition taggedExpr argEnv fenv lspace fname in (*Compiling the function definition*)

      let nextDefs, newTag = compile_fundef_list rest nextTag fenv in (* Compiling the next definitions*)

      (compiledDef @ nextDefs, newTag) (*Concatenating the definitions and keeping track of the tag*)
    | RecordGetter (_, _) | RecordInit (_, _) -> compile_fundef_list rest currTag fenv (* Records dont need to be compiled *)
    
    | DefSys (_, _, _) -> failwith "Defsys Not implemented"
    )

let compile_prog (p : prog) : string =
  (* let compile_flags = print_string (Option.value (Sys.getenv_opt "CFLAGS") ~default:"-g") in *)
  let fundefs, body = p in
  let anfFundefs = map (
    fun fdef -> 
      match fdef with 
      | RecordGetter (_, _) | RecordInit (_, _) -> fdef
      | DefFun (fname, args, body) -> DefFun (fname, args, normalize body)
      | DefSys (_, _, _) -> failwith "Defsys Not Implemented"
    ) fundefs in
  let fenv = build_fenv anfFundefs [] in
  let compiledDefs, nextTag = compile_fundef_list anfFundefs 1 fenv in
  let anfBody = (normalize body) in
  let local_space = count_local_space anfBody in
  let taggedBody, _ = (tag anfBody nextTag) in
  let instrs = compile_main_function_definition taggedBody [] fenv local_space "our_code_starts_here" in
  let error_handlers = type_error_handlers in
  let prelude ="
section .text
extern print
extern typeError
extern overflowError
extern divByZeroError
extern indexError
extern arityError
extern print_stack
extern set_stack_bottom
extern try_gc
global our_code_starts_here
" in
  prelude ^ pp_instrs instrs ^ pp_instrs compiledDefs ^ pp_instrs error_handlers ^ (if safe then pp_instrs arithmetic_safety_handlers else "")
