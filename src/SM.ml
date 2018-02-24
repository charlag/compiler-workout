open GT

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
let rec eval cfg prog =
  match prog with
  | [] -> cfg
  | inst :: rest ->
    let (stack, config) = cfg in
    let (e_state, input, output) = config in
    match inst with
    | CONST value   -> eval (value :: stack, config) rest
    | READ          -> let (fi :: ri) = input in eval (fi :: stack, config) rest
    | WRITE         ->
      let (fs :: rs) = stack in eval (rs, (e_state, input, output @ [fs])) rest
    | ST    varname -> let (fs :: rs) = stack in
      let e_state' = Syntax.Expr.update varname fs e_state in
      let config'  = (e_state', input, output) in
      eval (rs, config') rest
    | LD    varname -> let v = e_state varname in
      eval (v :: stack, config) rest
    | BINOP opname  -> let (fs :: ss :: rs) = stack in
      let v = Syntax.Expr.e_binop opname fs ss in
      eval (v :: rs, config) rest

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let compile _ = failwith "Not yet implemented"
