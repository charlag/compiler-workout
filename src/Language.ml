(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators

(* Simple expressions: syntax and semantics *)
module Expr =
  struct

    (* The type for expressions. Note, in regular OCaml there is no "@type..."
       notation, it came from GT.
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* State: a partial map from variables to integer values. *)
    type state = string -> int

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Eval binop *)
    let e_binop op lr rr =
      let num_to_bool = (<>) 0 in
      let bool_to_num bool = if bool then 1 else 0 in
      let bool_fun camlfun = fun l r -> bool_to_num (camlfun (num_to_bool l) (num_to_bool r)) in
      let comp_fun camlfun = fun l r -> bool_to_num (camlfun l r) in
      let fun_for_binop binop =
        match binop with
        | "!!" -> bool_fun ( ||  )
        | "&&" -> bool_fun ( &&  )
        | "==" -> comp_fun ( ==  )
        | "!=" -> comp_fun ( <>  )
        | "<=" -> comp_fun ( <=  )
        | "<"  -> comp_fun ( <   )
        | ">=" -> comp_fun ( >=  )
        | ">"  -> comp_fun ( >   )
        | "+"  ->          ( +   )
        | "-"  ->          ( -   )
        | "*"  ->          ( *   )
        | "/"  ->          ( /   )
        | "%"  ->          ( mod )
        | _    ->          failwith ("Unknwon  op" ^ binop) in
      (fun_for_binop op) lr rr

    (* Expression evaluator
          val eval : state -> t -> int
       Takes a state and an expression, and returns the value of the expression in
       the given state.
    *)
    let rec eval st expr =
      match expr with
      | Const num                -> num
      | Var   s                  -> st s
      | Binop (op, expr1, expr2) ->
        e_binop op (eval st expr1) (eval st expr2)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string

    *)

    let make_binop op = fun x y -> Binop(op, x, y)

        ostap (
        primary: n:DECIMAL {Const n} | n:IDENT {Var n} | -"(" parse -")";
        parse:
          !(Ostap.Util.expr
              (fun x -> x)
              [|
                `Lefta , [ostap("!!"), make_binop "!!"];
                `Lefta , [ostap("&&"), make_binop"&&"];
                `Nona  , [
                  ostap(">="), make_binop ">=";
                  ostap(">"), make_binop ">";
                  ostap("<="), make_binop "<=";
                  ostap("<"), make_binop "<";
                  ostap("=="), make_binop "==";
                  ostap("!="), make_binop "!="
                ];
                `Lefta , [ostap("+"), make_binop "+"; ostap("-"), make_binop "-"];
                `Lefta , [
                  ostap("*"), make_binop "*";
                  ostap("/"), make_binop "/";
                  ostap("%"), make_binop "%"
                ]
              |]
              primary
           )
      )

  end

(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval cfg stmt =
      let (e_state, input, output) = cfg in
      match stmt with
      | Read var           ->
        let hd :: tl = input in
        let e_state' = Expr.update var hd e_state in
        (e_state', tl, output)
      | Write w_expr       ->
        let value = Expr.eval e_state w_expr in
        (e_state, input, output @ [value])
      | Assign (var, expr) ->
        let value = Expr.eval e_state expr in
        let e_state' = Expr.update var value e_state in
        (e_state', input, output)
      | Seq (fst, snd)     ->
        let cfg' = eval cfg fst in eval cfg' snd

    (* Statement parser *)
          ostap(
          statement:
            -"read" -"(" var:IDENT -")" { Read var }
        | -"write" -"(" expr:!(Expr.parse) -")" { Write expr }
          | var:IDENT -":=" expr:!(Expr.parse) { Assign (var, expr) };
            parse: <s::ss> : !(Ostap.Util.listBy)[ostap (";")][statement] {
                List.fold_left(fun x y -> Seq (x, y)) s ss
              }
        )

  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse
