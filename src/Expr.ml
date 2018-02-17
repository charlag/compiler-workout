(* Simple expressions: syntax and semantics *)

(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* The type for the expression. Note, in regular OCaml there is no "@type..."
   notation, it came from GT.
*)
@type expr =
  (* integer constant *) | Const of int
  (* variable         *) | Var   of string
  (* binary operator  *) | Binop of string * expr * expr with show

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

(* An example of a non-trivial state: *)
let s = update "x" 1 @@ update "y" 2 @@ update "z" 3 @@ update "t" 4 empty

(* Some testing; comment this definition out when submitting the solution. *)
(* let _ =
 *   List.iter
 *     (fun x ->
 *        try  Printf.printf "%s=%d\n" x @@ s x
 *        with Failure s -> Printf.printf "%s\n" s
 *     ) ["x"; "a"; "y"; "z"; "t"; "b"] *)

(* Expression evaluator

     val eval : state -> expr -> int

   Takes a state and an expression, and returns the value of the expression in
   the given state.
*)
let rec eval st expr =
  let num_to_bool = (<>) 0 in
  let bool_to_num bool = if bool then 1 else 0 in
  let bool_fun camlfun = fun l r -> bool_to_num (camlfun (num_to_bool l) (num_to_bool r)) in
  let comp_fun camlfun = fun l r -> bool_to_num (camlfun l r) in
  let fun_for_binop binop =
    match binop with
    | "!!" -> bool_fun (||)
    | "&&" -> bool_fun (&&)
    | "==" -> comp_fun (==)
    | "!=" -> comp_fun (<>)
    | "<=" -> comp_fun (<=)
    | "<"  -> comp_fun (<)
    | ">=" -> comp_fun (>=)
    | ">"  -> comp_fun (>)
    | "+"  -> (+)
    | "-"  -> (-)
    | "*"  -> ( * )
    | "/"  -> (/)
    | "%"  -> (mod)
    | _ -> failwith ("Unknwon  op" ^ binop) in
  match expr with
  | Const num -> num
  | Var s -> st s
  | Binop (op, expr1, expr2) -> (fun_for_binop op) (eval st expr1) (eval st expr2)
