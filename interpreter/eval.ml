open Ast

type builtin = value list -> environment -> value

and procedure =
  | ProcBuiltin of builtin
  | ProcLambda of variable list * environment * expression list

and value =
  | ValDatum of datum
  | ValProcedure of procedure

and binding = value ref Environment.binding
and environment = value ref Environment.environment

(* Parses a datum into an expression. *)
let rec read_expression (input : datum) : expression =
  match input with

  | Nil -> failwith "idk, man"

  | Atom (Identifier id) when Identifier.is_valid_variable id ->
      ExprVariable (Identifier.variable_of_identifier id)

  | Atom (Identifier id) ->
    (* Above match case didn't succeed, so id is not a valid variable. *)
    failwith "Identifier is not a valid variable."

  | Atom (Integer i) -> ExprSelfEvaluating (SEInteger i)

  | Atom (Boolean b) -> ExprSelfEvaluating (SEBoolean b)

  | Cons (Atom (Identifier i),cdr) when Identifier.is_keyword i -> 
    (match Identifier.string_of_identifier i with
    | "quote" -> 
      (match cdr with
      | Cons(d,Nil) -> ExprQuote d
      | _ -> failwith "Invalid call of quote.")
    | "if" -> 
      (match cdr with
       | (Cons(guard,Cons(e1,Cons(e2,Nil)))) -> 
         ExprIf (read_expression guard,read_expression e1,read_expression e2)
       | _ -> failwith "Invalid \"if\" expression."
      )
    (* | "lambda" -> 
      let rec var_ids input acc =
        match input with
         | Nil -> acc
         | Cons (Atom(Identifier id),t) -> var_ids t (id::acc) 
         | _ -> failwith "Invalid variable bindings in lambda expression." in 
      let rec exps input acc =
        match input with
         | Nil -> acc
         | Cons (h,t) -> exps t ((read_expression h)::acc)
         | _ -> failwith "Invalid exp. list in lambda expression." in
      match cdr with
      | Cons (vars,es) -> ExprLambda (var_ids vars) (exps es)
      | _ -> failwith "Invalid lambda syntax"
    | "define" | "set!" -> 
      (match cdr with 
       | (Cons(id,exp)) -> ExprAssignment id (read_expression exp)
       | _ -> failwith "Invalid assignment of variable."
      ) *)
    | _ -> failwith "Not implemented")

  | Cons (Atom (Identifier i),cdr) ->
    let rec parse_args lst acc =
      match lst with
      | Cons(arg,Nil) -> List.rev((read_expression arg)::acc)
      | Cons(arg,tail) -> parse_args tail ((read_expression arg)::acc)
      | _ -> failwith "Invalid arguments." in
    if Identifier.is_valid_variable i then
      let var = Identifier.variable_of_identifier i in
      ExprProcCall ((ExprVariable var), parse_args cdr [])
    else 
      failwith "Variable is not valid."

  | _ -> failwith "Not implemented"





(* Parses a datum into a toplevel input. *)
let read_toplevel (input : datum) : toplevel =
  match input with
  | _ -> ToplevelExpression (read_expression input)

(* This function returns an initial environment with any built-in
   bound variables. *)
let rec initial_environment () : environment =
  let add (val_list : value list) (env : environment) = print_endline "add";
    let rec helper acc lst = 
      match lst with 
      | [] -> acc
      | (ValDatum(Atom(Integer a)))::t -> helper (a+acc) t
      | _ -> failwith "Invalid call of \"+\"" in
    ValDatum(Atom(Integer (helper 0 val_list))) in 
  let mult (val_list : value list) (env : environment) =
    let rec helper lst acc = 
      match lst with 
      | [] -> acc
      | ValDatum(Atom(Integer a))::t -> helper t (a*acc)
      | _ -> failwith "Invalid call of \"*\"" in
    ValDatum(Atom(Integer (helper val_list 1))) in
  let cons (val_list : value list) (env : environment) =
    match val_list with
    | (ValDatum d1)::(ValDatum d2)::[] -> ValDatum(Cons(d1,d2))
    | _ -> failwith "Invalid use of \"cons\" operator" in
  let car (val_list : value list) (env : environment) =
    match val_list with 
    | ValDatum(Cons(res,_))::[] -> ValDatum(res)
    | _ -> failwith "Invalid use of \"car\" operator" in
  let cdr (val_list : value list) (env : environment) =
    match val_list with 
    | ValDatum(Cons(_,res))::[] -> ValDatum(res)
    | _ -> failwith "Invalid use of \"cdr\" operator" in
  let equal (val_list : value list) (env : environment) =
    match val_list with
    | x::y::[] -> ValDatum(Atom(Boolean(x=y)))
    | _ -> failwith "Invalid use of \"equal?\" operator" in
  let env = Environment.add_binding 
    (Environment.empty_environment) 
    (Identifier.variable_of_identifier (Identifier.identifier_of_string "course"),ref (ValDatum (Atom (Integer 3110)))) in 
  let env = Environment.add_binding 
    env 
    (Identifier.variable_of_identifier (Identifier.identifier_of_string "+"),ref (ValProcedure (ProcBuiltin(add)))) in
  let env = Environment.add_binding 
    env 
    (Identifier.variable_of_identifier (Identifier.identifier_of_string "*"),ref (ValProcedure (ProcBuiltin(mult)))) in
  let env = Environment.add_binding 
    env 
    (Identifier.variable_of_identifier (Identifier.identifier_of_string "cons"),ref (ValProcedure (ProcBuiltin(cons)))) in
  let env = Environment.add_binding 
    env 
    (Identifier.variable_of_identifier (Identifier.identifier_of_string "car"),ref (ValProcedure (ProcBuiltin(car)))) in
  let env = Environment.add_binding 
    env 
    (Identifier.variable_of_identifier (Identifier.identifier_of_string "cdr"),ref (ValProcedure (ProcBuiltin(cdr)))) in
  Environment.add_binding 
    env 
    (Identifier.variable_of_identifier (Identifier.identifier_of_string "equal?"),ref (ValProcedure (ProcBuiltin(equal)))) 

(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
and eval (expression : expression) (env : environment) : value =
  match expression with
  | ExprSelfEvaluating (SEBoolean b) -> ValDatum (Atom (Boolean b))

  | ExprSelfEvaluating (SEInteger i) -> ValDatum (Atom (Integer i))

  | ExprVariable v ->
    if Environment.is_bound env v then
      !(Environment.get_binding env v)
    else
      failwith "Variable is not bound in environment."

  | ExprQuote d ->
      ValDatum d

  | ExprLambda (_,_) -> failwith "Sing along with me as I row my boat!'"

  | ExprProcCall (ExprVariable var,lst) ->
    if Environment.is_bound env var then 
      let res = !(Environment.get_binding env var) in
      match res with 
      | ValProcedure(ProcBuiltin p) -> 
        let rec parse_args lst acc =
          match lst with
          | [] -> acc
          | h::t -> parse_args t ((eval h env)::acc) in
        p (List.rev((parse_args lst []))) env
      | ValProcedure(ProcLambda _) -> failwith "TODO"
      | _ -> failwith "Variable is not bound to function in environment."
    else
      failwith "Variable is not bound in environment."

  | ExprIf (guard, t, f) ->
    if (eval guard env) = ValDatum (Atom (Boolean true)) then
      eval t env
    else
      eval f env

  | ExprAssignment (_, _) ->
     failwith "Say something funny, Rower!"

  | ExprLet (_, _)

  | ExprLetStar (_, _)

  | ExprLetRec (_, _)     ->
     failwith "Ahahaha!  That is classic Rower."

  | _ -> failwith "TODO"

(* Evaluates a toplevel input down to a value and an output environment in a
   given environment. *)
let eval_toplevel (toplevel : toplevel) (env : environment) :
      value * environment =
  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (_, _)     ->
     failwith "I couldn't have done it without the Rower!"

let rec string_of_value value =
  let rec string_of_datum datum =
    match datum with
    | Atom (Boolean b) -> if b then "#t" else "#f"
    | Atom (Integer n) -> string_of_int n
    | Atom (Identifier id) -> Identifier.string_of_identifier id
    | Nil -> "()"
    | Cons (car, cdr) -> string_of_cons car cdr

  and string_of_cons car cdr =
    let rec strings_of_cons cdr =
      match cdr with
      | Nil -> []
      | Cons (car, cdr) -> (string_of_datum car) :: (strings_of_cons cdr)
      | _ -> ["."; string_of_datum cdr;] in
    let string_list = (string_of_datum car) :: (strings_of_cons cdr) in
    "(" ^ (String.concat " " string_list) ^ ")" in
  
  match value with
  | ValDatum (datum) -> string_of_datum datum
  | ValProcedure (ProcBuiltin p) -> "#<builtin>"
  | ValProcedure (ProcLambda (_, _, _)) -> "#<lambda>"
