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

(* Parses a datum into an expression. *)
let rec read_expression (input : datum) : expression =
  match input with

  | Atom (Identifier id) when Identifier.is_valid_variable id ->
      ExprVariable (Identifier.variable_of_identifier id)

  | Atom (Identifier id) ->
    (* Above match case didn't succeed, so id is not a valid variable. *)
    failwith "Identifier is not a valid variable."

  | Atom (Integer i) -> ExprSelfEvaluating (SEInteger i)

  | Atom (Boolean b) -> ExprSelfEvaluating (SEBoolean b)

  | Cons (Atom (Identifier i),cdr) when Identifier.string_of_identifier i = "quote" -> 
    (match cdr with
    | Cons(d,Nil) -> ExprQuote d
    | _ -> failwith "Invalid call of quote.")

  | Cons (Atom (Identifier i),cdr) when Identifier.string_of_identifier i = "if" ->
    (match cdr with
    | (Cons(guard,Cons(e1,Cons(e2,Nil)))) -> 
      ExprIf (read_expression guard,read_expression e1,read_expression e2)
    | _ -> failwith "Invalid \"if\" expression."
    )

  | Cons (Atom (Identifier i),Cons (vars,exps)) when Identifier.string_of_identifier i = "lambda"  ->
    print_endline "lambda";
    let rec parse_vars input acc =
      print_endline "1";
      match input with
      | Cons (Atom(Identifier id),Nil) -> List.rev ((Identifier.variable_of_identifier id)::acc)
      | Cons (Atom(Identifier id),t) -> 
        if Identifier.is_valid_variable id && not (List.mem (Identifier.variable_of_identifier id) acc) then
          parse_vars t ((Identifier.variable_of_identifier id)::acc)
        else  
          failwith "Invalid variable names."
       | _ -> failwith "Invalid variable bindings in lambda expression." in 
    let rec parse_exps input acc =
      print_endline "2";
      match input with
      | Cons (exp,Nil) -> List.rev ((read_expression exp)::[])
      | Cons (h,t) -> parse_exps t ((read_expression h)::acc)
      | d -> [read_expression d] in
    (match exps with
    | Nil -> failwith "Invalid lambda syntax"
    | _ -> ExprLambda ((parse_vars vars []),(parse_exps exps [])))
    
(*| Cons (Atom (Identifier i),cdr) when i = "define" 
  | Cons (Atom (Identifier i),cdr) when i = "set!"
    (match cdr with 
    | (Cons(id,exp)) -> ExprAssignment id (read_expression exp)
    | _ -> failwith "Invalid assignment of variable."
    ) *)

  | Cons (Atom (Identifier id),cdr) ->
    print_endline (Identifier.string_of_identifier id);
    let rec parse_args lst acc =
      match lst with
      | Cons(arg,Nil) -> List.rev((read_expression arg)::acc)
      | Cons(arg,tail) -> parse_args tail ((read_expression arg)::acc)
      | _ -> failwith "Invalid arguments." in
    if Identifier.is_valid_variable id then
      let var = Identifier.variable_of_identifier id in
      ExprProcCall ((ExprVariable var), parse_args cdr [])
    else 
      failwith "Variable is not valid."

  | Cons (
      (Cons (
        (Atom (Identifier id)),
        _) as lambda),
      args) 
      when Identifier.string_of_identifier id = "lambda" 
      -> 
    print_endline "lambda_call";
    let rec parse_args lst acc =
      match lst with
      | Cons(arg,Nil) -> List.rev((read_expression arg)::acc)
      | Cons(arg,tail) -> parse_args tail ((read_expression arg)::acc)
      | _ -> failwith "Invalid arguments." in
    let result = read_expression lambda in
    (match result with
    | ExprLambda _ -> ExprProcCall (result, parse_args args [])
    | _ -> failwith "Invalid lambda syntax (also this case should fail earlier)."
    )
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

  | ExprLambda (vars,exps) -> 
      ValProcedure(ProcLambda (vars,env,exps))

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
      | ValProcedure(ProcLambda (vars,env',exps)) -> 
        eval 
          (ExprProcCall(ExprLambda(vars,exps),lst)) 
          (Environment.combine_environments env env')
      | _ -> failwith "Variable is not bound to function in environment."
    else
      failwith "Variable is not bound in environment."

  | ExprProcCall (ExprLambda (vars,exps),args) ->
    let rec populate_env vars args acc =
      match vars,args with
      | v::t1, a::t2 -> 
        print_endline (string_of_value (eval a acc));
        let env' = Environment.add_binding acc (v,ref (eval a acc)) in
        populate_env t1 t2 env'
      | [],[] -> acc
      | _ -> failwith "Mismatched variables and arguments." in
    let env' = populate_env vars args Environment.empty_environment in
    let env' = Environment.combine_environments env' env in 
    List.fold_left (fun _ e -> eval e env') (ValDatum(Nil)) exps 

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

