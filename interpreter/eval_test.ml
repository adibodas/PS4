open Assertions
open Ast
open Eval

(*-------- HELPERS --------*)
let to_var v = 
	Identifier.variable_of_identifier (
		Identifier.identifier_of_string v)

let to_id = Identifier.identifier_of_string

(*-------- INITIAL_ENVIRONMENT TESTS (1) -----------*)

let env = initial_environment ()

TEST_UNIT "initial_environment_1" = 
	let v = !(Environment.get_binding env (to_var "course")) in
	assert_true (v = ValDatum(Atom(Integer(3110))))
TEST_UNIT "initial_environment_2" = 
	let v = !(Environment.get_binding env (to_var "+")) in
	assert_true 
	(
		match v with
		| ValProcedure(ProcBuiltin(_)) -> true
		| _ -> false
	)
TEST_UNIT "initial_environment_3" = 
	let v = !(Environment.get_binding env (to_var "*")) in
	assert_true 
	(
		match v with
		| ValProcedure(ProcBuiltin(_)) -> true
		| _ -> false
	)
TEST_UNIT "initial_environment_4" = 
	let v = !(Environment.get_binding env (to_var "cons")) in
	assert_true 
	(
		match v with
		| ValProcedure(ProcBuiltin(_)) -> true
		| _ -> false
	)
TEST_UNIT "initial_environment_5" = 
	let v = !(Environment.get_binding env (to_var "car")) in
	assert_true 
	(
		match v with
		| ValProcedure(ProcBuiltin(_)) -> true
		| _ -> false
	)
TEST_UNIT "initial_environment_6" = 
	let v = !(Environment.get_binding env (to_var "cdr")) in
	assert_true 
	(
		match v with
		| ValProcedure(ProcBuiltin(_)) -> true
		| _ -> false
	)
TEST_UNIT "initial_environment_7" = 
	let v = !(Environment.get_binding env (to_var "equal?")) in
	assert_true 
	(
		match v with
		| ValProcedure(ProcBuiltin(_)) -> true
		| _ -> false
	)
TEST_UNIT "initial_environment_8" = 
	let v = !(Environment.get_binding env (to_var "eval")) in
	assert_true 
	(
		match v with
		| ValProcedure(ProcBuiltin(_)) -> true
		| _ -> false
	)

(*-------- READ_EXPRESSION TESTS -----------*)

TEST_UNIT "read_expression_1" = 
	assert_true (
		read_expression (
			Atom (Identifier (to_id "swag"))
		) = ExprVariable (to_var "swag"))
TEST_UNIT "read_expression_2" =
	assert_true (
		read_expression (
			Atom(Integer(0))
		) = ExprSelfEvaluating (SEInteger 0))
TEST_UNIT "read_expression_3" = 
	assert_true (
		read_expression (
			Atom (Boolean true)
		) = ExprSelfEvaluating (SEBoolean true))
TEST_UNIT "read_expression_4" = 
	assert_true (
		read_expression (
			Atom (Boolean false)
		) = ExprSelfEvaluating (SEBoolean false)
	)
TEST_UNIT "read_expression_5" = 
	assert_true (
		read_expression (
			Cons (
				Atom (
					Identifier (to_id "quote")
				),
				Cons (
					Atom(Integer(0)),
					Nil
				)
			)
		)
		 = ExprQuote (Atom (Integer 0)))
TEST_UNIT "read_expression_6" = 
	assert_true (
		read_expression (
			Cons (
				Atom (Identifier (to_id "if")),
				Cons (
					Atom (Boolean true),
					Cons (
						Atom(Integer(23)),
						Cons (
							Atom(Integer(55)),
							Nil
						)
					)
				)
			)
		) 
		= 
		ExprIf (
			ExprSelfEvaluating(SEBoolean(true)),
 			ExprSelfEvaluating(SEInteger(23)),
 			ExprSelfEvaluating(SEInteger(55))
 		)
	)
TEST_UNIT "read_expression_7" = 
	assert_true (
		read_expression (
			Cons (
				Atom (Identifier (to_id "lambda")),
				Cons (
					Cons (
						Atom(Identifier (to_id "supercool")),
						Nil
					),
					Cons (
						Atom(Integer(0)),
						Nil
					)
				)
			)
		) 
		= 
		ExprLambda (
			[to_var "supercool"], 
			[ExprSelfEvaluating(SEInteger 0)]
		)
	)
TEST_UNIT "read_expression_8" = 
	assert_true (
		read_expression (
			Cons (
				Atom (Identifier (to_id "set!")),
				Cons(
					Atom (Identifier (to_id "king")), 
					Cons(Atom(Integer(3110)),Nil)
				)
			)
		) 
		= 
		ExprAssignment (to_var "king", Ast.ExprSelfEvaluating (Ast.SEInteger 3110)))
TEST_UNIT "read_expression_9" = 
	assert_true (
		read_expression (
			Cons (
				Atom (Identifier (to_id "letrec")),
				Cons (
					Cons (
						Cons (
							Atom (Identifier (to_id "x")),
							Cons (
								Atom (Integer 30),
								Nil
							)
						),
						Cons (
							Cons (
								Atom (Identifier (to_id "y")),
								Cons (
									Atom (Integer 42),
									Nil
								)
							),
							Nil
						)
					),
					Cons (
						Atom (Identifier (to_id "x")),
						Nil
					)
				)
			)
		)
		= 
		ExprLetRec (
			[
				(to_var "x",ExprSelfEvaluating(SEInteger 30));
				(to_var "y",ExprSelfEvaluating(SEInteger 42))
			],
			[
				(ExprVariable (to_var "x"))
			]
		) 
	)
TEST_UNIT "read_expression_10" = 
	assert_true (
		read_expression (
			Cons (
				Atom (Identifier (to_id "mainvar")),
				Cons(
					Atom(Integer(0)),
					Nil
				)
			)
		) 
		= 
		ExprProcCall (
			ExprVariable (to_var "mainvar"),
 			[ExprSelfEvaluating (SEInteger 0)]
 		))

(*--------- READ_TOPLEVEL TESTS ------------*)

TEST_UNIT "read_toplevel_1" =
	let dat = 
		Cons (
			Atom (Identifier (to_id "letrec")),
			Cons (
				Cons (
					Cons (
						Atom (Identifier (to_id "x")),
						Cons (
							Atom (Integer 30),
							Nil
						)
					),
					Cons (
						Cons (
							Atom (Identifier (to_id "y")),
							Cons (
								Atom (Integer 42),
								Nil
							)
						),
						Nil
					)
				),
				Cons (
					Atom (Identifier (to_id "x")),
					Nil
				)
			)
		) in
	assert_true (
		read_toplevel dat = ToplevelExpression(read_expression dat)
	)
TEST_UNIT "read_toplevel_2" =
	assert_true (
		read_toplevel (
			Cons (
				Atom (Identifier (to_id "define")),
				Cons (
					Atom (Identifier (to_id "x")),
					Cons (
						Atom (Integer 30),
						Nil
					)
				)
			)
		) = 
		ToplevelDefinition (
			(
				to_var "x",
				ExprSelfEvaluating(SEInteger(30))
			)
		)
	)
TEST_UNIT "read_toplevel_3" =
	assert_raises 
		(Some
			(Failure
				"Invalid define syntax."))
		read_toplevel
		(
			Cons (
				Atom (Identifier (to_id "define")),
				Cons (
					Atom (Identifier (to_id "quote")),
					Cons (
						Atom (Integer 30),
						Nil
					)
				)
			)
		)

(*--------- EVAL TESTS -------------*)

TEST_UNIT "eval_1" =
	assert_true (
		eval (
			ExprSelfEvaluating(SEBoolean(true))
		) env = ValDatum(Atom(Boolean(true)))
	)
TEST_UNIT "eval_2" =
	assert_true (
		eval (
			ExprSelfEvaluating(SEInteger(21873))
		) env = ValDatum(Atom(Integer(21873)))
	)
TEST_UNIT "eval_3" =
	assert_true (
		eval (
			ExprVariable (to_var "course")
		) env = ValDatum(Atom(Integer(3110)))
	)
TEST_UNIT "eval_4" =
	assert_raises 
		(Some (Failure "m is not bound within the environment (look-up)."))
		(eval (ExprVariable(to_var "m")))
		env
TEST_UNIT "eval_5" = 
	assert_true (
		eval (
			ExprQuote (Atom(Integer(0)))
		) env = ValDatum(Atom(Integer(0)))
	)
TEST_UNIT "eval_6" =
	assert_true (
		eval (
			ExprQuote (Atom(Boolean(true)))
		) env = ValDatum(Atom(Boolean(true)))
	)
TEST_UNIT "eval_7" =
	assert_true (
		eval (
			ExprQuote (
				Cons(
					Atom(Integer(7)),
					Cons(Atom(Integer(2)),Nil)
				)
			)
		) env 
		= 
		ValDatum(
			Cons(
				Atom(Integer(7)),
				Cons(Atom(Integer(2)),Nil)
			)
		)
	)

(*testing lambda requires testing of environment strings, 
	which is very difficult to do without first assuming our 
	code is correct. Will test lambda calls, however*)

TEST_UNIT "eval_8" =
	assert_true (
		eval (
			ExprProcCall(
				ExprLambda(
					[to_var "f"],
					[ExprVariable(to_var "f")]
				),
				[ExprSelfEvaluating(SEInteger(3))])
		) env = ValDatum(Atom(Integer(3)))
	)
TEST_UNIT "eval_9" =
	assert_true (
		eval (
			ExprIf(
				ExprSelfEvaluating(SEBoolean(true)),
				ExprSelfEvaluating(SEInteger(0)),
				ExprSelfEvaluating(SEInteger(1))
			)
		) env = ValDatum(Atom(Integer(0)))
	)
TEST_UNIT "eval_10" =
	assert_true (
		eval (
			ExprIf (
				ExprSelfEvaluating(SEBoolean(false)),
				ExprSelfEvaluating(SEInteger(0)),
				ExprSelfEvaluating(SEInteger(1))
			)
		) env = ValDatum(Atom(Integer(1)))
	)
TEST_UNIT "eval_11" =
	let _ = 
		eval (
			ExprAssignment (
				to_var "course", 
				ExprSelfEvaluating(SEInteger(1389))
			)
		) env in
	assert_true (
		!(Environment.get_binding env (to_var "course")) 
		= 
		ValDatum(Atom(Integer(1389)))
	)

(*reset*)
let env = initial_environment ()

TEST_UNIT "eval_12" =
	assert_raises 
		(
			Some (
				Failure 
					"f is not bound within the environment (assignment)."
			)
		)
		(eval (
			ExprAssignment (
				to_var "f", 
				ExprSelfEvaluating(SEInteger(1389))
			))
		) 
		env
TEST_UNIT "eval_13" =
	assert_true (
		eval (
			ExprLet (
				[(to_var "x",ExprSelfEvaluating(SEInteger(30)))],
				[ExprVariable(to_var "x")]
			)
		) env = ValDatum(Atom(Integer(30))) 
	)
TEST_UNIT "eval_14" = 
	assert_true (
		eval (
			ExprLet (
				[
					(to_var "course",ExprSelfEvaluating(SEInteger(30)));
					(to_var "x", ExprVariable(to_var "course"))
				],
				[ExprVariable(to_var "x")]
			)
		) env = ValDatum(Atom(Integer(3110))) 
	)
TEST_UNIT "eval_15" =
	assert_true (
		eval (
			ExprLetStar (
				[
					(to_var "course",ExprSelfEvaluating(SEInteger(30)));
					(to_var "x", ExprVariable(to_var "course"))
				],
				[ExprVariable(to_var "x")]
			)
		) env = ValDatum(Atom(Integer(30)))
	)
TEST_UNIT "eval_16" =
	assert_true (
		(* 
			(letrec ((x (lambda (y) (if y #t (x #t))))) (x #t))
		*)
		eval (
			ExprLetRec (
				[
					(to_var "f", ExprLambda ([to_var "y"],
						[ExprIf (
							ExprVariable (to_var "y"),
							ExprSelfEvaluating(SEBoolean(true)),
							ExprProcCall(
								ExprVariable(to_var "f"),
								[ExprSelfEvaluating(SEBoolean(true))]
							)
						)])
					)
				],
				[
					ExprProcCall (
						ExprVariable(to_var "f"),
						[ExprSelfEvaluating(SEBoolean(true))]
					)
				]
			)
		) env = ValDatum(Atom(Boolean(true)))
	)
TEST_UNIT "eval_17" =
	assert_true (
		(* 
			(letrec ((x (lambda (y) (if y #t (x #t))))) (x #f))
		*)
		eval (
			ExprLetRec (
				[
					(
						to_var "f", 
						ExprLambda (
							[to_var "y"],
							[
								ExprIf (
									ExprVariable (to_var "y"),
									ExprSelfEvaluating(SEBoolean(true)),
									ExprProcCall (
										ExprVariable(to_var "f"),
										[ExprSelfEvaluating(SEBoolean(false))]
									)
								)
							]
						)
					)
				],
				[
					ExprProcCall (
						ExprVariable(to_var "f"),
						[ExprSelfEvaluating(SEBoolean(true))]
					)
				]
			)
		) env = ValDatum(Atom(Boolean(true)))
	)

(*--------- EVAL_TOPLEVEL TESTS ---------*)

TEST_UNIT "eval_toplevel_1" =
	assert_true (
		match 
			eval_toplevel
				(ToplevelExpression(ExprSelfEvaluating(SEInteger(30))))
				env 
			with
		|(ValDatum(Atom(Integer(30))),_) -> true
		| _ -> false
	)
TEST_UNIT "eval_toplevel_2" =
	let (_, new_env) =
		eval_toplevel
			(ToplevelDefinition(
				to_var "x",
				ExprSelfEvaluating(SEInteger(30))
			))
			env in
	let v = !(Environment.get_binding new_env (to_var "x")) in
	assert_true (v = ValDatum(Atom(Integer 30)))
TEST_UNIT "eval_toplevel_3" =
	let exp = 
		ExprLambda (
			[to_var "y"],
			[
				ExprIf (
					ExprVariable (to_var "y"),
					ExprSelfEvaluating(SEBoolean(true)),
					ExprProcCall (
						ExprVariable(to_var "f"),
						[ExprSelfEvaluating(SEBoolean(false))]
					)
				)
			]
		) in
	let (_,new_env) = 
		eval_toplevel
			(ToplevelDefinition (to_var "f",exp))
			env in
	assert_true (
		eval
			(
				ExprProcCall(
					ExprVariable(to_var "f"),
					[
						ExprSelfEvaluating(SEBoolean(true))
					]
				)
			)
			new_env
		= 
		ValDatum(Atom(Boolean(true)))
	)
	
(*---------------------------------------*)
let () = Pa_ounit_lib.Runtime.summarize()

