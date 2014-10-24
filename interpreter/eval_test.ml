open Assertions
open Ast
open Identifier
open Eval


(*read_expression*)

TEST_UNIT "read_expression_1" =
	assert_true (
		read_expression (
			Atom(Integer(0))
		) = ExprSelfEvaluating(SEInteger(0)))



(*--------- EVAL TESTS -------------*)
let env = initial_environment ()
let to_var v = variable_of_identifier (identifier_of_string v)

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
(*testing lambda requires testing of environment strings, which is very difficult to do
	without first assuming our code is correct. Will test lambda calls, however*)
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
			ExprIf(
				ExprSelfEvaluating(SEBoolean(false)),
				ExprSelfEvaluating(SEInteger(0)),
				ExprSelfEvaluating(SEInteger(1))
			)
		) env = ValDatum(Atom(Integer(1)))
	)
TEST_UNIT "eval_11" =
	let _ = 
		eval (
			ExprAssignment(
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
		(Some 
			(Failure 
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
					(to_var "f", ExprLambda ([to_var "y"],
						[ExprIf (
							ExprVariable (to_var "y"),
							ExprSelfEvaluating(SEBoolean(true)),
							ExprProcCall(
								ExprVariable(to_var "f"),
								[ExprSelfEvaluating(SEBoolean(false))]
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

TEST_UNIT "read_expression1" = 
	assert_true (read_expression (Atom (Identifier "swag")) = ExprVariable "swag")
TEST_UNIT "read_expression2" =
	assert_true (read_expression (Atom(Integer(0))) = ExprSelfEvaluating (SEInteger 0))
TEST_UNIT "read_expression3" = 
	assert_true (read_expression (Atom (Boolean true)) = ExprSelfEvaluating (SEBoolean true))
TEST_UNIT "read_expression4" = 
	assert_true (read_expression (Atom (Boolean false)) = ExprSelfEvaluating (SEBoolean false))
TEST_UNIT "read_expression5" = 
	assert_true (Cons (Atom (Identifier "quote"),Cons(Atom(Integer(0)),Nil)) = ExprQuote (Atom (Ast.Integer 0)))
TEST_UNIT "read_expression6" = 
	assert_true (Cons (Atom (Identifier "if"),(Cons((Atom (Boolean true)),Cons((Atom(Integer(23))),Cons((Atom(Integer(55))),Nil))))) = Ast.ExprIf (Ast.ExprSelfEvaluating (Ast.SEBoolean true),
 Ast.ExprSelfEvaluating (Ast.SEInteger 23),
 Ast.ExprSelfEvaluating (Ast.SEInteger 55)))
TEST_UNIT "read_expression7" = 
	assert_true (read_expression (Cons (Atom (Identifier "lambda"),Cons (Cons (Atom(Identifier "supercool"),Nil),Cons (Atom(Integer(0)),Nil)))) = Ast.ExprLambda (["supercool"], [Ast.ExprSelfEvaluating (Ast.SEInteger 0)]))
TEST_UNIT "read_expression8" = 
	assert_true (Cons (Atom (Identifier "set!"),Cons(Atom (Identifier ("king")), Cons(Atom(Integer(3110)),Nil))) = Ast.ExprAssignment ("king", Ast.ExprSelfEvaluating (Ast.SEInteger 3110)))
(*TEST_UNIT "read_expression9" =  ... next lambda ...*)

TEST_UNIT "read_expression11" = 
	assert_true (Cons (Atom (Identifier "letrec"),Cons (lets,exps)) = ExprLetRec (parse_lets [] [] lets, parse_exps exps []) )

	TEST_UNIT "read_expression10" = 
	assert_true (Cons (Atom (Identifier "mainvar"),Cons(Atom(Integer(0)),Nil)) = ExprProcCall (ExprVariable "mainvar",
 [ExprSelfEvaluating (SEInteger 0)]))

let () = Pa_ounit_lib.Runtime.summarize()
>>>>>>> 14300929e3b3bbc4b3e1ca5e8570249d2dae52fd
