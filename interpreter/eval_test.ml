open Assertions
open Ast
open Identifier
open Eval

(*read_expression*)


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
