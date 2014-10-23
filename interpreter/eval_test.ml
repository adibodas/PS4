open Assertions
open Ast
open Identifier
open Eval

(*read_expression*)

TEST_UNIT "read_expression_1" =
	assert_true (read_expression (Atom(Integer(0))) = ExprSelfEvaluating(SEInteger(0)))