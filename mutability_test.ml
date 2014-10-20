open Assertions
open Mutability

let a = count_up_from 3 4
TEST_UNIT "count_up_from_1" =
	assert_true (a() = 3)
TEST_UNIT "count_up_from_2" =
	assert_true (a() = 7)
TEST_UNIT "count_up_from_3" =
	assert_true (a() = 11)

let b = count_up_from 0 (-1)
TEST_UNIT "count_up_from_4" =
	assert_true (b() = 0)
TEST_UNIT "count_up_from_5" =
	assert_true (b() = (-1))
TEST_UNIT "count_up_from_6" =
	assert_true (b() = (-2))


TEST_UNIT "tabulate_1" = 
	assert_true (tabulate (fun x -> x*x) 10 = [|0;1;4;9;16;25;36;49;64;81|])
TEST_UNIT "tabulate_2" =
	assert_true (tabulate (fun x -> x) 0 = [||])


TEST_UNIT "fold_1" =
	assert_true (fold_left_imp (+) 0 [1;2;3;4;5;6;7;8;9] = 45)
TEST_UNIT "fold_2" = 
	assert_true (fold_left_imp (+) 1276 [] = 1276)
TEST_UNIT "fold_3" =
	assert_true	(fold_left_imp ( * ) 0 [1;2;3;4] = 0)


TEST_UNIT "zardoz_1" =
	assert_false (List.rev (List.map zardoz lst) = List.map zardoz (List.rev lst))