open Iterator
open Assertions

open ListIterator

let list_iter = create [1;2;3;4;5;6] 
TEST_UNIT "ListIterator_1" =
	assert_true (next list_iter = 1); assert_true (has_next list_iter)
TEST_UNIT "ListIterator_2" =
	assert_true (next list_iter = 2); assert_true (has_next list_iter)
TEST_UNIT "ListIterator_3" =
	assert_true (next list_iter = 3); assert_true (has_next list_iter)
TEST_UNIT "ListIterator_4" =
	assert_true (next list_iter = 4); assert_true (has_next list_iter)
TEST_UNIT "ListIterator_5" =
	assert_true (next list_iter = 5); assert_true (has_next list_iter)
TEST_UNIT "ListIterator_6" =
	assert_true (next list_iter = 6); assert_false (has_next list_iter)
TEST_UNIT "ListIterator_7" =
	assert_raises (Some NoResult) next list_iter

TEST_UNIT "ListIterator_8" =
	let e : int list = [] in let empty = create e in
	assert_false (has_next empty); assert_raises (Some NoResult) next empty

open InorderTreeIterator
let tree_iter = 
	let tree = 
		Node(4,
			Node(2,
				Node(1, Leaf, Leaf),
				Node(3, Leaf, Leaf)),
			Node(6,
				Node(5, Leaf, Leaf),
				Node(7, Leaf, Leaf))) in
	create tree

TEST_UNIT "InorderTreeIterator_1" =
	assert_true (next tree_iter = 1); assert_true (has_next tree_iter)
TEST_UNIT "InorderTreeIterator_2" =
	assert_true (next tree_iter = 2); assert_true (has_next tree_iter)
TEST_UNIT "InorderTreeIterator_3" =
	assert_true (next tree_iter = 3); assert_true (has_next tree_iter)
TEST_UNIT "InorderTreeIterator_4" =
	assert_true (next tree_iter = 4); assert_true (has_next tree_iter)
TEST_UNIT "InorderTreeIterator_5" =
	assert_true (next tree_iter = 5); assert_true (has_next tree_iter)
TEST_UNIT "InorderTreeIterator_6" =
	assert_true (next tree_iter = 6); assert_true (has_next tree_iter)
TEST_UNIT "InorderTreeIterator_7" =
	assert_true (next tree_iter = 7); assert_false (has_next tree_iter)
TEST_UNIT "InorderTreeIterator_8" =
	assert_raises (Some NoResult) next tree_iter

TEST_UNIT "InorderTreeIterator_9" =
	let empty = create Leaf in
	assert_false (has_next empty); assert_raises (Some NoResult) next empty

module TakeIter = TakeIterator (ListIterator)
open TakeIter
let take_iter = create 5 (ListIterator.create [1;2;3;4;5;6;7])

TEST_UNIT "TakeIterator_1" =
	assert_true (next take_iter = 1); assert_true (has_next take_iter)
TEST_UNIT "TakeIterator_2" =
	assert_true (next take_iter = 2); assert_true (has_next take_iter)
TEST_UNIT "TakeIterator_3" =
	assert_true (next take_iter = 3); assert_true (has_next take_iter)
TEST_UNIT "TakeIterator_4" =
	assert_true (next take_iter = 4); assert_true (has_next take_iter)
TEST_UNIT "TakeIterator_5" =
	assert_true (next take_iter = 5); assert_false(has_next take_iter)
TEST_UNIT "TakeIterator_8" =
	assert_raises (Some NoResult) next take_iter

let take_iter_2 = create 0 (ListIterator.create [1;2;3;4;5;6;7])

TEST_UNIT "TakeIterator_9" =
	assert_false(has_next take_iter); assert_raises (Some NoResult) next take_iter

let take_iter_3 = create 6 (ListIterator.create [1;2;3])

TEST_UNIT "TakeIterator_10" =
	assert_true (next take_iter_3 = 1); assert_true (has_next take_iter_3)
TEST_UNIT "TakeIterator_11" =
	assert_true (next take_iter_3 = 2); assert_true (has_next take_iter_3)
TEST_UNIT "TakeIterator_12" =
	assert_true (next take_iter_3 = 3); assert_false (has_next take_iter_3)
TEST_UNIT "TakeIterator_13" =
	assert_raises (Some NoResult) next take_iter_3

module Utils = IteratorUtilsFn(ListIterator)
open Utils
let util_iter = ListIterator.create [1;2;3;4;5;6;7]

TEST_UNIT "IteratorUtilsFn_1" = 
	advance 5 util_iter; assert_true (ListIterator.next util_iter = 6); 
		assert_true (ListIterator.has_next util_iter)
TEST_UNIT "IteratorUtilsFn_2" =
	assert_raises (Some ListIterator.NoResult) (advance 3) util_iter
TEST_UNIT "IteratorUtilsFn_3" =
	let util_iter = ListIterator.create [1;2;3;4;5;6;7] in
	assert_true (fold (+) 0 util_iter = 28)
TEST_UNIT "IteratorUtilsFn_4" =
	let x : int list = [] in
	let util_iter = ListIterator.create x in
	assert_true (fold (+) 0 util_iter = 0)
TEST_UNIT "IteratorUtilsFn_5" =
	let util_iter = ListIterator.create ["A";"B";"C";"D";"E"] in
	assert_true (fold (^) "" util_iter = "ABCDE")

module Range = RangeIterator(ListIterator)
open Range
let range_iter = create 3 6 (ListIterator.create [1;2;3;4;5;6;7;8;9])

TEST_UNIT "RangeIterator_1" = 
	assert_true (next range_iter = 3); assert_true (has_next range_iter)
TEST_UNIT "RangeIterator_2" = 
	assert_true (next range_iter = 4); assert_true (has_next range_iter)
TEST_UNIT "RangeIterator_3" = 
	assert_true (next range_iter = 5); assert_true (has_next range_iter)
TEST_UNIT "RangeIterator_4" = 
	assert_true (next range_iter = 6); assert_false (has_next range_iter)
TEST_UNIT "RangeIterator_5" = 
	assert_raises (Some NoResult) next range_iter
TEST_UNIT "RangeIterator_6" =
	assert_raises (Some NoResult) (create 5 4) (ListIterator.create [1])

let () = Pa_ounit_lib.Runtime.summarize()