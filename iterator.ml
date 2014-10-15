module type ITERATOR = sig
  type 'a t
  exception NoResult

  (* returns: true if there are more results to yield,
   *   otherwise returns false. *)
  val has_next: 'a t -> bool

  (* returns:  the next result, if there are more results
   *   to yield.  Otherwise raises NoResult.
   * effects:  modifies the state of the iterator to record
   *   the yield. *)
  val next: 'a t -> 'a
end

module type LIST_ITERATOR = sig
  include ITERATOR
  (* parameters:  a list l
   * returns:  an iterator that will yield the elements of l,the
   *   each exactly once, in the order that they appear in l,
   *   starting with the head.  *)
  val create: 'a list -> 'a t
end

module ListIterator : LIST_ITERATOR = struct
  type 'a t = 'a list ref
  exception NoResult

  let create (lst : 'a list) : 'a t = ref lst
  let has_next (r : 'a t) : bool = !r <> []
  let next (r : 'a t) : 'a = 
    match !r with
    | [] -> raise NoResult
    | h::t -> r := t; h
end


type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree)

module type INORDER_TREE_ITERATOR = sig
  include ITERATOR
  (* parameters:  a tree t
   * returns:  an iterator that will yield the elements of t,
   *   each exactly once, in the order that would be produced
   *   by an in-order traversal of t. *)
  val create: 'a tree -> 'a t
end

module InorderTreeIterator : INORDER_TREE_ITERATOR = struct
  type 'a t = 'a list ref
  exception NoResult

  (*have to make this efficient*)
  let create (t : 'a tree) : 'a t = 
    let rec helper t = 
      match t with 
      | Leaf -> []
      | Node (v, l, r) -> (helper l) @ [v] @ (helper r) in
    ref (helper t)

  let has_next (r : 'a t) : bool = !r <> []
  let next (r : 'a t) : 'a = 
    match !r with
    | [] -> raise NoResult
    | h::t -> r := t; h
end


module type TAKE_ITERATOR = functor (I: ITERATOR) -> sig
  include ITERATOR
  (* parameters:  an integer n and an iterator i
   * returns:  an iterator that behaves the same as i for
   *   exactly n calls to next, but afterwards
   *   raises NoResult. *)

  val create: int -> 'a I.t -> 'a t
end

module TakeIterator : TAKE_ITERATOR = functor (I : ITERATOR) -> struct
  type 'a t = (int * 'a I.t) ref
  exception NoResult

  let create (n : int) (i : 'a I.t) : 'a t = ref (n,i)

  let has_next r = 
    let (n,i) = !r in 
    n <> 0 && (I.has_next i)

  let next (r : 'a t) : 'a = 
    let (n,i) = !r in
    if n = 0 then 
      raise NoResult
    else 
      r := (n-1, i); (I.next i)
end


module IteratorUtilsFn (I : ITERATOR) = struct
  open I
  (* effects: causes i to yield n results, ignoring
   *   those results.  Raises NoResult if i does.  *)
  let rec advance (n: int) (iter: 'a I.t) : unit =
    if n = 0 then () else let _ = next iter in advance (n-1) iter

  (* returns: the final value of the accumulator after
   *   folding f over all the results returned by i,
   *   starting with acc as the initial accumulator.
   * effects: causes i to yield all its results. *)
  let rec fold (f : ('a -> 'b -> 'a)) (acc : 'a) (iter: 'b I.t) : 'a =
    if has_next iter then
      fold f (f acc (next iter)) iter
    else
      acc
end

module type RANGE_ITERATOR = functor (I : ITERATOR) -> sig
  include ITERATOR

  (* parameters: integers n and m and an iterator i
   * returns: an iterator that behaves the way I would
   *   on the nth through mth calls to next, and
   *   afterwards raises NoResult.
   *
   *   If n > m the resulting iterator should always raise NoResult.
   *)
  val create : int -> int -> 'a I.t -> 'a t
end

module RangeIterator : RANGE_ITERATOR = functor (I : ITERATOR) -> struct
  module Take = TakeIterator(I)

  type 'a t = 'a Take.t
  exception NoResult

  let create (start : int) (fin : int) (iter : 'a I.t) : 'a t =
    try 
      if start>fin then raise NoResult else 
      let module Utils = IteratorUtilsFn(I) in
      let _ = Utils.advance (start-1) iter in
      Take.create (fin-start+1) iter
    with _-> raise NoResult

  let has_next i = try Take.has_next i with _-> raise NoResult

  let next i = try Take.next i with _ -> raise NoResult
end

