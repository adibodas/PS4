(*Returns a function (call it f) The first time f is called, 
	it will return n. The second time f is called, it should 
	returnn+k. More generally, the i-th time f is called, 
	it should return n+(i-1)*k.*)
let count_up_from n k = 
	let x = ref (n-k) in
	fun () -> (x := !x + k; !x)

(*Initializes an array with n indices, and sets an element at
	index i to the result of evaluating (f i)
	Precondition: n>=0, can't have negative indices*)
let tabulate f n = 
	try Array.init n f with _ -> 
		failwith "Tried to initialize with negative indices."

(*Folds over a list with a function and accumulator,
	just like List.fold_left*)
let fold_left_imp f acc xs =
	let r = ref (acc, xs) in 
	let loop = ref true in
	while !loop do
		match !r with
		| (acc,h::t)	-> r := (f acc h,t)
		| _ -> loop := false 
	done; let (ans,_) = !r in ans

type t = unit 
type u = int
let lst : t list= [();();();();();();()]
(*A disproof of:
		List.map f (List.rev xs) = List.rev (List.map f xs)
	using imperative features
	*)
let zardoz : t -> u = 
	count_up_from 3 4

