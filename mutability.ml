let count_up_from n k = 
	let x = ref (n-k) in
	fun () -> (x := !x + k; !x)

let tabulate f n = Array.init n f

let fold_left_imp f acc xs = (*How do you do this without loops?*)
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
let zardoz : t -> u = 
	count_up_from 3 4

