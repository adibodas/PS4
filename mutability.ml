let count_up_from n k = 
	let x = ref (n-k) in
	fun () -> (x := !x + k; !x)

let tabulate f n = Array.init n f

let fold_left_imp f acc xs = (*How do you do this without loops?*)
	let ans = ref acc in 
	for i=0 to List.length xs - 1 do 
		ans := f (!ans) (List.nth xs i) 
	done; !ans

type t = unit 
type u = int
let lst : t list= [();();();();();();()]
let zardoz : t -> u = 
	count_up_from 3 4

