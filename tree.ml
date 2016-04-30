(* Operations on trees *)

type 'a tree = 
	Br of 'a * 'a tree * 'a tree
	| Lf

let rec size tr =
	match tr with
		Br (_, l, r) -> 1 + size l + size r
		| Lf -> 0

let rec total tr = 
	match tr with
		Br (x, l, r) -> x + total l + total r
		| Lf -> 0

let max x y = 
	if x > y then x else y

let rec maxdepth tr = 
	match tr with
		Br( _, l, r ) -> 1 + max (maxdepth l) (maxdepth r)
		|Lf -> 0

let rec list_of_tree tr =
	match tr with 
		Br ( x, l, r ) -> (list_of_tree l) @ [x] @ (list_of_tree r) 
		|Lf -> []

let rec tree_map f tr = 
	match tr with
		Br ( x, l, r ) ->  Br ( f x, tree_map f l, tree_map f r ) 
		| Lf -> Lf

let rec iter f tr =
	match tr with
		Lf -> ()
		|Br ( x, l, r ) -> iter f l; f x; iter f r
 
let print_entry k =
	print_int k;
	print_newline ()

let print_tree tr =
	iter print_entry tr
