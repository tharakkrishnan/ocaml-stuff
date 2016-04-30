(* List operations *)
let rec length_helper l n =
	match l with
		h::t -> length_helper t (n+1)
		|_ -> n

let len l = length_helper l 0

let rec rev l = 
	match l with
		[] -> []
		| h::t -> rev t @ [h]

let rec take n l = 
	match l with
		h::t -> if n < 0 then 
				raise (Invalid_argument "take" )
			else if n = 0 then []
			else  h:: take (n-1) t
		|[] -> []

let rec drop n l = 
	if n = 0 then l 
	else match l with
		h::t -> if n < 0 then 
				raise (Invalid_argument "drop" )
			else drop (n-1) t
		|[] -> []
			
let rec append l m =
	match l with
		[] -> m
		|h::t -> h :: append t m
let isempty l =
	match l with
		[] -> true
		|_ -> false

let rec merge cmp x y =
	match x, y with
		[],l -> l
		|l,[] -> l
		|hx::tx, hy::ty ->
			if cmp hx hy 
			then hx::merge cmp tx (hy::ty)
			else hy::merge cmp (hx::tx) ty

let rec msort cmp l =
	match l with 
		[] -> []
		|[a] -> l
		|_ -> let left = take (len l / 2) l in
	 		let right = drop (len l / 2) l in
				merge cmp (msort cmp left) (msort cmp right) 

let rec map f l =
	match l with
	[] -> []
	|h :: t -> f h :: map f t


let rec filter f l =
	match l with
	[] -> []
	|h :: t -> if f h = true then 
			h :: filter f t
		   else filter f t 


let rec forall f l =
	match l with
	[] -> false
	|[a] -> if (f a = true) then true else false
	|h :: t -> if f h = true then
			forall f t
		   else false
		
let rec iter f l =
	match l with
		[] -> ()
		|h :: t -> f h; iter f t

let print_element k = 
	print_int k; 
	print_newline () 

let print_list l =
	iter print_element l
 
