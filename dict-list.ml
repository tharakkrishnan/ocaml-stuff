(* Dictionary operations *)

let fst p = match p with (x, _) -> x;;
let snd p = match p with (_, x) -> x;;

let rec lookup x d = 
	match d with
		[] -> raise Not_found
		|(k, v) :: t -> if k = x then v
				else  lookup x t;;
let rec add k v d =
	match d with
		[] -> [(k, v)]
		|(k', v') :: t -> if k' = k then (k, v)::t
				 else (k', v'):: add k v t;;

let rec remove k d =
	match d with 
		[] -> raise Not_found
		|(k', v') :: t -> if k = k' then t
				  else (k', v') :: remove k t;;

let rec key_exists k d =
	match d with
		[] -> false
		|(k', v') :: t -> if k = k' then true
				  else  key_exists k t;;

 

