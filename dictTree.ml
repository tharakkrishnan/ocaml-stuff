(* Dictionary based on Binary Search Trees *)
include Tree

type 'a option = 
	 None
	|Some of 'a 


(* lookup: ('a * 'b) tree -> 'a -> 'b *)

let rec lookup tr a = 
	match tr with
	Br ( ( i, j), l, r) ->	if i = a then Some j
			      	else if a < i then (lookup l a) 
				else (lookup r a)
	|Lf -> None


(* insert: ('a * 'b) tree -> 'a -> 'b -> ('a * 'b) tree *)

let rec insert tr k v =
	match tr with
		Lf -> Br( (k, v), Lf, Lf)
		|Br ((k', v'), l, r) -> 
				if k > k' then Br ((k',v'), l , (insert r k v))
				else if k = k' then Br ((k, v), l, r)
				else Br ((k',v'), (insert l k v), r)


(* print_tuple: (int * string) -> unit *)

let print_tuple (k,v) = 
	print_int k;
	print_string ", ";
	print_string v;
	print_newline ()

(* print_dict_tree: (int * string) tree -> unit *)

let print_dict_tree tr =
	iter print_tuple tr

(* read_dict_entry : (int * string) tree -> (int * string) tree *)

let rec read_dict_entry tr = 
	try
		let i = read_int () in
		if i = 0 then tr else
		let name = read_line () in
		read_dict_entry (insert tr i name)
	with 
		Failure "int_of_string" ->
		print_string "This is not a valid integer. Plese try again";
		print_newline ();
		read_dict_entry tr

(* dict_entry_to_channel: out_channel -> int * string -> unit *)

let dict_entry_to_channel ch (k, v) =
	output_string ch ( string_of_int k );
	output_char ch '\n';
	output_string ch v;
	output_char ch '\n'

(* dict_to_cannel: out_channel -> (int * string) tree -> unit *)

let dict_to_channel ch tr = 
	iter (dict_entry_to_channel ch) tr

(* dict_to_file : string -> (int * string) tree -> unit *)

let dict_to_file fn tr =
	let ch = open_out fn in 
		dict_to_channel ch tr;
	close_out ch

let channel_to_dict_entry ch =
	let number = input_line ch in
		let name = input_line ch in
			( int_of_string  number, name )

let rec channel_to_dict ch = 
	try 
		let (k, v) = channel_to_dict_entry ch in
		insert (channel_to_dict ch)  k v;
	with 
		End_of_file -> Lf

let file_to_dict fn = 
	let ch = open_in fn in
		let tr = channel_to_dict ch in
	close_in ch;
	tr
