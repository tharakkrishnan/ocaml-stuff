(* compute statistics on a text file *)


let print_histogram arr =
	print_string "Character count histogram";
	print_newline();

	for i = 0 to 255 do 
	begin
		if i mod 16 = 0 then print_newline ();
		print_char (char_of_int i);
		print_string "=";
		print_int arr.(i);
		print_string " | "
	end	
	done
	

let channel_statistics in_channel =
	let lines = ref 0 in
	let words = ref 0 in
	let scentences = ref 0 in
	let characters = ref 0 in
	let c_array = Array.make 256 0 in
	
	try 
		while true do
			let line = input_line in_channel in
				lines := !lines + 1;
				String.iter (fun c ->
					match c with
					' ' ->  words := !words + 1; 
						characters := !characters + 1
					|'.' | '?' | '!'  -> 	scentences := !scentences + 1; 
								characters := !characters + 1
					|_ -> characters := !characters + 1
					) line;
				String.iter (fun c->
					c_array.(int_of_char c ) 
					<- c_array.(int_of_char c ) + 1) line
					
		done
	with
		End_of_file -> 	print_string "There were ";
			      	print_int !lines;
		 		print_string " lines, ";
				print_int !scentences;
				print_string " scentences, ";
				print_int !words;
				print_string " words and ";
	 			print_int !characters;
				print_string " characters. ";
				print_newline ();
				print_histogram c_array;
				print_newline ()
					


let filestat fn =

	let in_channel = open_in fn in							
	try
		channel_statistics in_channel;
		close_in in_channel
	with
		_-> close_in in_channel


