(* Merge two sorted list *)
let rec disj_merge l1 l2 = match l1, l2 with
	| [], l -> l
	| l, [] -> l
	| t1::q1, t2::q2 when t1 < t2 -> t1::(disj_merge q1 (t2::q2))
	| t1::q1, t2::q2 when t1 > t2 -> t2::(disj_merge (t1::q1) q2)
	| t1::q1, t2::q2 -> t1::(disj_merge q1 q2)

(* Converts a string to char list *)
let explode s =
	let rec expl i l =
		if i < 0 then
			l
		else
			expl (i - 1) (s.[i] :: l) in
	expl (String.length s - 1) []

(* Converts a list of char to string *)
let implode l =
	let result = String.create (List.length l) in
	let rec imp i = function
		| [] -> result
		| c :: l -> result.[i] <- c; imp (i + 1) l
	in imp 0 l;;


(* Splits s on ' ' chars *)
let split s =
	let rec cut_on_space = function
		| [] -> [[]]
		| t::q when t = ' ' -> []::(cut_on_space q)
		| t::q ->
			let ret = cut_on_space q in 
			match ret with
				| [] -> failwith "unexpected behaviour in Tools:split"
				| tr::qr -> (t::tr)::qr
	in List.map implode (cut_on_space (explode s))
