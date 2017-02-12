open Cnf

type dimacs_types =
	| CNF_file of (int * int) (* nb variables * nb clauses *)
	| Picross of (int * int) (* nb colones / nb lignes *)

let rec get_dimacs_type () =
	let entry = Tools.split (read_line ()) in
	let entry = List.filter (fun x -> x <> "") entry in
	match entry with
		| [] -> get_dimacs_type ()
		| t::q when t = "c" -> get_dimacs_type ()
		| t::kind::q when t = "p" && kind = "cnf" ->
			begin match q with
				| nb_vars::nb_clauses::_ -> CNF_file(int_of_string nb_vars, int_of_string nb_clauses)
				| _ -> failwith "error with cnf file"
			end
		| t::kind::q when t = "p" && kind = "picross" ->
			begin match q with
				| nb_col::nb_lin::_ -> Printf.printf "dan \n";Picross(int_of_string nb_col, int_of_string nb_lin)
				| _ -> failwith "error with cnf file"
			end
		| _ -> failwith "unknown dimacs type"

let dimacs () = match get_dimacs_type () with
	| CNF_file(nb_vars, nb_clauses) ->
		let vars = Array.init nb_vars (fun i -> make_var ("P" ^ string_of_int (i+1))) in
		let ret = ref [] in
		for i = 1 to nb_clauses do
			let line = read_line() in
			let nums = Tools.split line in
			let filt = fun s -> try 0 <> int_of_string s with _ -> false in
			let nums = List.filter filt nums in
			let nums = List.map int_of_string nums in
			let nums = List.map (fun id ->
				if id > 0 then
					Var(vars.(id-1))
				else
					Neg(vars.(-id-1))
			) nums in
			ret := Clause(nums)::(!ret)
		done;
		CNF(!ret)
	| _ -> failwith "Not a cf input"

let read_picross () = match get_dimacs_type () with
	| Picross(nb_c, nb_l) ->
		let col = Array.make nb_c (Array.make 0 0) in
		let lin = Array.make nb_l (Array.make 0 0) in
		for i = 0 to nb_c-1 do
			let l_in = ref [] in
			let n_in = ref (-1) in
			while !n_in <> 0 do
				n_in := Scanf.scanf "%d " (fun x -> x);
				if !n_in <> 0 then l_in := !l_in @ [!n_in]
			done;
			col.(i) <- Array.of_list (!l_in)
		done;
		for i = 0 to nb_l-1 do
			let l_in = ref [] in
			let n_in = ref (-1) in
			while !n_in <> 0 do
				n_in := Scanf.scanf "%d " (fun x -> x);
				if !n_in <> 0 then l_in := !l_in @ [!n_in]
			done;
			lin.(i) <- Array.of_list (!l_in)
		done;
		(col, lin)
	| _ -> failwith "Not a picross input"

