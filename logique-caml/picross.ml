open Printf
open Cnf
open Tools

type t = int array array * int array array

let build_sat pb =
	let vert, horiz = pb in
	let nc = Array.length vert in
	let nl = Array.length horiz in
	let grille = Array.make_matrix (nc+2) (nl+2) (make_var "vide") in
	for i = 0 to nc+1 do
		for j = 0 to nl+1 do
			grille.(i).(j) <- make_var (sprintf "g %d %d" (i-1) (j-1))
		done;
	done;

	let clauses = ref [] in
	for i = 0 to nc+1 do
		clauses := Clause([Neg(grille.(i).(0))])
			::Clause([Neg(grille.(i).(nl+1))])
			::(!clauses)
	done;
	for i = 0 to nl+1 do
		clauses := Clause([Neg(grille.(0).(i))]) (* Borders mustn't be filled *)
			::Clause([Neg(grille.(nc+1).(i))])
			::(!clauses)
	done;
	for i = 1 to nc do
		let nb_seq = Array.length (vert.(i-1)) in
		let begin_at_pos = Array.make_matrix nb_seq (nl+2) (make_var "vide") in
		for k = 0 to nb_seq-1 do
			let exists = ref [] in
			for j = 1 to nl do
				begin_at_pos.(k).(j) <- make_var (sprintf "start_c%d(%d@%d)" (i-1) k (j-1));
				for l = 0 to k-1 do
					for a = j to nl do
						clauses := Clause([Neg(begin_at_pos.(l).(a)) ; Neg(begin_at_pos.(k).(j))]) :: (!clauses)
					done
				done;

				let mm = min (j + vert.(i-1).(k) - 1) (nc+1) in
				for a = j to mm do
					clauses := Clause([ Neg(begin_at_pos.(k).(j)) ; Var(grille.(i).(a)) ]) :: (!clauses)
				done;
				clauses := Clause([ Neg(begin_at_pos.(k).(j)) ; Neg(grille.(i).(min (mm+1) (nl+1)))]) :: (!clauses);
				clauses := Clause([ Neg(begin_at_pos.(k).(j)) ; Neg(grille.(i).(j-1))]) :: (!clauses);

				exists := Var(begin_at_pos.(k).(j)) :: (!exists)
			done;
			clauses := Clause([ Neg(begin_at_pos.(k).(0)) ]) :: Clause([ Neg(begin_at_pos.(k).(nl+1)) ]) :: Clause(!exists) :: (!clauses)
		done;
		for j = 1 to nc do
			let starts_need = ref [ Neg(grille.(i).(j)) ] in
			for k = 0 to nb_seq-1 do
				for p = max 0 (j-vert.(i-1).(k)+1) to j do
					starts_need := Var(begin_at_pos.(k).(p)) :: (!starts_need)
				done
			done;
			clauses := Clause(!starts_need) :: (!clauses)
		done
	done;
	for i = 1 to nl do
		let nb_seq = Array.length (horiz.(i-1)) in
		let begin_at_pos = Array.make_matrix nb_seq (nc+2) (make_var "vide") in
		for k = 0 to nb_seq-1 do
			let exists = ref [] in
			for j = 1 to nc do
				begin_at_pos.(k).(j) <- make_var (sprintf "start_l%d(%d@%d)" (i-1) k (j-1));
				for l = 0 to k-1 do
					for a = j to nc do
						clauses := Clause([Neg(begin_at_pos.(l).(a)) ; Neg(begin_at_pos.(k).(j))]) :: (!clauses)
					done
				done;
				let mm = min (j + horiz.(i-1).(k) - 1) (nc+1) in
				for a = j to mm do
					clauses := Clause([ Neg(begin_at_pos.(k).(j)) ; Var(grille.(a).(i)) ]) :: (!clauses)
				done;
				clauses := Clause([ Neg(begin_at_pos.(k).(j)) ; Neg(grille.(min (mm+1) (nc+1)).(i))]) :: (!clauses);
				clauses := Clause([ Neg(begin_at_pos.(k).(j)) ; Neg(grille.(j-1).(i))]) :: (!clauses);

				exists := Var(begin_at_pos.(k).(j)) :: (!exists)
			done;
			clauses := Clause([ Neg(begin_at_pos.(k).(0)) ]) :: Clause([ Neg(begin_at_pos.(k).(nc+1)) ]) :: Clause(!exists) :: (!clauses)
		done;
		for j = 1 to nc do
			let starts_need = ref [ Neg(grille.(j).(i)) ] in
			for k = 0 to nb_seq-1 do
				for p = max 0 (j-horiz.(i-1).(k)+1) to j do
					starts_need := Var(begin_at_pos.(k).(p)) :: (!starts_need)
				done
			done;
			clauses := (Clause(!starts_need)) :: (!clauses)
		done
	done;
	(CNF(!clauses))

let extract_grid phi =
	let vars = get_vars phi in
	let nb_l, nb_c =
		let max_x, max_y = ref (-1), ref (-1) in
		List.iter (fun v ->
			let args = split v.name in
			if List.hd args = "g" then begin
				max_x := max (!max_x) (abs (int_of_string (List.nth args 2)));
				max_y := max (!max_y) (abs (int_of_string (List.nth args 1)))
			end
		) vars;
		(!max_x, !max_y)
	in
	let grid = Array.make_matrix nb_c nb_l false in
	List.iter (fun v ->
		let args = split v.name in
		if List.hd args = "g" then
			let i = abs (int_of_string (List.nth args 1)) in
			let j = abs (int_of_string (List.nth args 2)) in
			if v.value = T then
				grid.(i).(j) <- true
	) vars;
	grid

let debug_grid mat =
	let n = Array.length mat in
	let m = Array.length mat.(0) in

	printf "  ";
	for i = 1 to n do printf "_" done;
	printf " \n";

	for j = 0 to m-1 do
		printf " |";
		for i = 0 to n-1 do
			if mat.(i).(j) then printf "#" else printf " ";
		done;
		printf "|\n"
	done;

	printf "  ";
	for i = 1 to n do printf "‾" done;
	printf " \n"

