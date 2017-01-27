open Printf
open Cnf

type t = int array array * int array array

let build_sat pb =
	let vert, horiz = pb in
	let n = Array.length vert in
	let grille = Array.make_matrix (n+2) (n+2) (make_var "vide") in
	for i = 0 to n+1 do
		for j = 0 to n+1 do
			grille.(i).(j) <- make_var (sprintf "g[%d,%d]" (i-1) (j-1))
		done;
	done;

	let clauses = ref [] in
	for i = 0 to n+1 do
		clauses := Clause([Neg(grille.(0).(i))]) (* Borders mustn't be filled*)
			::Clause([Neg(grille.(n+1).(i))])
			::Clause([Neg(grille.(i).(0))])
			::Clause([Neg(grille.(i).(n+1))])
			::(!clauses)
	done;

	for i = 1 to n do
		let nb_seq = Array.length (vert.(i-1)) in
		let begin_at_pos = Array.make_matrix nb_seq (n+2) (make_var "vide") in
		for k = 0 to nb_seq-1 do
			let exists = ref [] in
			for j = 1 to n do
				begin_at_pos.(k).(j) <- make_var (sprintf "start_c%d(%d@%d)" (i-1) k (j-1));
				for l = 0 to k-1 do
					for a = j to n do
						clauses := Clause([Neg(begin_at_pos.(l).(a)) ; Neg(begin_at_pos.(k).(j))]) :: (!clauses)
					done
				done;

				let mm = min (j + vert.(i-1).(k) - 1) (n+1) in
				for a = j to mm do
					clauses := Clause([ Neg(begin_at_pos.(k).(j)) ; Var(grille.(i).(a)) ]) :: (!clauses)
				done;
				clauses := Clause([ Neg(begin_at_pos.(k).(j)) ; Neg(grille.(i).(min (mm+1) (n+1)))]) :: (!clauses);
				clauses := Clause([ Neg(begin_at_pos.(k).(j)) ; Neg(grille.(i).(j-1))]) :: (!clauses);

				exists := Var(begin_at_pos.(k).(j)) :: (!exists)
			done;
			clauses := Clause([ Neg(begin_at_pos.(k).(0)) ]) :: Clause([ Neg(begin_at_pos.(k).(n+1)) ]) :: Clause(!exists) :: (!clauses)
		done;
		for j = 1 to n do
			let starts_need = ref [ Neg(grille.(i).(j)) ] in
			for k = 0 to nb_seq-1 do
				for p = max 0 (j-vert.(i-1).(k)+1) to j do
					starts_need := Var(begin_at_pos.(k).(p)) :: (!starts_need)
				done
			done;
			clauses := Clause(!starts_need) :: (!clauses)
		done
	done;

	for i = 1 to n do
		let nb_seq = Array.length (horiz.(i-1)) in
		let begin_at_pos = Array.make_matrix nb_seq (n+2) (make_var "vide") in
		for k = 0 to nb_seq-1 do
			let exists = ref [] in
			for j = 1 to n do
				begin_at_pos.(k).(j) <- make_var (sprintf "start_l%d(%d@%d)" (i-1) k (j-1));
				for l = 0 to k-1 do
					for a = j to n do
						clauses := Clause([Neg(begin_at_pos.(l).(a)) ; Neg(begin_at_pos.(k).(j))]) :: (!clauses)
					done
				done;

				let mm = min (j + horiz.(i-1).(k) - 1) (n+1) in
				for a = j to mm do
					clauses := Clause([ Neg(begin_at_pos.(k).(j)) ; Var(grille.(a).(i)) ]) :: (!clauses)
				done;
				clauses := Clause([ Neg(begin_at_pos.(k).(j)) ; Neg(grille.(min (mm+1) (n+1)).(i))]) :: (!clauses);
				clauses := Clause([ Neg(begin_at_pos.(k).(j)) ; Neg(grille.(j-1).(i))]) :: (!clauses);

				exists := Var(begin_at_pos.(k).(j)) :: (!exists)
			done;
			clauses := Clause([ Neg(begin_at_pos.(k).(0)) ]) :: Clause([ Neg(begin_at_pos.(k).(n+1)) ]) :: Clause(!exists) :: (!clauses)
		done;
		for j = 1 to n do
			let starts_need = ref [ Neg(grille.(j).(i)) ] in
			for k = 0 to nb_seq-1 do
				for p = max 0 (j-horiz.(i-1).(k)+1) to j do
					starts_need := Var(begin_at_pos.(k).(p)) :: (!starts_need)
				done
			done;
			clauses := Clause(!starts_need) :: (!clauses)
		done
	done;

	let _ = Dpll.solve (CNF(!clauses)) in
	for j = 1 to n do
		for i = 1 to n do
			match grille.(i).(j).value with
			| Trilean.T -> printf "#"
			| Trilean.F -> printf " "
			| Trilean.U -> printf "O"
		done;
		printf "\n";
	done;
	(CNF(!clauses))

