open Trilean
open Cnf

let rec simplifier = function
	| CNF(t::q) when (Cnf.eval t) = T -> simplifier (CNF(q))
	| CNF(t::q) ->
		begin match simplifier (CNF(q)) with
		| CNF(l) -> CNF((simplifier t)::l)
		| _ -> failwith "bug in 'dpll.ml:simplifer"
		end
	| Clause(t::q) when eval t = F -> simplifier (Clause(q))
	| Clause (t::q) ->
		begin match simplifier (Clause(q)) with
		| Clause(l) -> Clause(t::l)
		| _ -> failwith "bug in 'dpll.ml:simplifer"
		end
	| e -> e


let rec get_lonely_var = function
	| CNF([]) -> None
	| CNF(t::q) ->
		let lonely_t = get_lonely_var t in
		if lonely_t <> None then
			lonely_t
		else
			get_lonely_var (CNF(q))
	| Clause(l) -> 
		let litteraux = Cnf.get_vars (Clause(l)) in
		if List.length litteraux = 1 then
			Some(List.nth litteraux 0)
		else
			None
	| _ -> failwith "'get_lonely_var' should only be called on CNF or non-empty clauses"

let select_var phi =
	let lonely = get_lonely_var phi in
	match lonely with
		| Some(v) -> v
		| None -> List.nth (get_vars phi) 0

let rec solve phi =
	let psi = simplifier phi in
	match eval psi with
		| T -> true
		| F -> false
		| U ->
			let jesus = select_var psi in
			if not ((jesus.value <- T; solve psi) || (jesus.value <- F; solve psi)) then begin
				jesus.value <- U;
				false
			end else
				true