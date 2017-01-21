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

let get_constant_var phi =
	let vars = get_vars phi in
	List.iteri (fun i v -> v.index <- i) vars;
	let as_var = Array.make (List.length vars) false in
	let as_neg = Array.make (List.length vars) false in
	let rec parcours = function
		| CNF(t::q) -> parcours t ; parcours (CNF(q))
		| Clause(t::q) -> parcours t ; parcours (Clause(q))
		| Var(b) -> as_var.(b.index) <- true
		| Neg(b) -> as_neg.(b.index) <- true
		| _ -> ()
	in
	parcours phi;
	let ret = ref None in
	List.iter (fun b ->
		if not (as_neg.(b.index)) || not (as_var.(b.index))
		then ret := Some(b)
	) vars;
	match !ret with
		| None -> None
		| Some(b) ->
			begin if as_neg.(b.index) then
				b.value <- F
 			else
				b.value <- T
			end;
			Some(b)

let get_max_occur_var phi =
	let vars = get_vars phi in
	List.iteri (fun i v -> v.index <- i) vars;
	let occurs = Array.make (List.length vars) 0 in
	let rec parcours = function
		| CNF(t::q) -> parcours t ; parcours (CNF(q))
		| Clause(t::q) -> parcours t ; parcours (Clause(q))
		| Var(b) -> occurs.(b.index) <- 1 + occurs.(b.index)
		| Neg(b) -> occurs.(b.index) <- 1 + occurs.(b.index)
		| _ -> ()
	in
	parcours phi;
	let nbmax = ref 0 in
	let ret = ref (List.hd vars) in
	List.iter (fun b ->
		if occurs.(b.index) > !nbmax then begin
			nbmax := occurs.(b.index);
			ret := b
		end
	) vars;
	!ret

let select_var phi =
	match get_lonely_var phi with
		| Some(v) -> v
		| None -> match get_constant_var phi with
			| Some(v) -> v
			| None -> get_max_occur_var phi

let rec solve phi =
	let psi = simplifier phi in
	match eval psi with
		| T -> true
		| F -> false
		| U ->
			let jesus = select_var psi in
			if jesus.value = U then begin
				if not ((jesus.value <- T; solve psi) || (jesus.value <- F; solve psi)) then begin
					jesus.value <- U;
					false
				end 
				else true
			end 
			else begin (* value of the chosen one is imposed *)
				if not (solve psi) then begin
					jesus.value <- U;
					false
				end
				else true
			end

