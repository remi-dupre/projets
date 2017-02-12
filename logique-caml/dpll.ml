open Trilean
open Cnf

(* A general constant to choose selection method *)
type var_selection_enum = RANDOM | MAX_PRES | MAX_UP
let var_selection = ref MAX_PRES

(* Supprime les clauses évaluées à vrai (en triléen) et les variables évaluées à faux *)
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

(* Retourne une variable qui est toute seule dans sa clause, ou None s'il n'en existe pas *)
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

(* Retourne une variable qui apparait toujours en négation ou toujours positivement et la fixe à la valeur qui valide ses clauses *)
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

(* ********** Var Selection ********** *)

(* Sélectionne un pivot aléatoire *)
let get_random_var phi =
	let vars = get_vars phi in
	let i = Random.int (List.length vars) in
	List.nth vars i

(* Sélectionne la variable qui apparait le plus dans la formule *)
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

(* Sélectionne la variable qui laisserait le plus de clauses unitaire après avoir été supprimée *)
let get_up_var phi =
	let rec count_up var phi =
		let with_neg = ref 0 in
		let with_pos = ref 0 in
		match phi with
			| CNF(l) -> List.iter (fun clause ->
					match clause with
						| Clause(l) ->
							if List.length l == 2 then begin
								if List.mem (Var(var)) l then
									incr with_pos
								else if List.mem (Neg(var)) l then
									incr with_neg
							end
						| _ -> failwith "Not a CNF at dpll.ml:get_up_vars"
				) l ; max (!with_neg) (!with_pos)
			| _ -> failwith "Not a CNF at dpll.ml:get_up_vars"
	in
	let max_var = ref (List.hd (get_vars phi)) in
	let max_up = ref 0 in
	List.iter (fun var ->
		let nb = count_up var phi in
		if nb > !max_up then begin
			max_up := nb;
			max_var := var
		end
	) (get_vars phi);
	!max_var

(* Selectionne un pivot suivant la méthode définie par 'var_selection' *)
let select_var phi =
	match get_lonely_var phi with
		| Some(v) 	-> v
		| None -> match get_constant_var phi with
			| Some(v) -> v
			| None -> match !var_selection with
				| RANDOM -> get_random_var phi
				| MAX_PRES -> get_max_occur_var phi
				| MAX_UP -> get_up_var phi

(* ********** Solve a SAT formula ********* *)

(* Décide de la satisfiabilité d'une formule *)
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

