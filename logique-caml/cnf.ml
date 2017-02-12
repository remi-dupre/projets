open Trilean

(* Implémentation des fonctions sur les cnf
 * Le type défini ici n'impose pas le format cnf ('Clause' est équivalent à 'Or' et CNF à 'And')
 * Mais les fonctions supposerons quand même que ce format est respecté
 *)

(* ********** Build a CNF ********** *)

(* Une variable booléene, qui peut être associée à des informations pour les calculs *)
type boolvar = {
	id : int;			(* To ensure unicity *)
	name : string;
	mutable value : Trilean.t;
	mutable index : int (* Only used for calculations *)
}

type formula =
	| CNF of formula list
	| Clause of formula list
	| Var of boolvar
	| Neg of boolvar

let max_id = ref 0
let make_var n =
	incr max_id;
	{id = !max_id ; name = n ; value = U ; index = -1}

(* *********** Extract information ********** *)

(* Évalue le cnf avec les valeurs associées à ses variables respectives *)
let rec eval = function
	| CNF([]) -> T
	| CNF(t::q) -> let v = (eval t) in if v = F then F else (v &&& (eval (CNF(q))))
	| Clause([]) -> F
	| Clause(t::q) -> let v = (eval t) in if v = T then T else (v ||| (eval (Clause(q))))
	| Var(b) -> b.value
	| Neg(b) -> tnot (b.value)

(* Affiche la formule *)
let rec to_string = function
	| CNF([]) -> ""
	| CNF([e]) -> "(" ^ to_string e ^ ")"
	| CNF(t::q) -> "(" ^ (to_string t) ^ ")\n^" ^ (to_string (CNF(q)))
	| Clause([]) -> ""
	| Clause([e]) -> to_string e
	| Clause(t::q) -> (to_string t) ^ "+" ^ (to_string (Clause(q)))
	| Var(b) -> b.name
	| Neg(b) -> "-" ^ b.name

(* Retourne la liste des variables booléenes présentes dans la formule *)
let rec get_vars = function
	| CNF([]) -> []
	| CNF(t::q) -> Tools.disj_merge (get_vars t) (get_vars (CNF(q)))
	| Clause([]) -> []
	| Clause(t::q) -> Tools.disj_merge (get_vars t) (get_vars (Clause(q)))
	| Var(b) -> [b]
	| Neg(b) -> [b]

(* ********** Outputs ********** *)

let rec clause_line = function
	| [] -> "0\n"
	| Var(t)::q -> string_of_int (t.index) ^ " " ^ (clause_line q)
	| Neg(t)::q -> string_of_int (-t.index) ^ " " ^ (clause_line q)
	| _ -> failwith "Not a cnf at cnf.ml:clause_line"

(* Génère un fichier dimacs contenant *)
let make_dimacs = function
	| CNF(clauses) ->
		let vars = get_vars (CNF(clauses)) in
		List.iteri (fun i v -> v.index <- i+1) vars;
		List.fold_left (fun ret clause -> match clause with
			| Clause(l) -> ret ^ (clause_line l)
			| _ -> failwith "Not a CNF"
		) (Printf.sprintf "p cnf %d %d\n" (List.length vars) (List.length clauses)) (List.rev clauses)
	| _ -> failwith "Not a CNF"

