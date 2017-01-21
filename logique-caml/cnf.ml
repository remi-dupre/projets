open Trilean

type boolvar = {
	name : string;
	mutable value : Trilean.t
}

type formula =
	| CNF of formula list
	| Clause of formula list
	| Var of boolvar
	| Neg of boolvar

(* *********** *)

let make_var n =
	{name = n ; value = U}

(* *********** *)

let rec eval = function
	| CNF([]) -> T
	| CNF(t::q) -> (eval t) &&& (eval (CNF(q)))
	| Clause([]) -> F
	| Clause(t::q) -> (eval t) ||| (eval (Clause(q)))
	| Var(b) -> b.value
	| Neg(b) -> tnot (b.value)

let rec to_string = function
	| CNF([]) -> ""
	| CNF([e]) ->  "(" ^ to_string e ^ ")"
	| CNF(t::q) -> "(" ^ (to_string t) ^ ")^" ^ (to_string (CNF(q)))
	| Clause([]) -> ""
	| Clause([e]) -> to_string e
	| Clause(t::q) -> (to_string t) ^ "+" ^ (to_string (Clause(q)))
	| Var(b) -> b.name
	| Neg(b) -> "-" ^ b.name

let rec get_vars phi =
	let rec disj_merge l1 l2 = match l1, l2 with
		| [], l -> l
		| l, [] -> l
		| t1::q1, t2::q2 when t1 < t2 -> t1::(disj_merge q1 (t2::q2))
		| t1::q1, t2::q2 when t1 > t2 -> t2::(disj_merge (t1::q1) q2)
		| t1::q1, t2::q2 -> t1::(disj_merge q1 q2)
	in
	match phi with
		| CNF([]) -> []
		| CNF(t::q) -> disj_merge (get_vars t) (get_vars (CNF(q)))
		| Clause([]) -> []
		| Clause(t::q) -> disj_merge (get_vars t) (get_vars (Clause(q)))
		| Var(b) -> [b]
		| Neg(b) -> [b]
