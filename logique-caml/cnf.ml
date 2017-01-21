open Trilean

(* ********** Build a CNF ********** *)

type boolvar = {
	name : string;
	mutable value : Trilean.t;
	mutable index : int (* Only used for calculations *)
}

type formula =
	| CNF of formula list
	| Clause of formula list
	| Var of boolvar
	| Neg of boolvar

let make_var n =
	{name = n ; value = U ; index = -1}

(* *********** Extract information ********** *)

let rec eval = function
	| CNF([]) -> T
	| CNF(t::q) -> let v = (eval t) in if v = F then F else (v &&& (eval (CNF(q))))
	| Clause([]) -> F
	| Clause(t::q) -> let v = (eval t) in if v = T then T else (v ||| (eval (Clause(q))))
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

(* ********** ********** *)

let rec get_vars = function
	| CNF([]) -> []
	| CNF(t::q) -> Tools.disj_merge (get_vars t) (get_vars (CNF(q)))
	| Clause([]) -> []
	| Clause(t::q) -> Tools.disj_merge (get_vars t) (get_vars (Clause(q)))
	| Var(b) -> [b]
	| Neg(b) -> [b]
