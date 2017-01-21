type formula =
	| CNF of formula list
	| Clause of formula list
	| Var of int
	| Neg of int

let rec eval expr instance = match expr with
	| CNF([]) -> true
	| CNF(t::q) -> (eval t instance) && (eval (CNF(q)) instance)
	| Clause([]) -> false
	| Clause(t::q) -> (eval t instance) && (eval (Clause(q)) instance)
	| Var(i) -> instance.(i)
	| Neg(i) -> not (instance.(i))

let rec to_string = function
	| CNF([]) -> ""
	| CNF([e]) -> to_string e
	| CNF(t::q) -> (to_string t) ^ "^" ^ (to_string (CNF(q)))
	| Clause([]) -> ""
	| Clause([e]) -> to_string e
	| Clause(t::q) -> (to_string t) ^ "+" ^ (to_string (Clause(q)))
	| Var(i) -> string_of_int i
	| Neg(i) -> "-" ^ (string_of_int i)
