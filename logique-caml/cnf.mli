type boolvar = {
	name : string;
	mutable value : Trilean.t;
	mutable index : int
}

type formula =
	| CNF of formula list
	| Clause of formula list
	| Var of boolvar
	| Neg of boolvar

val make_var : string -> boolvar
val get_vars : formula -> boolvar list
val eval : formula -> Trilean.t

val to_string : formula -> string
