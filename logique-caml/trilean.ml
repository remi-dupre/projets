type t = T | F | U

let ( ||| ) a b = match a, b with
	| T, _ | _, T -> T
	| U, _ | _, U -> U
	| _ -> F

let ( &&& ) a b = match a, b with
	| F, _ | _, F -> F
	| U, _ | _, U -> U
	| _ -> T

let tnot = function
	| T -> F
	| F -> T
	| U -> U

let string_of_trilean = function
	| T -> "T"
	| F -> "F"
	| U -> "?"
