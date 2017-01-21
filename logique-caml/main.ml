open Printf;;
open Cnf;;
open Trilean;;

let a = make_var "A" in
let b = make_var "B" in
let phi = CNF([
	Clause([Var(a); Neg(b)]) ;
	Clause([Neg(a)]) ; 
	Clause([Var(a); Var(b)])
]) in
printf "%s : %b\n" (to_string phi) (Naif.solve phi)
