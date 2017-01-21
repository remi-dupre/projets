open Printf;;
open Cnf;;
open Trilean;;

let phi = Input.dimacs () in
printf "%s : %b\n" (to_string phi) (Naif.solve phi)
