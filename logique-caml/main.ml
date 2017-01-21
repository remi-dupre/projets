open Printf
open Cnf;;

let phi = Input.dimacs () in
(*printf "%s : %b\n" (to_string phi) (Dpll.solve phi)*)
printf "%b\n" (Dpll.solve phi)
