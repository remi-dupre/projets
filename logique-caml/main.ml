open Printf
open Cnf;;

(*
let phi = Input.dimacs () in
(*printf "%s : %b\n" (to_string phi) (Dpll.solve phi)*)
printf "%b\n" (Dpll.solve phi)
*)

let debug_sol phi =
	let rec debugvars = function
		| [] -> ()
		| t::q -> printf "%s : %s\n" (t.name) (Trilean.string_of_trilean (t.value)) ; debugvars q
	in debugvars (get_vars phi)
;;

let grille =
	[| [|2|]; [|1|] |],
	[| [|1|]; [|2|] |]
in
let phi = Picross.build_sat grille
in printf "%s\n" (to_string phi) ; printf "%b\n" (Dpll.solve phi) ; debug_sol phi
;;
let grille =
	[| [|1;1|]; [|1|]; [|1;1|] |],
	[| [|1;1|]; [|1|]; [|1;1|] |]
in
let phi = Picross.build_sat grille
in printf "%s\n" (to_string phi) ; printf "%b\n" (Dpll.solve phi) ; debug_sol phi
;;
(*http://www.picross.co.uk/solutionimages/moderate/image140.jpg*)
let grille =
	[| [|3|];[|5|];[|4;3|];[|7|];[|5|];[|3|];[|5|];[|1;8|];[|3;3;3|];[|7;3;2|];[|5;4;2|];[|8;2|];[|10|];[|2;3|];[|6|] |],
	[| [|3|];[|4|];[|5|];[|4|];[|5|];[|6|];[|3;2;1|];[|2;2;5|];[|4;2;6|];[|8;2;3|];[|8;2;1;1|];[|2;6;2;1|];[|4;6|];[|2;4|];[|1|] |]
in
let phi = Picross.build_sat grille
in printf "%s\n" (to_string phi) ; printf "%b\n" (Dpll.solve phi) ; debug_sol phi
