open Printf
open Cnf;;

(*
let phi = Input.dimacs () in
(*printf "%s : %b\n" (to_string phi) (Dpll.solve phi)*)
printf "%b\n" (Dpll.solve phi)
*)

let grille =
	[| [|2|]; [|2|] |],
	[| [|1|]; [|2|]; [|1|] |]
in
let _ = Picross.build_sat grille in printf "\n%!"
;;
let grille =
	[| [|1;1|]; [|1|]; [|1;1|] |],
	[| [|1;1|]; [|1|]; [|1;1|] |]
in
let _ = Picross.build_sat grille in printf "\n%!"
;;
let grille =
	[| [|2|];[|5|];[|3;1;1|];[|3;2|];[|3;2|];[|3;1;1|];[|5|];[|2|] |],
	[| [|1;1|]; [|2;2|];[|6|];[|1;2;1|];[|8|];[|2;2|];[|4|];[|2|]|]
in
let _ = Picross.build_sat grille in printf "\n%!"
;;
(*http://www.picross.co.uk/solutionimages/moderate/image140.jpg*)
(*let grille =
	[| [|3|];[|4|];[|5|];[|4|];[|5|];[|6|];[|3;2;1|];[|2;2;5|];[|4;2;6|];[|8;2;3|];[|8;2;1;1|];[|2;6;2;1|];[|4;6|];[|2;4|];[|1|] |],
	[| [|3|];[|5|];[|4;3|];[|7|];[|5|];[|3|];[|5|];[|1;8|];[|3;3;3|];[|7;3;2|];[|5;4;2|];[|8;2|];[|10|];[|2;3|];[|6|] |]
in
let _ = Picross.build_sat grille in printf "\n%!"
;;
*)