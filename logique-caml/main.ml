open Printf
open Dpll;;

(*
(*
let phi = Input.dimacs () in
(*printf "%s : %b\n" (to_string phi) (Dpll.solve phi)*)
printf "%b\n" (Dpll.solve phi)
*)

(*let grille =
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
printf "%s\n%!" (make_dimacs (Picross.build_sat grille))
;;
(*http://www.picross.co.uk/solutionimages/moderate/image140.jpg*)
(*let grille =
	[| [|3|];[|4|];[|5|];[|4|];[|5|];[|6|];[|3;2;1|];[|2;2;5|];[|4;2;6|];[|8;2;3|];[|8;2;1;1|];[|2;6;2;1|];[|4;6|];[|2;4|];[|1|] |],
	[| [|3|];[|5|];[|4;3|];[|7|];[|5|];[|3|];[|5|];[|1;8|];[|3;3;3|];[|7;3;2|];[|5;4;2|];[|8;2|];[|10|];[|2;3|];[|6|] |]
in
let _ = Picross.build_sat grille in printf "\n%!"
;;
*)*)

let grille =
[| [|17; 3|];
[| 1; 1; 1; 8; |];
[| 2; 1; 15 |];
[| 3; 1; 1; 6 |];
[| 1; 1; 2; 6; 6 |];
[| 5; 3; 4; 1; 4 |];
[| 5; 1; 3; 2 |];
[| 4; 6; 5 |];
[| 2; 9 |];
[| 4; 1; 3; 4 |];
[| 6; 4; 2 |];
[| 1; 5; 4 |];
[| 1; 1; 6; 2 |];
[| 1; 2; 2; 4 |];
[| 1; 2; 2; 2; 2; 1 |];
[| 1; 1; 7 |];
[| 1; 2; 6 |];
[| 1; 4; 1; 6 |];
[| 1; 1; 4 |];
[| 1; 1; 1 |] |]
,
[| [| 5; 3; 2 |];
[| 1; 2; 3; 2; 1 |];
[| 1; 2; 7; 1; 2 |];
[| 2; 1;0 ;2 ;1 |];
[| 1; 2; 2; 1; 1 |];
[| 1; 3; 1; 1 |];
[| 2; 1; 3; 1 |];
[| 1; 4; 3; 2 |];
[| 1; 1; 1; 3 |];
[| 1; 1; 2 |];
[| 1; 1; 1; 3 |];
[| 1; 1; 1; 5 |];
[| 1; 1; 1; 6 |];
[| 3; 2; 4 |];
[| 1; 1; 5 |];
[| 1; 1; 5 |];
[| 1; 2; 4; 2 |];
[| 3; 2; 1; 1 |];
[| 2; 1; 1; 3 ;1 |];
[| 4; 2; 2; 3 |];
[| 4; 1; 1; 7 |];
[| 7; 1; 6; |];
[| 8; 1; 4; |];
[| 6; 1; 1; 4 |];
[| 6; 1; 1; 4 |] |]
in
let _ = (make_dimacs (Picross.build_sat grille)) in printf "%!"
;; *)

if (Array.length (Sys.argv)) >= 2 then begin
	let phi = Input.dimacs() in
	let res = match Sys.argv.(1) with
		| "random" -> Dpll.var_selection := RANDOM ; solve phi
		| "max-pres" -> Dpll.var_selection := MAX_PRES ; solve phi
		| _ -> failwith "unknown argument"
	in if res then printf "SAT\n" else printf "UNSAT\n"
end

