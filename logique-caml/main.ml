open Printf
open Dpll;;


let help =
"-> satsol solve [méthode]
  résoud une instance de sat donnée sur l'entrée standart
  méthode : random, max-pres, up (defaut : max-pres)
-> satsol debug
  prend en entrée un dimacs et affiche le cnf associé

-> satsol picross solve
  prend un picross sur l'entrée standart et affiche la grille de la solution
-> satsol picross convert
  prend en entrée un picross et sort le dimacs correspondant (on perd l'association variable / grille)
-> satsole picross debug
  prend en entrée un picross et affiche la formule cnf correspondante
" in
let n = Array.length (Sys.argv) in
if n = 1 then begin
	printf "%s" help
end else match Sys.argv.(1) with
	(* Deal with Dimacs file *)
	| "solve" -> begin
		let phi = Input.dimacs() in
		let res =
			if n <= 2 then
				solve phi		
			else match Sys.argv.(2) with
				| "random" -> Dpll.var_selection := RANDOM ; solve phi
				| "max-pres" -> Dpll.var_selection := MAX_PRES ; solve phi
				| "up" -> Dpll.var_selection := MAX_UP ; solve phi
				| _ -> failwith "unknown argument"
		in if res then printf "SAT\n" else printf "UNSAT\n"
		end
	(* Deal with picross file *)
	| "picross" ->
		let picross = Input.read_picross () in
		let phi = Picross.build_sat picross in
		if n < 3 || Sys.argv.(2) = "solve" then begin
			if not (solve phi) then
				printf "Pas de solution\n"
			else
				Picross.debug_grid (Picross.extract_grid phi)
		end else if n >= 3 then begin match Sys.argv.(2) with
			| "convert" -> printf "%s" (Cnf.make_dimacs phi)
			| "debug" -> printf "%s\n" (Cnf.to_string phi)
		end
	| "debug" ->
		let phi = Input.dimacs () in
		printf "%s\n" (Cnf.to_string phi)
	| _ -> printf "Pas compris\n"

