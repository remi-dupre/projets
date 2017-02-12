open Printf
open Dpll;;


let help =
"-> satsol solve [méthode]
  résoud une instance de sat donnée sur l'entrée standart
  méthode : random, max-pres, up (defaut : max-pres)
-> satsol picross
  prend un picross sur l'entrée standart et affiche la grille de la solution
" in
let n = Array.length (Sys.argv) in
if n = 1 then begin
	printf "%s" help
end else match Sys.argv.(1) with
	| "solve" -> begin
		let phi = Input.dimacs() in
		let res =
			if n <= 1 then
				solve phi		
			else match Sys.argv.(2) with
				| "random" -> Dpll.var_selection := RANDOM ; solve phi
				| "max-pres" -> Dpll.var_selection := MAX_PRES ; solve phi
				| "up" -> Dpll.var_selection := MAX_UP ; solve phi
				| _ -> failwith "unknown argument"
		in if res then printf "SAT\n" else printf "UNSAT\n"
		end
	| "picross" ->
		let picross = Input.read_picross () in
		let phi = Picross.build_sat picross in 
		let grid = Picross.extract_grid phi in
		if not (solve phi) then
			printf "Pas de solution\n"
		else
			Picross.debug_grid (Picross.extract_grid phi)
	| _ -> printf "Pas compris\n"

