open Trilean
open Cnf

let rec next_config = function
	| [] -> false
	| t::q when t.value = F -> t.value <- T ; true
	| t::q when t.value = T -> t.value <- F ; next_config q
	| _ -> failwith "unexpected undefined trilean"

let solve phi =
	let vars = get_vars phi in
	List.iter (fun var -> var.value <- F) vars;
	while (eval phi <> T) && (next_config vars) do () done;
	(eval phi) = T