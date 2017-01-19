open Cparse
open Scope
open Expr
open Exception

open Printf
open List
open Array

let arg_order = [|"%rdi"; "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9"|]

let nb_if = ref 0
let nb_return = ref 0
let nb_while = ref 0
let nb_try = ref 0

(* ************** Declarations *************** *)

(* Adds a var to the scope and outputs the declaration *)
let rec int_declaration out env loc name =
	try
		let env = Scope.ins_var name env in
		let offset = Scope.get_offset name env in
		Format.fprintf out "pushq	$0	# %s at %d(%%rbp)@," name offset ;
		env
	with _ ->
		Error.error (Some(loc)) ("'"^name^"' is already defined in that scope") ;
		env


(* Declares a function
 * Inputs :
 *  - out, loc, name
 *  - dec_list : arguments of the function
 *  - loc_c : content of the function
 *)
and fun_declaration out env loc name dec_list loc_c =
	let env = try Scope.ins_func name env with
		_ -> Error.error (Some(loc)) "Fonction déjà définie" ; env
	in
	let local_env = ref (Scope.import_scope env) in
	Format.fprintf out "%s:@,@[<v 4>    " name;
	Format.fprintf out "pushq	%%rbp@,";
	Format.fprintf out "movq	%%rsp, %%rbp@,@,";

	(* Prepare for uncatch exception / init global variables *)
	if name = "main" then begin
		Format.fprintf out "cmpq	$1, exception_inited@,";
		Format.fprintf out "je		.EXC_INIT_END@,";
		Format.fprintf out "movq	$0, comp_try@,";
		Format.fprintf out "movq	$0, delayed_ret@,";
		Format.fprintf out "movq	%%rbp, save_try_rsp@,";
		Format.fprintf out "leaq	.EXIT_MAIN, %%rax@,";
		Format.fprintf out "movq	%%rax, save_try_exc@,";
		Format.fprintf out "movq	$0, exception_inited@,";
		Format.fprintf out "@]@,.EXC_INIT_END:@,@[<v 4>    "
	end;

	(* Get arguments *)
	Format.fprintf out "# %d Arguments@," (List.length dec_list);
	for i = 0  to (List.length dec_list) - 1 do
		let loc, name = match List.nth dec_list i with
			| CDECL(loc, name) -> loc, name
			| _ -> failwith "Arguments must be integers" 
		in
		local_env := int_declaration out !local_env loc name;
		if i < Array.length arg_order then
			Format.fprintf out "movq	%s, %d(%%rbp) # (arg %d)@,@," (arg_order.(i)) (Scope.get_offset name !local_env) i 
		else begin
			let n = 16 + 8*(i-(Array.length arg_order)) in (* position in the stack *)
			Format.fprintf out "movq	%d(%%rbp), %%rax # (arg %d)@," n i;
			Format.fprintf out "movq	%%rax, %d(%%rbp)@,@," (Scope.get_offset name !local_env)
		end
	done;

	(* Function content *)
	Format.fprintf out "# %s's content@," name;
	let loc, bloc = loc_c in
	let _ = code out !local_env loc bloc in ();

	(* End of the function *)
	Format.fprintf out "# Leave the function %s@," name;
	Format.fprintf out "leave@,";
	Format.fprintf out "ret@]@,@,";

	(* To return from main with an exception *)
	if name = "main" then begin
		Format.fprintf out "@]@,.EXIT_MAIN:@,@[<v 4>    ";
		Format.fprintf out "movq	%%rsp, %%rbp@,";
		code out env loc (Exception.default_catch loc);
		Format.fprintf out "leave@,";
		Format.fprintf out "ret@,"
	end;

	env


(* Generates assembly for a declaration of local variable or of function*)
and declarations out env dec_list = match dec_list with
	| [] -> env
	| [t] ->
		begin match t with
			| CDECL(loc, name) -> int_declaration out env loc name 
			| CFUN(loc, name, dec_list, loc_c) -> fun_declaration out env loc name dec_list loc_c
		end
	| t::q -> let env = declarations out env [t] in declarations out env q


(* Generates assembly for a bloc of code *)
and code out env loc bloc = match bloc with
	| CBLOCK(decl_l, code_l) ->
		(* Create variables *)
		let local_env = Scope.import_scope env in
		let local_env = declarations out local_env decl_l in
		(* Execution*)
		Format.pp_print_cut out ();
		List.iter (fun (loc, bloc) -> let _ = code out local_env loc bloc in ()) code_l;
		(* Free memory *)
		let n_vars = Scope.count_local_vars local_env in
		Format.fprintf out "addq	$%d, %%rsp	# Free %d variables@," (8*n_vars) n_vars
	| CEXPR((loc, expr)) -> Expr.cmpl out env loc expr ; Format.pp_print_cut out ();
	| CIF((loc_e, e), (loc_c1, c1), (loc_c2, c2)) ->
		let if_id = incr nb_if ; !nb_if in
		Expr.cmpl out env loc_e e;
		Format.fprintf out "cmpq	$1, %%rax@,";
		Format.fprintf out "je		.IF_%d@," if_id;
		code out env loc_c2 c2; (* else {...} *)
		Format.fprintf out "jmp		.ENDIF_%d@]\n@," if_id;
		Format.fprintf out ".IF_%d:@,@[<v 4>    " if_id;
		code out env loc_c1 c1; (* if {...} *)
		Format.fprintf out "@]@,.ENDIF_%d:@,@[<v 4>    " if_id
	| CWHILE((loc_e, e), (loc_c, c)) ->
		let while_id = incr nb_while ; !nb_while in
		Format.fprintf out "@]@,.WHILE_%d:@,@[<v 4>    " while_id;
		Expr.cmpl out env loc_e e;
		Format.fprintf out "cmpq	$0, %%rax@,";
		Format.fprintf out "je		.WHILE_%d_END@," while_id;
		code out env loc_c c;
		Format.fprintf out "jmp		.WHILE_%d@," while_id;
		Format.fprintf out "@]@,.WHILE_%d_END:@,@[<v 4>    " while_id
	| CRETURN(x) ->
		begin match x with
			| Some(loc, expr) -> Expr.cmpl out env loc expr
			| None -> ()
		end;
		Exception.call_finally out;
		Format.fprintf out "leave@,";
		Format.fprintf out "ret@,"
	| CTHROW(name, (loc_e, e)) ->
		Expr.cmpl out env loc_e e;
		Format.fprintf out "movq	%%rax, exception_ret@,";
		Format.fprintf out "movq	$%d, exception_num@," (Exception.get_id name);
		Format.fprintf out "movq	save_try_rsp, %%rsp@,";
		Format.fprintf out "jmp		*save_try_exc@,"
	| CTRY((loc, ccode), l_cas, def_code) ->
		let if_try = incr nb_try ; !nb_try in
		(* Saves environement *)
		Format.fprintf out "addq	$1, comp_try@,";
		Format.fprintf out "pushq	%%rbp@,";
		Format.fprintf out "pushq	save_try_exc@,";
		Format.fprintf out "pushq	save_try_rsp@,";
		Format.fprintf out "movq	%%rsp, save_try_rsp@,";
		Format.fprintf out "leaq	.CATCH_%d, %%rcx@," if_try;
		Format.fprintf out "movq	%%rcx, save_try_exc@,";
		(* Attempt to execute code *)
		let try_env = Scope.shift env 3 in
		code out try_env loc ccode;
		Format.fprintf out "movq	$-1, exception_num@,"; (* As default : only execute the finally *)
		(* Handel exception*)
		Format.fprintf out "@]@,.CATCH_%d:@,@[<v 4>    " if_try;
		Format.fprintf out "movq	$0, exception_catched@,";
		Format.fprintf out "subq	$1, comp_try@,";
		Format.fprintf out "popq	save_try_rsp@,";
		Format.fprintf out "popq	save_try_exc@,";
		Format.fprintf out "popq	%%rbp@,";
		code out env loc (Exception.magical_ast l_cas def_code);
		Exception.catch_finally out;
		(* Relay the exception *)
		Format.fprintf out "cmpq	$1, exception_catched@,";
		Format.fprintf out "je		.CATCHED_%d@," if_try;
		Format.fprintf out "cmpq	$-1, exception_num@,"; (* Don't relay fictive exceptions (for return) *)
		Format.fprintf out "je		.CATCHED_%d@," if_try;
		Format.fprintf out "movq	save_try_rsp, %%rsp@,";
		Format.fprintf out "jmp		*save_try_exc@,";
		Format.fprintf out "@]@,.CATCHED_%d:@,@[<v 4>    " if_try


(* Generates assembly for root declarations *)
let rec cmpl out env dec_list = match dec_list with
	| [] -> ()
	| CFUN(loc, name, dec_list, loc_c)::q ->
		let env = declarations out env [ (CFUN(loc, name, dec_list, loc_c)) ] in
		cmpl out env q
	| CDECL(loc, name)::q ->
		Format.fprintf out ".comm	%s, 8, 8@," name;
		let env = Scope.ins_global name env in
		cmpl out env q
