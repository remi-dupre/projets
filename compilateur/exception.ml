open Cparse

(* Gestion des exceptions :
 * Maintient une "sauvegarde" de l'execution à l'endroit où une éventuelle exception devrait être rattrapée :
 *  - save_try_exc : l'adresse à laquelle jumper quand on retourne une exception
 *  - save_try_rsp : l'adresse du pointeur de pile pour retourner dans le bon environement (avec quelques trucs qui ont été empilés avant, par exemple %rbp)
 * Lorsqu'une exception est lancée :
 *  - Stocke l'id de l'exception dans exception_num
 *  - Stocke la valeur donnée en exception dans exception_ret
 *  - Saute au bon endroit, dépile %rbp
 *  - Dépile les valeurs précédentes de save_try_exc et save_try_rsp (correspondant au try plus haut dans l'ast)
 * Pour gérer les exceptions non récupérées une sorte de try est simulé en modifiant l'AST dans le main
 *)

(* List of the exceptions defined in the code *)
let exceptions_list = ref []
let nb_finally = ref 0

(* Returns an unique id reprensenting the exception *)
let get_id exc_name =
	let rec ins_srch_l name l = match l with
		| [] -> l@[name], 0
		| t::q when t = name -> l, 0
		| t::q -> let retq, n = ins_srch_l name q in t::retq, n+1
	in
	let l, n = ins_srch_l exc_name (!exceptions_list) in
	exceptions_list := l ; n

(* Gives the AST for default catch (used at the end of main) *)
let default_catch loc =
	let rec build_conditions l_exceptions = match l_exceptions with
		| [] -> CBLOCK([], [])
		| exception_name::q ->
			(CIF(
				(loc, CMP(C_EQ, (loc, VAR("exception_num")), (loc, CST(get_id exception_name)))),
				(loc, CBLOCK([], [
					(loc, CEXPR(loc, CALL("printf", [
						(loc, STRING("/!\\ Uncatched exception : %s(%d)\n"));
						(loc, STRING(exception_name));
						(loc, VAR("exception_ret"))
					])))
				])),
				(loc, build_conditions q)
			))
	in build_conditions (!exceptions_list)

(* Redirects to a finally if there is a finally to execute *)
let call_finally out =
	incr nb_finally;
	Format.fprintf out "cmpq	$0, comp_try@,";
	Format.fprintf out "je		.FINALLY_%d_CHECK@," (!nb_finally);
	(* There is a finally to execute *)
	Format.fprintf out "movq	%%rax, backup_ret@,";
	Format.fprintf out "movq	$1, delayed_ret@,";
	Format.fprintf out "movq	$-1, exception_num@,";
	Format.fprintf out "movq	save_try_rsp, %%rsp@,";
	Format.fprintf out "jmp		*save_try_exc@,";
	(* No finally*)
	Format.fprintf out "@]@,.FINALLY_%d_CHECK:@,@[<v 4>    " (!nb_finally)

(* Ends a finally : if a return has been delayed, it is launched *)
let catch_finally out =
	incr nb_finally;
	Format.fprintf out "cmpq	$0, delayed_ret@,";
	Format.fprintf out "je		.FINALLY_%d_RET@," (!nb_finally);
	Format.fprintf out "cmpq	$0, comp_try@,";
	Format.fprintf out "je		.FINALLY_%d_CHECK@," (!nb_finally);
	(* There is a finally to execute*)
	Format.fprintf out "movq	$-1, exception_num@,";
	Format.fprintf out "jmp		*save_try_exc@,";
	(* No finally*)
	Format.fprintf out "@]@,.FINALLY_%d_CHECK:@,@[<v 4>    " (!nb_finally);
	Format.fprintf out "movq	backup_ret, %%rax@,";
	Format.fprintf out "movq	$0, delayed_ret@,";
	Format.fprintf out "leave@,ret@,";
	Format.fprintf out "@]@,.FINALLY_%d_RET:@,@[<v 4>    " (!nb_finally)


(* Gives the ast to catch expressions
 *  -> in the form (if(exception == e1) {....} else if(exception == e2) {....} ....) *)
let magical_ast cond_list default =
	let default_code = match default with
		| None -> CBLOCK([], [])
		| Some(loc, code) -> code
	in
	let rec build_conditions ast_out l_test = match l_test with
		| [] -> ast_out
		| (exception_name, varname, (loc, code))::q ->
			(CIF(
				(loc, CMP(C_EQ, (loc, VAR("exception_num")), (loc, CST(get_id exception_name)))),
				(loc, CBLOCK([], [
					(loc, CBLOCK(
						[ CDECL(loc, varname) ],
						[	(loc, CEXPR(loc, SET_VAR("exception_catched", (loc, CST(1)))));
							(loc, CEXPR(loc, SET_VAR(varname, (loc, VAR("exception_ret")))));
							(loc, code) ]
					));
					(loc, CBLOCK([], []))
				])),
				(loc, build_conditions ast_out q)
			))
	in
	let loc = "generated", 42, 42, 42, 42 in
	CBLOCK([], [
		(loc, build_conditions (CBLOCK([], [])) cond_list);
		(loc, default_code)
	])
