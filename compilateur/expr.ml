open Cparse

open List
open Array
open String


let arg_order = [|"%rdi"; "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9"|]

let nb_comp = ref 0
let nb_tern = ref 0
let nb_str = ref 0

(* (Just for fancy outputs)
 * Returns a string of C that gives a given expression's AST
 *)
let rec str_of_expr = function
	| VAR(name) -> name
	| CST(x) -> Printf.sprintf "%d" x
	| STRING(str) -> Printf.sprintf "\"%s\"" (String.escaped str)
	| SET_VAR(name, (_, expr)) -> Printf.sprintf "(%s = %s)" name (str_of_expr expr)
	| SET_ARRAY(name, (_, e1), (_, e2)) -> Printf.sprintf "(%s[%s] = %s)" name (str_of_expr e1) (str_of_expr e2)
	| CALL(name, args) -> name ^ "(" ^ (List.fold_left (fun str (_, expr) -> str ^ (if str = "" then "" else ", ") ^ (str_of_expr expr)) "" args) ^ ")"
	| OP1(op, (_, expr)) -> begin match op with
		| M_MINUS -> "-" ^ (str_of_expr expr)
		| M_NOT -> "!" ^ (str_of_expr expr)
		| M_PRE_INC -> "++" ^ (str_of_expr expr)
		| M_POST_INC -> (str_of_expr expr) ^ "++"
		| M_PRE_DEC -> "--" ^ (str_of_expr expr)
		| M_POST_DEC -> (str_of_expr expr) ^ "--"
		end
	| OP2(op, (_, e1), (_, e2)) -> begin match op with
		| S_MUL -> Printf.sprintf "(%s * %s)" (str_of_expr e1) (str_of_expr e2)
		| S_DIV -> Printf.sprintf "(%s / %s)" (str_of_expr e1) (str_of_expr e2)
		| S_MOD -> Printf.sprintf "(%s %% %s)" (str_of_expr e1) (str_of_expr e2)
		| S_ADD -> Printf.sprintf "(%s + %s)" (str_of_expr e1) (str_of_expr e2)
		| S_SUB -> Printf.sprintf "(%s - %s)" (str_of_expr e1) (str_of_expr e2)
		| S_INDEX -> Printf.sprintf "%s[%s]" (str_of_expr e1) (str_of_expr e2)
		end
	| CMP(op, (_, e1), (_, e2)) -> begin match op with
		| C_LT -> Printf.sprintf "(%s < %s)" (str_of_expr e1) (str_of_expr e2)
		| C_LE -> Printf.sprintf "(%s <= %s)" (str_of_expr e1) (str_of_expr e2)
		| C_EQ -> Printf.sprintf "(%s == %s)" (str_of_expr e1) (str_of_expr e2)
		end
	| EIF((_, e1), (_, e2), (_, e3)) -> Printf.sprintf "(%s ? %s : %s)"  (str_of_expr e1) (str_of_expr e2) (str_of_expr e3)
	| ESEQ([(_, e)]) -> str_of_expr e
	| ESEQ(_) -> ""

(* *************** Compile an expression *************** *)

(* Generates assembly to calculate expr and puts the result in %rax 
 * Input :
 *  - out, env, loc
 *  - cmp_op : operator
 *)
let rec cmpl out env loc expr = begin match expr with
	| VAR(name) when Scope.is_global env name ->
		Format.fprintf out "movq	%s, %%rax@," name
	| VAR(name) ->
		Format.fprintf out "movq	%d(%%rbp), %%rax	# get %s@," (Scope.get_offset name env) name
	| CST(x) -> Format.fprintf out "movq	$%d, %%rax@," x
	| STRING(str) ->
		incr nb_str;
		Format.fprintf out "jmp	.STRING_END_%d@," !nb_str;
		Format.fprintf out ".STRING_%d:@," !nb_str;
		Format.fprintf out "	.string \"%s\"@," (String.escaped str);
		Format.fprintf out "	.align 16@,";
		Format.fprintf out ".STRING_END_%d:@," !nb_str;
		Format.fprintf out "movq	$.STRING_%d, %%rax@," !nb_str
	| SET_VAR(name, (loc_expr, expr)) when Scope.is_global env name ->
		cmpl out env loc_expr expr;
		Format.fprintf out "movq	%%rax, %s	# %s -> %s@,@," name (str_of_expr expr) name
	| SET_VAR(name, (loc_expr, expr)) ->
		cmpl out env loc_expr expr;
		Format.fprintf out "movq	%%rax, %d(%%rbp)	# %s -> %s@,@," (Scope.get_offset name env) (str_of_expr expr) name
	| SET_ARRAY(name, (l_e1, e1), (l_e2, e2)) ->
		cmpl out env l_e1 e1;
		Format.fprintf out "pushq	%%rax@,";
		cmpl out env l_e2 e2;
		Format.fprintf out "popq	%%rbx@,";
		if Scope.is_global env name then
			Format.fprintf out "movq	%s, %%rcx	# %s@," name name
		else
			Format.fprintf out "movq	%d(%%rbp), %%rcx	# %s@," (Scope.get_offset name env) name;
		Format.fprintf out "movq	%%rax, (%%rcx, %%rbx, 8)	# %s -> %s[%s]@," (str_of_expr e2) name (str_of_expr e1)
	| CALL(name, args) ->
		let ret64 = Scope.has_func env name in (* Returns in 64 bits if ret64 *)
		let n = List.length args in
		let nb_push = max 0 (n - (Array.length arg_order)) in (* Number of args that will be sent with the stack *)
		Format.fprintf out "pushq	comp_try@,";
		Format.fprintf out "movq	$0, comp_try@,";
		Format.fprintf out "# Call %s with %d arguments (%d bits)@," name n (if ret64 then 64 else 32);
		(* Process argumenst, stores them in the stack *)
		for i = 0 to n-1 do
			let (e_loc, e) = List.nth args (n-i-1) in
			cmpl out env e_loc e;
			Format.fprintf out "pushq	%%rax	# stores %s's arg %d : %s@," name (n-i-1) (str_of_expr e)
		done;
		(* Recover arguments *)
		for i = 0 to min ((Array.length arg_order)-1) (n-1) do
			Format.fprintf out "popq	%s	# arg %d@," (arg_order.(i)) i
		done;
		Format.fprintf out "movq	$0, %%rax@,";
		Format.fprintf out "call	%s@," name;
		if not ret64 then
			Format.fprintf out "movslq	%%eax, %%rax	# 32-bits function@,";
		Format.fprintf out "addq	$%d, %%rsp	# free %s's args@," (8*nb_push) name;
		Format.fprintf out "popq	comp_try@,"
	| OP1(mon_op, (loc_e, expr)) ->
		cmpl out env loc_e expr;
		cmpl_op1 out env mon_op loc;
		begin match mon_op with (* Deal with pre-incrementation *)
			| M_PRE_INC | M_POST_INC ->
				begin match expr with
					| VAR(name) when Scope.is_global env name ->
						Format.fprintf out "addq	$1, %s@," name
					| VAR(name) ->
						Format.fprintf out "addq	$1, %d(%%rbp)	# increment %s@," (Scope.get_offset name env) name
					| OP2(S_INDEX, (l1, e1), (l2, e2)) ->
						Format.fprintf out "pushq	%%rax@,";
						cmpl out env l2 e2;
						Format.fprintf out "pushq	%%rax@,";
						cmpl out env l1 e1;
						Format.fprintf out "popq	%%rbx@,";
						Format.fprintf out "addq	$1, (%%rax, %%rbx, 8)@,";
						Format.fprintf out "popq	%%rax@,"
					| _ -> Error.error (Some(loc)) "Can't increment or decrement this kind of expression."
				end
			| M_PRE_DEC | M_POST_DEC ->
				begin match expr with
					| VAR(name) when Scope.is_global env name ->
						Format.fprintf out "subq	$1, %s@," name
					| VAR(name) ->
						Format.fprintf out "subq	$1, %d(%%rbp)	# increment %s@," (Scope.get_offset name env) name
					| OP2(S_INDEX, (l1, e1), (l2, e2)) ->
						cmpl out env l2 e2;
						Format.fprintf out  "pushq	%%rax@,";
						cmpl out env l1 e1;
						Format.fprintf out "popq	%%rbx@,";
						Format.fprintf out "subq	$1, (%%rax, %%rbx, 8)@,"
					| _ -> Error.error (Some(loc)) "Can't increment or decrement this kind of expression."
				end
			| _ -> ()
		end
	| OP2(bin_op, (loc_e1, e1), (loc_e2, e2)) ->
		cmpl out env loc_e2 e2;
		Format.fprintf out "pushq	%%rax@,";
		cmpl out env loc_e1 e1;
		(* Format.fprintf out "movq	%%rax, %%rbx@,"; *) (* It seems that some are happier when applied from left to right *) (* Finaly it seems not *)
		Format.fprintf out "popq	%%rbx@,";
		cmpl_op2 out env bin_op loc
	| CMP(cmp_op, (loc1, e1), (loc2, e2)) ->
		cmpl out env loc2 e2;
		Format.fprintf out "pushq	%%rax@,";
		cmpl out env loc1 e1;
		Format.fprintf out "popq	%%rbx@,";
		cmpl_cmp out env cmp_op loc
	| EIF((l1, e1), (l2, e2), (l3, e3)) ->
		let tern_id = incr nb_tern ; !nb_tern in
		cmpl out env l1 e1;
		Format.fprintf out "cmpq	$1, %%rax@,";
		Format.fprintf out "je		.TER_%d@," tern_id;
		cmpl out env l3 e3; (* else {...} *)
		Format.fprintf out "jmp		.ENDTER_%d@]@," tern_id;
		Format.fprintf out ".TER_%d:@,@[<v 4>    " tern_id;
		cmpl out env l2 e2; (* if {...} *)
		Format.fprintf out "@]@,.ENDTER_%d:@,@[<v 4>    " tern_id
	| ESEQ((loc, expr)::q) -> cmpl out env loc expr ; cmpl out env loc (ESEQ(q))
	| ESEQ([]) -> ()
end;
Format.fprintf out "# %s@," (str_of_expr expr)

(* Generates assembly for a compare operation %%rax . %%rbx*)
and cmpl_cmp out env cmp_op loc =
	Format.fprintf out "cmpq	%%rbx, %%rax@,"; (* Strange thing : cmp a b process b . a *)
	Format.fprintf out "movq	$1, %%rax@,";
	let comp_id = incr nb_comp ; !nb_comp in
	begin match cmp_op with
		| C_LT ->
			Format.fprintf out "jl	.LT_%d@," comp_id;
			Format.fprintf out "movq	$0, %%rax@]@,";
			Format.fprintf out ".LT_%d:@,@[<v 4>    " comp_id
		| C_LE ->
			Format.fprintf out "jle	.LE_%d@," comp_id;
			Format.fprintf out "movq	$0, %%rax@]@,";
			Format.fprintf out ".LE_%d:@,@[<v 4>    " comp_id
		| C_EQ ->
			Format.fprintf out "je	.EQ_%d@," comp_id;
			Format.fprintf out "movq	$0, %%rax@]@,\n";
			Format.fprintf out ".EQ_%d:@,@[<v 4>    " comp_id
	end

(* Generates assembly to process monoargument-operation on %%rax (without side effects) *)
and cmpl_op1 out env mon_op loc = match mon_op with
	| M_MINUS -> Format.fprintf out "negq	%%rax@,"
	| M_NOT -> Format.fprintf out "notq	%%rax@,"
	| M_PRE_INC -> Format.fprintf out "addq	$1, %%rax	# pre-incrementation@,"
	| M_PRE_DEC -> Format.fprintf out "subq	$1, %%rax	# pre-decrementation@,"
	| M_POST_INC | M_POST_DEC -> ()

(* Generates assembly to process biargument-operation : %rax . %rbx *)
and cmpl_op2 out env bin_op loc = match bin_op with
	| S_MUL -> Format.fprintf out "imulq	%%rbx, %%rax@,"
	| S_DIV ->
		Format.fprintf out "cqto@,";
		Format.fprintf out "idivq	%%rbx@,"
	| S_MOD ->
		Format.fprintf out "cqto@,";
		Format.fprintf out "idivq	%%rbx@,";
		Format.fprintf out "movq	%%rdx, %%rax@,"
	| S_ADD -> Format.fprintf out "addq	%%rbx, %%rax@,"
	| S_SUB ->
		Format.fprintf out "subq	%%rbx, %%rax@,"
	| S_INDEX -> Format.fprintf out "movq	(%%rax, %%rbx, 8), %%rax@,"
