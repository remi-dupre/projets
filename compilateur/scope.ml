module Dic = Map.Make(String);;

let standart_64 = ["malloc"]
let standart_global = ["exception_num" ; "exception_ret" ; "exception_catched"] @ ["stdout"; "stderr"; "NULL"]

type t =
	(* Maximum offset *)
	int ref
	(* Offsets of every vars, the second integer gives the range of the variable :
	 *  - 0 : local variable
	 *  - 1 : defined in a parent scope
	 *  - 2 : global variable *)
	* (int * int) Dic.t
	(* Functions declared in this scope *)
	* unit Dic.t

(* Creates a local scope *)
let empty =
	let dic = ref Dic.empty in
	List.iter (fun name -> dic := Dic.add name (0, 2) (!dic)) standart_global;

	let func = ref Dic.empty in
	List.iter (fun name -> func := Dic.add name () (!func)) standart_64;

	ref 0, !dic, !func

(* Makes a copy of a scope, all vars are now as extern *)
let import_scope scope =
	let make_ext elmt =
		let offset, range = elmt in
		if range = 0 then
			offset, 1
		else
			offset, range
	in
	let max_offset, dic, func = scope in
	let dic = Dic.map make_ext dic in
	max_offset, dic, func

(* Return the curent offset of the scope *)
let get_max_offset scope =
	let max, _, _ = scope in !max

(* Return dic of variables known in the scope *)
let get_dic scope =
	let _, dic, _ = scope in dic

(* For a given variable, returns its range *)
let get_range scope name =
	let dic = get_dic scope in
	let _, range = Dic.find name dic in
	range

(* Return true if 'name' is global *)
let is_global scope name =
	let dic = get_dic scope in
	let _, range = Dic.find name dic in
	range = 2

(* Return dic of the functions *)
let get_func scope =
	let _, _, func = scope in func

(* Make space for n variables unused *)
let shift scope n =
	let offset, dict, func = scope in
	(ref (!offset - (n*8)), dict, func)

(* Returns true if 'name' is defined in the scope *)
let has_func scope name =
	let _, _, func = scope in
	Dic.mem name func

(* Adds the function 'name' in the scope *)
let ins_func name scope =
	let offset = get_max_offset scope in
	let dic = get_dic scope in
	let func = get_func scope in
	if Dic.mem name func then
		failwith "Function already defined"
	else
		let func = Dic.add name () func in
		ref offset, dic, func

(* Adds the variable 'name' to the scope *)
let ins_var name scope =
	let offset = get_max_offset scope in
	let dic = get_dic scope in
	let func = get_func scope in
	if not(Dic.mem name dic) then
		ref (offset-8), (Dic.add name (offset-8, 0) dic), func
	else begin
		let _, range = Dic.find name dic in
		if range = 1 then
			let dic = Dic.remove name dic in
			ref offset, (Dic.add name (offset-8, 0) dic), func
		else
			failwith "defined"
	end

(* Adds a global variable to the scope *)
let ins_global name scope =
	(* TODO : duplicata *)
	let offset = get_max_offset scope in
	let dic = get_dic scope in
	let func = get_func scope in
	let dic = Dic.add name (0, 2) dic in
	ref offset, dic, func

(* Return the number of variables in local scope *)
let count_local_vars scope =
	let counter = ref 0 in
	let check elmt =
		let _, range = elmt in
		if range = 0 then incr counter
	in
	let dic = get_dic scope in
	let _ = Dic.map check dic in
	!counter

(* Returns the offset of a given variable *)
let get_offset name scope =
	(* TODO : globals *)
	let dic = get_dic scope in
	if not(Dic.mem name dic) then
		failwith ("Unbound Value " ^ name)
	else
		let offset, _ = Dic.find name dic in
		offset
