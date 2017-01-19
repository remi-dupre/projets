open Cparse
open Genlab
open Decl


(* Compiles an AST *)
let compile fstream decl_list =
	let out = Format.formatter_of_out_channel fstream in
	Format.pp_open_vbox out 0;
	
	Format.fprintf out "# C-- compiled@,";
	Format.fprintf out ".text@,.globl	main@,@,";
	Format.fprintf out ".comm	exception_inited, 8, 8		# bool : true if these constants have been set@,";
	Format.fprintf out ".comm	exception_ret, 8, 8			# value given with the throw@,";
	Format.fprintf out ".comm	exception_num, 8, 8			# id of the exception raised@,";
	Format.fprintf out ".comm	comp_try, 8, 8				# number of try containing current line@,";
	Format.fprintf out ".comm	exception_catched, 8, 8		# bool : false if an exception is being raised@,";
	Format.fprintf out ".comm	delayed_ret, 8, 8			# bool : true if the code is trying to exit a function@,";
	Format.fprintf out ".comm	backup_ret, 8, 8			# value that has to be returned@,";
	Format.fprintf out ".comm	save_try_rsp, 8, 8			# rsp of last try's environement@,";
	Format.fprintf out ".comm	save_try_exc, 8, 8			# position of last try@,@,";
	
	Decl.cmpl out (Scope.empty) decl_list;
	Format.pp_close_box out ();
	Format.pp_print_flush out ()
