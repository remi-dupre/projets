(* Define the offset for vars in a scope
 *)

type t

val empty : t
val import_scope : t -> t

val get_max_offset : t -> int
val has_func : t -> string -> bool
val is_global : t -> string -> bool

val shift : t -> int -> t
val ins_func : string -> t -> t
val ins_var : string -> t -> t
val ins_global : string -> t -> t
val count_local_vars : t -> int

val get_offset : string -> t -> int
val get_range : t -> string -> int
