val get_id : string -> int
val magical_ast : (string * string * Cparse.loc_code) list -> Cparse.loc_code option -> Cparse.code
val default_catch : Error.locator -> Cparse.code

val call_finally : Format.formatter -> unit
val catch_finally : Format.formatter -> unit