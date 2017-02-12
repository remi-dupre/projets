type t = int array array * int array array
val build_sat : t -> Cnf.formula
val extract_grid : Cnf.formula -> bool array array
val debug_grid : bool array array -> unit

