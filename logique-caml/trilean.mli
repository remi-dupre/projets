type t = T | F | U

val ( ||| ) : t -> t -> t
val ( &&& ) : t -> t -> t
val tnot : t -> t

val string_of_trilean : t -> string
