type direction = Up | Right | Down | Left

type 'a grid

val empty : int -> int -> 'a -> 'a grid
        
val get : 'a grid -> Position.t -> 'a       

val set : 'a grid -> Position.t -> 'a -> 'a grid

val rows : 'a grid -> int

val cols : 'a grid -> int                                            
(* val fold : 'a grid -> direction -> 'b -> Position.t -> 'b*)
                                            
val next_to : 'a grid -> Position.t -> direction -> 'a option

                                                       
