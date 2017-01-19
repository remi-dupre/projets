type t = int * int

let from_int x y = (x,y)

let proj_x = fst

let proj_y = snd

let eq x y = x = y

let move (x,y) (x',y') = (x+x',y+y')

let string_of_position (x,y) = Printf.sprintf "(%s,%s)" (string_of_int x) (string_of_int y)

(* Check if a given position exists *)
let valid p =
	let x = proj_x p in
	let y = proj_y p in
	x >= 0 && y >= 0 && x < 15 && y < 15

