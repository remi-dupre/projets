open List
open Printf
open Random;;
Random.self_init()

(* *************** Map generation *************** *)

(* Places n balls at random positions *)
let rec random_map n =
	let random_pos () =
		let x = Random.int 15 in
		let y = Random.int 15 in
		Position.from_int x y
	in
	let rec in_list e =
		List.exists (fun x -> x = e)
	in

	if n = 0 then []
	else begin
		let map = random_map (n-1) in
		let pos = ref (random_pos ()) in
		while in_list !pos map do (* Tries really hard to place a new ball *)
			pos := random_pos ()
		done;
		!pos::map
	end

let random_game n =
	let map = random_map n in
	Rules.new_game (List.map Rules.make_ball map)

let generate n =
	let g = ref (random_game n) in
	while (Solver.solve !g) = None do
		g := random_game n
	done;
	!g

(* ************** For files **************** *)

(* Return a string representing a game *)
let game_to_str game =
	let r = ref "" in
	for i = 0 to 14 do
		for j = 0 to 14 do
			if Rules.is_ball game (Position.from_int i j)
			then r := !r ^ "o"
			else r := !r ^ "."
		done;
		r := !r ^ "\n"
	done;
	!r

(* Return the game represented by str *)
let game_of_str str =
	(* "Au pire ca crash" -- L.Pluvinage, 42/42/2042  *)
	let l = ref [] in
	for i = 0 to 14 do
		for j = 0 to 14 do
			let c = String.get str (j*16 + i) in (* \n adds a 16th char to the line *)
			if c = 'o' then	l := (Rules.make_ball (Position.from_int i j))::!l
		done
	done;
	Rules.new_game !l

