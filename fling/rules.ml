(* ___________.__               ________                       
 * \__    ___/|  |__   ____    /  _____/_____    _____   ____  
 *   |    |   |  |  \_/ __ \  /   \  ___\__  \  /     \_/ __ \ 
 *   |    |   |   Y  \  ___/  \    \_\  \/ __ \|  Y Y  \  ___/ 
 *   |____|   |___|  /\___  >  \______  (____  /__|_|  /\___  >
 *	           \/     \/          \/     \/      \/     \/ 
 *)


type direction = Up | Right | Down | Left

type ball = int * Position.t

type move = ball * direction

type game = ball list

(* ************************* *)

let nb_balls = ref 0 (* Number of balls created since the begining *)
let make_ball p =
	incr nb_balls ;
	!nb_balls-1, p

let ball_id b =
	let id, _ = b in
	id

let new_game ps = ps
	
let eq_ball b b' =
	let id, _ = b and id', _ = b' in
	id = id'

(* Gives the unit vector pointing on the direction *)
let dir_vect = function
	| Up -> (Position.from_int 0 1)
	| Down -> (Position.from_int 0 (-1))
	| Left -> (Position.from_int (-1) 0)
	| Right -> (Position.from_int 1 0)
;;

(* Gives the opposite of a vector*)
let rev_vect v =
	let x = Position.proj_x v in
	let y = Position.proj_y v in
	Position.from_int (-x) (-y)

let get_balls g = g

(* Removes the ball of id 'b' from game *)
let rec rm_ball game b =
	let id, _ = b in
	match get_balls game with
		| [] -> new_game []
		| (n, _)::q when n = id -> new_game q
		| t::q -> new_game t::(rm_ball q b)


(* Moves the ball of id 'b' in game *)
let rec mv_ball game b pos =
	let id, _ = b in
	match get_balls game with
		| [] -> new_game []
		| (n, _)::q when n = id -> (id, pos)::q
		| t::q -> t::(mv_ball q b pos)

let position_of_ball b =
	let _, p = b in p

let is_ball game p =
	let rec search = function
		| [] -> false
		| t::q -> Position.eq (position_of_ball t) p || search q
	in 
	let ball_list = get_balls game in
	search ball_list

exception EmptyPos
let ball_of_position game p =
	let rec search = function
		| [] -> raise EmptyPos (* There is no ball on the given coordinate *)
		| t::q -> if Position.eq (position_of_ball t) p then t else search q
	in
	let ball_list = get_balls game in
	search ball_list

let pos_valid = Position.valid (* No courage to search how to 'search and replace' on vim *)

(* ********** MOVES GESTION ********** *)

let make_move b d = (b, d)

exception NoBall
exception NoSpace
let apply_move g move =
	(* Checks that there is a ball in the direction *)
	let (id, pos), dir = move in
	let next = ref (Position.move pos (dir_vect dir)) in
	if is_ball g !next then
		raise NoSpace; (* There is another next to the first position *)
	while not (is_ball g !next) && pos_valid !next do
		next := Position.move !next (dir_vect dir)
	done;
	if not (is_ball g !next) then
		(* There is no ball in the given direction *)
		raise NoBall;

	(* Process consecutive movements *)
	let rec exec_move g move =
		let (id, pos), dir = move in
		let prev = ref pos in
		let next = ref (Position.move pos (dir_vect dir)) in
		while (pos_valid !next) && not (is_ball g !next) do
			prev := !next ;
			next := Position.move (!next) (dir_vect dir)
		done ;
		if not (pos_valid !next) then
			(* The ball went out of bound *)
			rm_ball g (ball_of_position g pos)
		else
			(* The ball has been stoped by another ball *)
			let game = mv_ball g (ball_of_position g pos) !prev in
			
		exec_move game (make_move (ball_of_position g !next) dir)
	in exec_move g move

let moves g =
	(* Lists every direction for each ball *)
	let rec all_moves = function
		| [] -> []
		| t::q -> (make_move t Up)
					::(make_move t Down)
					::(make_move t Left)
					::(make_move t Right)
					::(all_moves q)
	in
	(* Keeps valid moves *)
	let rec filter_moves = function
		| [] -> []
		| t::q ->
			let correct = try let _ = apply_move g t in true with _ -> false in
			if correct then t::(filter_moves q)
			else filter_moves q
	in
	let moves = all_moves (get_balls g) in
	filter_moves moves

