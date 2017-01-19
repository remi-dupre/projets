module G = Graphics
module D = Draw

(* max width of the grid printed *)
let max_x = 15

(* max height of the grid printed *)
let max_y = 15

(* game is a reference to the initial game. *)
let game = ref (Rules.new_game [])

(* return the ball that the player wants to move *)
let rec get_ball game =
  let status = G.wait_next_event [G.Button_down] in
  let (x,y) = (status.G.mouse_x,status.G.mouse_y) in
  let p = D.position_of_coord x y in
  if Rules.is_ball game p then
    begin
      let ball = Rules.ball_of_position game p in
      D.draw_ball ~select:true ball; (* to show which ball has been selected *)
      ball
    end
  else
    get_ball game (* the player has selected an empty cell *)

(* convert the key pressed into a char and call the continuation k on it *)
let get_key_pressed k =
  let status = G.wait_next_event [G.Key_pressed] in
  let key = Char.code status.G.key in
  k (Char.chr key)

(* return the direction choosen by the player *)
let rec get_ball_direction () =
  let dir_of_char c =
    Rules.(
      match c with
      | 'z' -> Some Up
      | 's' -> Some Down
      | 'd' -> Some Right
      | 'q' -> Some Left
      | _ -> None
    )
  in
  get_key_pressed (fun c -> match dir_of_char c with
      | Some (x) -> x
      | None -> get_ball_direction () (* wrong key pressed by the player *)
    )

(* get the next move of the player *)
let get_next_move game =
  let p = get_ball game in
  let d = get_ball_direction () in
  Rules.make_move p d


(* create_game allows the player to create its own game by putting balls over the grid *)
let create_game () =
  D.draw_game max_x max_y (Rules.new_game []);
  let rec add_balls l =
    let status = G.wait_next_event [G.Button_down;G.Key_pressed] in
    if status.G.keypressed = true &&  Char.chr (Char.code status.G.key) = 'e' then
      l
    else
      let (x,y) = (status.G.mouse_x, status.G.mouse_y) in
      let p = D.position_of_coord x y in
      let (x',y') = Position.proj_x p, Position.proj_y p in
      (* balls can not be outside the grid *)
      if 0 <= x' && x' < max_x && 0 <= y' && y' < max_y then
        let ball = Rules.make_ball p in
        D.draw_ball ball;
        add_balls (ball::l)
      else
        add_balls l
  in
  let balls = add_balls [] in
  Rules.new_game balls

(* A menu is a pair of string * f where f is a function of type unit -> unit.
   If the player choose on the menu which function should be called *)
let rec menu = [("solve", solve);("play", play);("generate", generate);("exit", leave)]

and generate () =
	game := Terrain.generate 10;
	loop !game;
	get_key_pressed (fun c -> main (("resolve", resolve)::menu))

(* play allows the player to create a new game, and then try to solve it *)
and play () =
  game := create_game ();
  loop !game;
  get_key_pressed (fun c -> main (("resolve", resolve)::menu))


(* solve allows the player to create a new game and then see if the game can be solved *)
and solve () =
  game := create_game ();
  solver !game

(* loop game loops on the game while their is still moves possible for the player *)
and loop game =
	D.draw_game max_x max_y game;
	if Rules.moves game <> [] then begin
		let move = get_next_move game in
		let game = try Rules.apply_move game move with _ -> game in
		D.draw_game max_x max_y game;
		loop game
	end

(* solver game solve the game if it is possible *)
and solver game  =
  D.draw_game max_x max_y game;
  let moves = Solver.solve game in
  match moves with
  | None ->   D.draw_string "No solution!"; get_key_pressed (fun c -> main menu)
  | Some moves ->
    let g = List.fold_left (fun g m -> D.draw_game max_x max_y g ;
                             D.draw_string "Solved!";
                             get_key_pressed (fun c -> ());
                             Rules.apply_move g m) game moves
    in
    D.draw_game max_x max_y g;
    get_key_pressed (fun c -> main (("resolve", resolve)::menu))

(* replay the previous game *)
and replay () =
  loop !game
(* resolve the preivous game *)
and resolve () =
  solver !game
(* leave the application *)
and leave () =
  D.close_window()

(* get the choice of the player *)
and main l =
  let choice c =
    let i = (int_of_char c) - (int_of_char '0') in
    if 0 <= i && i < List.length l then
      snd (List.nth l i) ()
    else
      main l
  in
  Random.self_init();
  D.init_window();
  D.draw_menu l;
  get_key_pressed choice

(* Reads a file content *)
let load_file f =
	let ic = open_in f in
	let n = in_channel_length ic in
	let s = String.create n in
	really_input ic s 0 n;
	close_in ic;
	s

(* ********** BEGINING ********** *)

let n = Array.length Sys.argv;;

(* Generates a game and displays it as a string *)
if n >= 3 && Sys.argv.(1) = "generate" then
	let n = int_of_string (Sys.argv.(2)) in (* Lets let the exception spread *)
	let g = Terrain.generate n in
	print_string (Terrain.game_to_str g)

(* Opens a file and play the represented game *)
else if n >= 3 && Sys.argv.(1) = "open" then
	let m = load_file Sys.argv.(2) in
	game := Terrain.game_of_str m ;
	D.init_window() ;
	loop !game ;
    get_key_pressed (fun c -> main (("resolve", resolve)::menu)) ;
	main menu

(* Display the main interface *)
else
	main menu
;;
