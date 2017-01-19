open List

let rec solve game =
	(* Game is finished when there are no balls *)
	if List.length (Rules.get_balls game) <= 1 then Some([])
	(* Tries a move *)
	else begin
		let l_moves = Rules.moves game in
		let rec test_moves = function
			| [] -> None
			| t::q ->
				let g = Rules.apply_move game t in
				match solve g with
					| None -> test_moves q
					| Some(soluce) -> Some(t::soluce)
		in test_moves l_moves
	end

