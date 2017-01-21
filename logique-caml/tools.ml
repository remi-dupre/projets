let rec disj_merge l1 l2 = match l1, l2 with
	| [], l -> l
	| l, [] -> l
	| t1::q1, t2::q2 when t1 < t2 -> t1::(disj_merge q1 (t2::q2))
	| t1::q1, t2::q2 when t1 > t2 -> t2::(disj_merge (t1::q1) q2)
	| t1::q1, t2::q2 -> t1::(disj_merge q1 q2)
