Inductive Ord : Set :=
| Zero : Ord
| Succ : Ord -> Ord
| Limit : (nat -> Ord) -> Ord.

(* Définition de w *)

Fixpoint enum_N (n : nat) : Ord :=
    match n with
    | 0 => Zero
    | S k => Succ (enum_N k)
    end.

Definition w : Ord := Limit enum_N.

Fixpoint add (a b : Ord) : Ord :=
    match b with
    | Zero => a
    | Succ b' => Succ (add a b')
    | Limit f =>
        let g (n : nat) : Ord := add a (f n) in
        Limit g
    end.

Fixpoint test (n : nat) : nat :=
    let a := 0 in a.

