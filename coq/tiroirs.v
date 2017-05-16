Require Import Nat.
Require Import List.

Fixpoint count (x : nat) (l : list nat) : nat :=
    match l with
    | nil => 0
    | t :: q => 
        if t =? x then
            1 + count x q
        else
            count x q
    end.

Definition repeats (l : list nat) : Prop :=
    exists x, count x l > 1.

Definition inject l1 l2 : Prop :=
   forall n, exists m, nth n l1 0 = nth m l2 0.
   
Theorem principe_des_tiroirs : forall l1 l2,
    inject l1 l2 /\ length l1 < length l2 -> repeats l2.
Proof.
    admit.
Admitted.

(* ***** *)

Fixpoint remove_l (x : nat) (l : list nat) : list nat :=
    match l with
    | nil => nil
    | t::q =>
        if t =? x then
            remove_l x q
        else
            t :: remove_l x q
    end.

   Search ((_::_) = _).
   
Lemma unicity_norep_first : forall t q, not (repeats (t::q)) -> remove_l t (t::q) = q. 
Proof.
    intros.
    simpl.
    replace (t =? t) with true.
    induction q.
    simpl.
    reflexivity.
    simpl.
    replace (a =? t) with false.
    replace (remove_l t q) with q.
    reflexivity.
    simpl.
    assert (not (repeats (t::q))).
    unfold repeats.
    intro.
    destruct H0.
    unfold repeats in H.
    apply H.
    assert (count x (t::q) > count x (t::a::q)).
    admit.
    exists x.
    admit. (*transitivity*)
    symmetry.
    apply IHq.
    assumption.
    admit.
    admit.
Admitted.



Lemma unicity_norep : forall l, (not (repeats l) -> forall n, S (length (remove_l (nth n l 0) l)) = length l).
Proof.
    induction l.
    unfold repeats.
    simpl.
    intro.
    intro.
    elim H.





Theorem contrapose : forall l1 l2,
    length l1 < length l2 /\ not (repeats l2) -> not (inject l1 l2).

