Require Import List.
Require Import Nat.

(** Implémentation d'un tri insertion *)

Fixpoint insert (x : nat) (l : list nat) : list nat :=
    match l with
    | nil => cons x nil
    | t::q =>
        if Nat.ltb t x then
            t::(insert x q)
        else
            x::t::q
    end.

Fixpoint sort (l : list nat) : list nat :=
    match l with
    | nil => nil
    | t::q => insert t (sort q)
    end.

(** Retourne le nombre d'occurences de x dans une liste *)
Fixpoint count (x : nat) (l : list nat) : nat :=
    match l with
    | nil => 0
    | t::q =>
        if t =? x then
            1 + count x q
        else
            count x q
    end.

Definition is_sorted (l : list nat) : Prop :=
    forall x y, x < y
        -> y < length l
        -> nth x l 0 <= nth y l 0.

Definition permuted (l1 : list nat) (l2 : list nat) : Prop :=
    forall x, count x l1 = count x l2.    


Lemma stag_ins : forall (x a :nat) (l : list nat), (x <> a) -> count a (insert x l) = count a l.
Proof.
    intros b x l Heq.
    destruct l.

    (* Liste vide *)
    simpl.
    replace (b =? x) with false.
    reflexivity.
    pose proof PeanoNat.Nat.eqb_neq b x.
    symmetry.
    apply H.
    assumption.

    (* n::l *)
    simpl.
    Restart.

    intros b x l Hneq.
    assert ((b =? x) = false) as Hneq2.
    pose proof PeanoNat.Nat.eqb_neq b x.
    apply H.
    assumption.
    induction l.

    (* Initialisation *)
    (*)simpl.
    replace (b =? x) with false.
    reflexivity.
    pose proof PeanoNat.Nat.eqb_neq b x.
    symmetry.
    apply H.
    assumption.*)
    simpl.
    replace (b =? x) with false.
    reflexivity.

    (* Induction *)
    simpl.
    compare (a <? b) true.

    (* a < b *)
    intro Hin.
    replace (a <? b) with true.
    simpl.
    compare (a =? x) true.

    intro Heq. (* a = x *)
    replace (a =? x) with true.
    pose proof eq_S (count x (insert b l)) (count x l).
    apply H.
    assumption.

    intro Heq. (* a <> x *)
    assert ((a =? x) = false).
    pose proof Bool.not_true_is_false (a =? x).
    apply H.
    assumption.
    replace (a =? x) with false.
    assumption.

    (* a >= b *)
    intro Hnin.
    assert ((a <? b) = false) as Hin.
    pose proof Bool.not_true_is_false.
    apply H.
    assumption.
    replace (a <? b) with false.
    compare (a =? x) true.

    intro Heq. (* a = x *)
    replace (a =? x) with true.
    simpl.
    replace (b =? x) with false.
    replace (a =? x) with true.
    reflexivity.

    intro Heq. (* a <> x *)
    simpl.
    replace (a =? x) with false.
    replace (b =? x) with false.
    reflexivity.
    pose proof Bool.not_true_is_false.
    symmetry.
    apply H.
    assumption.
Qed.


Lemma augm_ins : forall (x : nat) (l : list nat), count x (insert x l) = S (count x l).
Proof.
    intros x l.
    induction l.

    (* Liste vide *)
    simpl.
    pose proof PeanoNat.Nat.eqb_refl x.
    replace (x =? x) with true.
    reflexivity.

    (* Liste non vide *)
    simpl.
    compare (a <? x) true.
    intro H.
    replace (a <? x) with true.
    destruct H.

    (* if x = a ... *)
    compare x a.
    intro Heq.
    assert (true = (a =? x)) as Heqt.
    pose proof PeanoNat.Nat.eqb_eq a x.
    symmetry in Heq.
    symmetry.
    apply H.
    assumption.

    simpl.
    replace (a =? x) with true.
    pose proof eq_S (count x (insert x l)) (S(count x l)).
    apply H.
    assumption.

    (* if x <> a *)
    intro Heq.
    assert ((a =? x) = false) as Heqf.
    pose proof PeanoNat.Nat.eqb_neq a x.
    apply H.
    pose proof PeanoNat.Nat.neq_sym x a.
    apply H0.
    assumption.

    simpl.
    replace (a =? x) with false.
    assumption.

    (* if a is placed at head of list *)
    intro Hdiff.
    assert ((a <? x) = false) as Hdf.
    pose proof Bool.not_true_is_false (a <? x).
    apply H.
    assumption.

    simpl.
    replace (a <? x) with false.
    simpl.
    replace (a <? x) with false.
    pose proof PeanoNat.Nat.eqb_refl x.
    replace (x =? x) with true.
    reflexivity.
Qed.

Theorem keep_perm : forall (l : list nat), permuted l (sort l).
Proof.
    intros l x.
    induction l.
    
    (* Initialisation *)
    unfold permuted.
    simpl.
    reflexivity.

    (* Induction *)
    unfold permuted.
    unfold permuted in IHl.
    simpl.
    compare x a.

    (* x = a *)
    pose proof augm_ins a (sort l) as Haugm.
    intro Heq.
    assert ((a =? x) = true).
    pose proof PeanoNat.Nat.eqb_eq a x as Heqt.
    symmetry in Heq.
    apply Heqt.
    assumption. 

    replace (a =? x) with true.
    replace x with a.
    replace (count a l) with (count a (sort l)). (* IHl *)
    symmetry.
    assumption.
    replace a with x.
    symmetry.
    assumption.

    (* x <> a *)
    intro Hneq.
    assert ((a =? x) = false).
    pose proof PeanoNat.Nat.eqb_neq a x as Hneqf.
    apply Hneqf.
    pose proof PeanoNat.Nat.neq_sym x a.
    apply H.
    assumption.
    
    replace (a =? x) with false.
    pose proof stag_ins a x (sort l).
    replace (count x l) with (count x (sort l)).
    symmetry.
    apply Hneq.
    assumption.
simpl.

(** Maintenant on va prouver que la liste reste triée *)

Search (nth _ (_::_) _).
Search (In _).
Lemma nth_mon : forall (n t : nat) (q : list nat), nth n q 0 = nth (S n) (t::q) 0.
Proof.
    intros.
    simpl.
    reflexivity.
Qed.

Search (nth _ _ _).

Lemma sort_makes_sorted : forall (l : list nat), is_sorted (sort l).
Proof.
    intro l.
    unfold is_sorted.
    induction l.

    (* initialisation *)
    intros.
    replace (sort nil) with (nil : list nat).
    simpl.
    destruct x.
    destruct y.
    easy.
    easy.
    easy.
    easy.

    (* induction *)
    intros.
    destruct x.
    simpl.






pose proof nth_overflow (nil : list nat) x dd<F3><F3>.

Theorem sort_correct : forall (l : list nat), is_sorted (sort l) /\ same_elements l (sort l).

