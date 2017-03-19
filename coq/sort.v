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

Fixpoint lmin (l : list nat) : nat :=
    match l with
    | nil => 0
    | t::q => if (t <? lmin q) then t else lmin q
    end.

Inductive is_sorted3 : list nat -> Prop :=
| is_sorted_nil : is_sorted3 nil
| is_sorted_ind : forall t q, t <= lmin q /\ is_sorted3 q -> is_sorted3 (t::q)
.

Inductive is_sorted2 : list nat -> Prop :=
| is_sorted_nil : is_sorted2 nil
| is_sorted_sin : forall n, is_sorted2 (n::nil)
| is_sorted_ind : forall n m q, n <= m /\ is_sorted2 (m::q) -> is_sorted2 (n::m::q)
.

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

Lemma nth_mon : forall (n t : nat) (q : list nat), nth n q 0 = nth (S n) (t::q) 0.
Proof.
    intros.
    simpl.
    reflexivity.
Qed.


Lemma min_correct : forall t q, lmin (t::q) <= t /\ lmin (t::q) <= lmin q.
Proof.
    intros.
    split.
    simpl.
    compare (t <? lmin q) true.
    intro.
    replace (t <? lmin q) with true.
    reflexivity.
    intro.
    assert ((t <? lmin q) = false).
    destruct (t <? lmin q).
    easy.
    easy.
    replace (t <? lmin q) with false.
    pose proof PeanoNat.Nat.ltb_ge t (lmin q).
    apply H0.
    assumption.
    
    simpl.
    destruct (t <? lmin q) eqn : H.
    pose proof PeanoNat.Nat.ltb_lt t (lmin q).
    apply H0.
    assert (t < lmin q).
    apply H0.
    assumption.
    pose proof PeanoNat.Nat.lt_le_incl.
    apply H2.
    assumption.
    reflexivity.
Qed.

Lemma stab_ins : forall q t x, x < t /\ x < lmin q -> x < lmin (insert t q).
Proof.
    induction q.
    intros.
    easy.
    intros.
    simpl.
    destruct (a <? t) eqn : G.
    simpl.
    destruct (a <? lmin (insert t q)) eqn : F.
    pose proof min_correct a q.
    pose proof Gt.le_gt_trans a (lmin (a::q)) x.
    apply H1.
    apply H0.
    apply H.
    apply IHq.
    split.
    apply H.
    pose proof min_correct a q.
    pose proof Gt.le_gt_trans (lmin q) (lmin (a::q)) x.
    apply H1.
    apply H0.
    apply H.
    simpl.
    destruct (a <? lmin q).
    destruct (t <? a) eqn : F.
    apply H.
    assert (a <= t).
    assert ((a <=? t) = true).
    pose proof PeanoNat.Nat.ltb_antisym a t.
    replace (a <=? t) with (negb (t <? a)).
    replace (t <? a) with false.
    easy.
    destruct (t <? a).
    simpl.
    easy.
    simpl.
    replace true with (negb false).
    replace false with (negb (a <=? t)).
    pose proof Bool.negb_involutive (a <=? t).
    apply H1.
    easy.
    pose proof PeanoNat.Nat.leb_le a t.
    apply H1.
    assumption.
    assert (a = t).
    assert ((a <= t) /\ (a >= t)).
    split.
    pose proof PeanoNat.Nat.leb_le a t.
    apply H1.
    pose proof PeanoNat.Nat.leb_antisym t a.
    replace (a <=? t) with (negb (t <? a)).
    replace (t <? a) with false.
    easy.
    pose proof PeanoNat.Nat.leb_le t a.
    apply H1.
    pose proof PeanoNat.Nat.leb_antisym a t.
    replace (t <=? a) with (negb (a <? t)).
    replace (a <? t) with false.
    easy.
    pose proof PeanoNat.Nat.le_antisymm a t.
    apply H2.
    apply H1.
    apply H1.
    replace a with t.
    apply H.
    destruct (t <? lmin q).
    apply H.
    pose proof min_correct a q.
    pose proof Gt.le_gt_trans (lmin q) (lmin (a::q)) x.
    apply H1.
    apply H0.
    apply H.
Qed.


Lemma ins_gets_min_1 : forall t q a, t < a /\ t < lmin q -> t = lmin (insert a (t::q)).
Proof.
    intros.
    simpl.
    replace (t <? a) with true.
    simpl.
    replace (t <? lmin (insert a q)) with true.
    reflexivity.
    assert (t < lmin (insert a q)).
    pose proof stab_ins q a t.
    apply H0.
    assumption.
    pose proof PeanoNat.Nat.ltb_lt t (lmin (insert a q)).
    symmetry.
    apply H1.
    assumption.
    pose proof PeanoNat.Nat.ltb_lt t a.
    symmetry.
    apply H0.
    apply H.
Qed.

Lemma ins_gets_min_2 : forall l a, a < lmin l -> a = lmin (insert a l).
Proof.
    intros.
    induction l.
    assert (a < 0).
    replace 0 with (lmin nil).
    assumption.
    reflexivity.
    easy.

    simpl.
    destruct (a0 <? a) eqn : G.
    simpl.
    destruct (a0 <? lmin (insert a l)) eqn : F.
    pose proof min_correct a0 l.
    assert (a < a0).
    pose proof Gt.le_gt_trans a0 (lmin (a0::l)) a.
    apply H1.
    apply H0.
    apply H.
    assert (a0 < a).
    pose proof PeanoNat.Nat.ltb_lt a0 a.
    apply H2.
    assumption.
    assert (a < a).
    pose proof Gt.gt_trans a a0 a.
    apply H3.
    apply H2.
    apply H1.
    pose proof PeanoNat.Nat.lt_irrefl a.
    easy.
    assert (a0 < a).
    pose proof PeanoNat.Nat.ltb_lt a0 a.
    apply H0.
    assumption.
    assert (a < a0).
    pose proof min_correct a0 l.
    pose proof Gt.le_gt_trans a0 (lmin (a0::l)) a.
    apply H2.
    apply H1.
    assumption.
    assert (a < a).
    pose proof Gt.gt_trans a a0 a.
    apply H2.
    apply H0.
    apply H1.
    pose proof PeanoNat.Nat.lt_irrefl a.
    easy.
    replace (lmin (a::a0::l)) with (if a <? (lmin (a0::l)) then a else lmin (a0::l)).
    replace (a <? (lmin (a0::l))) with true.
    reflexivity.
    pose proof PeanoNat.Nat.ltb_lt a (lmin (a0::l)).
    symmetry.
    apply H0.
    assumption.
    reflexivity.
Qed.

Lemma sort_gets_min : forall l t q, sort l = t::q -> t = lmin (sort l).
Proof.
    induction l.
    intros.
    easy.
    intros.
    simpl.

    destruct (a <? lmin (sort l)) eqn : Cmp.
    assert (a < lmin (sort l)).
    admit.
    destruct (sort l) eqn : G.
    simpl.
    admit. (* a < 0 *)
    simpl.
    assert (a < n). (* transitivité *)
    admit.
    assert ((n <? a) = false).
    admit.
    replace (n <? a) with false.
    replace (lmin (a::n::l0)) with (if a <? lmin (n::l0) then a else lmin (n::l0)).
    replace (a <? lmin (n::l0)) with true.





    pose proof ins_gets_min_2 (sort l) a. 
    replace (lmin (insert a (sort l))) with a.
    replace t with (lmin (sort l)).
    apply IHl.


    destruct (a < lmin (sort l)) eqn : G.
    
    simpl in H.
    assert (t <= lmin )
    




Theorem sort_makes_sorted2 : forall (l : list nat), is_sorted2 (sort l).
Proof.
    intros.
    induction l.
    constructor.


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

Theorem sort_correct : forall (l : list nat), is_sorted (sort l) /\ same_elements l (sort l).

