Require Import Nat.
Require Import Bool.

Inductive bin : Set :=
| Zero : bin
| Double : bin -> bin
| DoubleOne : bin -> bin.

Fixpoint g (n : bin) : nat :=
    match n with
    | Zero => 0
    | Double k => 2 * (g k)
    | DoubleOne k => (2 * (g k)) + 1
    end.

Fixpoint incr (n : bin) : bin :=
    match n with
    | Zero => DoubleOne Zero
    | Double k => DoubleOne k
    | DoubleOne k => Double (incr k)
    end.

Fixpoint decr (n : bin) : bin :=
    match n with
    | Zero => Zero
    | Double k => DoubleOne (decr k)
    | DoubleOne k => Double k
    end.

Fixpoint f (n : nat) : bin :=
    match n with
    | 0 => Zero
    | S k => incr (f k)
    end.

Fixpoint h (b : bin) : bin :=
    match b with
    | Zero => Zero
    | Double k =>
        (match h k with
        | Zero => Zero
        | l => Double l
        end)
    | DoubleOne k => DoubleOne k
    end.

Lemma incr_ok_in : forall (b : bin), g (incr b) = S (g b).
Proof.
    Require Import Omega.
    intro b.
    induction b.

    reflexivity.
    
    simpl.
    omega.

    simpl.
    omega.
Qed.

Theorem rev_fg : forall (n : nat), g (f n) = n.
Proof.
    intro n.
    induction n.

    reflexivity.
    simpl.
    pose proof incr_ok_in (f n).
    replace (g (incr (f n))) with (S (g (f n))).
    pose proof eq_S (g (f n)) n.
    apply H0.
    apply IHn.
Qed.


Theorem h_fixed : forall (b : bin), g b = g (h b).
Proof.
    intro B.
    
    induction B.
    easy.
   
    simpl.

    destruct (h B).
    replace (g B) with (g Zero).
    easy.    

    replace (g B) with (g (Double b)).
    simpl.
    reflexivity.
    replace (g B) with (g (DoubleOne b)).
    simpl.
    reflexivity.
    simpl.
    reflexivity.
Qed.

Lemma incr_ok_out : forall (n : nat), incr (f n) = f (S n).
Proof.
    intro n.
    induction n.
    easy.

    simpl.
    replace (incr (f n)) with (f (S n)).
    reflexivity.
Qed.

Lemma no_make_zero : forall b, h (Double b) <> Zero -> h b <> Zero.
Proof.
    intros.
    assert (h b = Zero -> h (Double b) = Zero).
    intros.
    simpl.
    replace (h b) with Zero.
    reflexivity.
    auto. (* Je savais pas comment passer à la contraposée *)
Qed.

Lemma dec_inv : forall (b : bin), h b <> Zero -> incr (decr b) = b.
Proof.
    intros.
    induction b.
    easy.

    simpl.
    replace (incr (decr b)) with b.
    reflexivity.
    symmetry.
    apply IHb.
    pose proof no_make_zero b.
    apply H0.
    assumption.

    simpl.
    reflexivity.
Qed.

Theorem inject : forall (b : bin), exists (n : nat), h b = f n.
Proof.
    intros.
    induction b.
    exists 0.
    reflexivity.

    compare (h b) Zero.
    intro.
    exists 0.
    simpl.
    replace (h b) with Zero.
    reflexivity.

    intro.
    destruct IHb as [m H].
    exists (g (Double (f m))).
    replace (h (double b)) with (double (h b)). 
    simpl.
    

    destruct IHb as [n G].
    intro.
    exists (2*n).
    simpl.
    destruct (h b).
    assert (n = 0).
    pose proof rev_fg n.
    replace n with (g (f n)).
    replace (f n) with Zero.
    reflexivity.
    replace n with 0.
    reflexivity.
    replace (Double b0) with (f n).
Admitted.

Theorem rev_gf : forall (b : bin), f (g (h b)) = h b.
Proof.
    intros.
    pose proof inject b.
    destruct H as [p H].
    replace (h b) with (f p).
    pose proof rev_fg p.
    replace (g (f p)) with p.
    reflexivity.
Qed.

