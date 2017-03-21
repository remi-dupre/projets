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

Fixpoint h (b : bin) : bin := f (g b).

(* La définition naïve mais pas pratique de h
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
*)

(* **************** *)

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
    intros.
    destruct b.
    simpl.
    reflexivity.
    simpl.
    pose proof rev_fg (g b + (g b + 0)).
    rewrite H.
    reflexivity.
    simpl.
    pose proof rev_fg (g b + (g b + 0) + 1).
    rewrite H.
    reflexivity.
Qed.

Theorem rev_gf : forall (b : bin), f (g (h b)) = h b.
Proof.
    intros.
    destruct b.
    simpl.
    reflexivity.
    simpl.
    pose proof rev_fg (g b + (g b + 0)).
    rewrite H.
    reflexivity.
    simpl.
    pose proof rev_fg (g b + (g b + 0) + 1).
    rewrite H.
    reflexivity.
Qed.

