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

(* Définition de l'addition *)

Fixpoint add (a b : Ord) : Ord :=
    match b with
    | Zero => a
    | Succ b' => Succ (add a b')
    | Limit f =>
        let g (n : nat) : Ord := add a (f n) in
        Limit g
    end.

(*
 * Pour l'égalité de structure on a pas 2+w = w
 * Ca reviendrait à avoir Limit (n -> 2+n) = Limit (n -> n)
 * Du coup il faut définir une sémantique
 *)

(* Définition de < *)

Fixpoint lt (a b : Ord) : Prop :=
    match b with
    | Zero =>  False
    | Succ b' => a = b' \/ lt a b'
    | Limit f => exists n, lt a (f n)
    end.

Fixpoint le (a b : Ord) : Prop :=
    match a with
    | Zero => True
    | Succ a' => match b with
        | Zero => False
        | Succ b' => le a' b'
        | Limit f => exists n, le a' (f n)
        end
    | Limit g => forall n, le (g n) b
    end.

Definition eq (a b : Ord) : Prop := le a b /\ le b a.

Inductive leq : Ord -> Ord -> Prop :=
| zero_min : forall a : Ord, leq Zero a
| leq_incr : forall a b : Ord, lt a b -> leq (Succ a) b
| leq_lim  : forall (f : nat -> Ord) (b : Ord), (forall (n : nat), leq (f n) b) -> leq (Limit f) b
.

(* flksdjflizsejqofjzef *)

Lemma lim_mon : forall f, ((exists n, le o (f n)) -> le o (Limit f)).
Proof.
    intros.
    induction o.
    simpl.
    easy.
    simpl.
    destruct H.
    exists x.
Admitted.

Theorem ord_refl : forall o : Ord, le o o.
Proof.
    intros.
    induction o.
    simpl.
    easy.
    simpl.
    assumption.
    intro n.
    pose proof lim_mon (o n) o.
    apply H0.
    exists n.
    apply H.
Qed.

(* ***** Relation d'ordre ***** *)

Lemma ord_succ_mon : forall a b, lt (Succ a) b -> lt a b.
Proof.
    intros.
    induction b.
    simpl in H.
    assumption.
    simpl in H.
    destruct H.
    replace b with (Succ a).
    simpl.
    right.
    left.
    reflexivity.
    simpl.
    right.
    apply IHb.
    assumption.
    simpl.
    simpl lt in H.
    destruct H as [n H'].
    exists n.
    apply H0.
    assumption.
Qed.

Lemma ord_lim_mon : forall f b, lt (Limit f) b -> forall n, lt (f n) b.
Proof.
    intros.
    admit.
Admitted.

Theorem ord_trans : forall a b c, lt a b -> lt b c -> lt a c.
Proof.
    intros.
    induction c.
    admit.
    simpl.
    compare a c.
    intro.
    left.
    assumption.
    intro.
    right.
    apply IHc.
    admit.

    admit.
    admit.
Admitted.





