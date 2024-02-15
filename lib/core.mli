open Internal

exception ZarError of string

(** Convert positive (internal symbolic representation of positive
    integers) to int. *)
val positive_of_int : int -> positive

(** Convert int to positive. *)
val int_of_positive : positive -> int

(** Convert z (internal representation of integers) to int. *)
val z_of_int : int -> z

(** Convert int to z. *)
val int_of_z : z -> int

(** Convert numerator and denominator arguments to z and positive
    respectively and build a q (internal representation of rationals). *)
val qmake : int -> int -> q

(** Run an itree sampler. *)
val run_forever : (__, 'a) itree -> bool Seq.t -> 'a Seq.t
