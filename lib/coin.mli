(** This module exposes a basic interface for building and drawing
    samples from a biased coin (Bernoulli distribution) with rational
    bias. *)

(** Build and cache coin. *)
val build : int -> int -> unit

(** Flip the coin. *)
val flip : unit -> bool

(** Flip the coin n times. *)
val flips : int -> bool list
