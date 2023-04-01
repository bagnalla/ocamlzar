(** This module exposes a basic interface for building and drawing
    samples from an n-sided die (uniform distribution over the range
    \[0,n) of integers). *)

(** Build and cache die. *)
val build : int -> unit

(** Roll the die. *)
val roll : unit -> int

(** Flip the die n times. *)
val rolls : int -> int list
