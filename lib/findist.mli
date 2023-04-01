(** This module exposes a basic interface for building and drawing
    samples from an finite distribition over a range integers
    specified by a list of integer weights. *)

(** Build and cache sampler from list of weights. *)
val build : int list -> unit

(** Draw a single sample. *)
val sample : unit -> int

(** Draw n samples. *)
val samples : int -> int list
