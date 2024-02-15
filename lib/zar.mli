(** Default implementation of uniformly distributed random bit stream. *)
val bits : unit -> bool Seq.t

(** Initialize PRNG used for default implementation of bit stream. *)
val seed : unit -> unit

(** Coin stream transformer. *)
val coin_transformer : int -> int -> bool Seq.t -> bool Seq.t

(** Die stream transformer. *)
val die_transformer : int -> bool Seq.t -> int Seq.t

(** Findist stream transformer. *)
val findist_transformer : int list -> bool Seq.t -> int Seq.t

(** Coin stream (applied to bits). *)
val coin : int -> int -> bool Seq.t

(** Die stream (applied to bits). *)
val die : int -> int Seq.t

(** Findist stream (applied to bits). *)
val findist : int list -> int Seq.t
