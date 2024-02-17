(** Default implementation of uniformly distributed random bit stream. *)
val bits : unit -> bool Seq.t

(** Initialize PRNG used for default implementation of bit stream. *)
val self_init : unit -> unit

(** Initialize PRNG used for default implementation of bit stream. *)
val init : int -> unit

(** Coin stream transformer. *)
val coin_transformer : int -> int -> bool Seq.t -> bool Seq.t

(** Die stream transformer. *)
val die_transformer : int -> bool Seq.t -> int Seq.t

(** Findist stream transformer. *)
val findist_transformer : int list -> bool Seq.t -> int Seq.t

(** Coin stream (applied to bits). *)
val coin_stream : int -> int -> bool Seq.t

(** Die stream (applied to bits). *)
val die_stream : int -> int Seq.t

(** Findist stream (applied to bits). *)
val findist_stream : int list -> int Seq.t

(** Stateful interface over a stream of samples. *)
class ['a] sampler : 'a Seq.t -> object
  method gen : unit -> 'a
  method gen_n : int -> 'a list
end

(** Stateful coin sampler. *)
val coin : int -> int -> bool sampler

(** Stateful die sampler. *)
val die : int -> int sampler

(** Stateful findist sampler. *)
val findist : int list -> int sampler
