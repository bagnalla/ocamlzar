type 'a stream = 'a Stream.stream

(** Default implementation of uniformly distributed random bit stream. *)
val bits : unit -> bool stream

(** Initialize PRNG used for default implementation of bit stream. *)
val seed : unit -> unit

(** Coin stream transformer. *)
val coin_transformer : int -> int -> bool stream -> bool stream

(** Die stream transformer. *)
val die_transformer : int -> bool stream -> int stream

(** Findist stream transformer. *)
val findist_transformer : int list -> bool stream -> int stream

(** Coin stream (applied to bits). *)
val coin : int -> int -> bool stream

(** Die stream (applied to bits). *)
val die : int -> int stream

(** Findist stream (applied to bits). *)
val findist : int list -> int stream
