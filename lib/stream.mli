(** Streams / Lazy infinite lists. *)
type 'a stream = 'a Seq.t

(** First element (head) of a stream. *)
val first : 'a stream -> 'a

(** Rest (tail) of a stream. *)
val rest : 'a stream -> 'a stream

(** Take the first n elements of a stream. *)
val take : int -> 'a stream -> 'a list

(** Drop the first n elements of a stream. *)
val drop : int -> 'a stream -> 'a stream

(** Map a function over a stream. *)
val map : ('a -> 'b) -> 'a stream -> 'b stream

(** Build a stream from a head and tail. *)
val scons : 'a -> 'a stream -> 'a stream
