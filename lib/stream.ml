type 'a stream = 'a Seq.t

let first : 'a stream -> 'a = fun stream ->
  match stream () with
  | Seq.Cons (x, _) -> x
  (* Original streams were never empty. *)
  | Nil -> assert false

let rest : 'a stream -> 'a stream = fun stream ->
  match stream () with
  | Seq.Cons (_, xs) -> xs
  (* Original streams were never empty. *)
  | Nil -> assert false

(* Reverse the list to match original behavior. *)
let take n s = Seq.take n s |> List.of_seq |> List.rev

let rec drop (n : int) (s : 'a stream) : 'a stream =
  if n <= 0 then s else (drop [@tailcall]) (n-1) (rest s)

let map (f : 'a -> 'b) : 'a stream -> 'b stream = fun stream ->
  Seq.map f stream

let scons (x : 'a) (xs : 'a stream) : 'a stream =
  Seq.cons x xs
