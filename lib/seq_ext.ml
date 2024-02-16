let first : 'a Seq.t -> 'a = fun seq ->
  match seq () with
  | Seq.Cons (x, _) -> x
  (* Original streams were never empty. *)
  | Nil -> assert false

let rest : 'a Seq.t -> 'a Seq.t = fun seq ->
  match seq () with
  | Seq.Cons (_, xs) -> xs
  (* Original streams were never empty. *)
  | Nil -> assert false

(* Reverse the list to match original behavior. *)
let take_list : int -> 'a Seq.t -> 'a list = fun n seq ->
  Seq.take n seq |> List.of_seq |> List.rev
