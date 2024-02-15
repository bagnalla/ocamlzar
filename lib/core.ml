open Internal

exception ZarError of string

let first = Stream.first
let rest = Stream.rest

let positive_of_int (i : int) : positive =
  if i < 1 then
    raise (ZarError ("positive_of_int: int must be positive, got "
                     ^ string_of_int i))
  else
    let rec go (j : int) : positive =
      if j == 1 then
        XH
      else if j mod 2 == 0 then
        XO (go @@ j / 2)
      else
        XI (go @@ j / 2)
    in go i

let rec int_of_positive = function
  | XH -> 1
  | XO p' -> 2 * int_of_positive p'
  | XI p' -> 2 * int_of_positive p' + 1

let z_of_int (i : int) : z =
  if i == 0 then
    Z0
  else if i > 0 then
    Zpos (positive_of_int i)
  else
    Zneg (positive_of_int (-i))

let int_of_z = function
  | Z0 -> 0
  | Zpos p -> int_of_positive p
  | Zneg p -> - int_of_positive p

let qmake n d = { qnum = z_of_int n; qden = positive_of_int d }

(** Run an itree sampler to produce a single sample from a given
    stream of bits. Returns the unconsumed rest of the stream. *)
let rec run t bs =
  match observe t with
  | RetF x -> x, bs
  | TauF t' -> run t' bs
  | VisF (_, k) -> run (k (Obj.magic (first bs))) (rest bs)

let rec run_forever t bs =
  let x, bs' = run t bs in
  (fun () -> Seq.Cons (x, run_forever t bs'))
