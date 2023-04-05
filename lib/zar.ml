
open Core
open Internal
open Stream

let first : 'a stream -> 'a = first

let rest : 'a stream -> 'a stream = rest

let take (n : int) : 'a stream -> 'a list =
  let rec go (acc : 'a list) (i : int) (s : 'a stream) : 'a list =
    if i <= 0 then acc else (go [@tailcall]) (first s :: acc) (i-1) (rest s)
  in go [] n

let rec drop (n : int) (s : 'a stream) : 'a stream =
  if n <= 0 then s else (drop [@tailcall]) (n-1) (rest s)

let rec map (f : 'a -> 'b) : 'a stream -> 'b stream = function
  | SCons (x, s) -> SCons (f x, fun _ -> map f (s ()))

let scons (x : 'a) (xs : 'a stream) : 'a stream =
  SCons (x, fun _ -> xs)

(**********************************************************************)

let seed = Random.self_init

(** Default stream of uniformly distributed random bits. *)
let rec bits () : bool stream = SCons (Random.bool (), bits)

let coin_transformer n d bs =
  if n < 0 then
    raise (ZarError "coin_transformer: numerator must be nonnegative")
  else if n > d then
    raise (ZarError "coin_transformer: numerator must be <= denominator")
  else
    run_forever (samplers.coin_sampler (qmake n d)) bs

let die_transformer n bs =
  if n <= 0 then
    raise (ZarError "die_transformer: n must be positive")
  else
    map int_of_z @@
      run_forever (samplers.die_sampler @@ z_of_int n) bs

let findist_transformer weights bs =
  if List.exists (fun w -> w < 0) weights then
    raise (ZarError "findist_transformer: all weights must be nonnegative")
  else if List.for_all (fun w -> w <= 0) weights then
    raise (ZarError "findist_transformer: at least one weight must be positive")
  else
    map int_of_z @@
      run_forever (samplers.findist_sampler @@ List.map z_of_int weights) bs

let coin n d = coin_transformer n d @@ bits ()

let die n = die_transformer n @@ bits ()

let findist weights = findist_transformer weights @@ bits ()
