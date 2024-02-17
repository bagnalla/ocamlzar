open Core
open Internal

(**********************************************************************)

let bits () : bool Seq.t = Seq.forever Random.bool |> Seq.memoize

let self_init = Random.self_init

let init = Random.init

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
    Seq.map int_of_z @@
      run_forever (samplers.die_sampler @@ z_of_int n) bs

let findist_transformer weights bs =
  if List.exists (fun w -> w < 0) weights then
    raise (ZarError "findist_transformer: all weights must be nonnegative")
  else if List.for_all (fun w -> w <= 0) weights then
    raise (ZarError "findist_transformer: at least one weight must be positive")
  else
    Seq.map int_of_z @@
      run_forever (samplers.findist_sampler @@ List.map z_of_int weights) bs

let coin_stream n d = coin_transformer n d @@ bits ()

let die_stream n = die_transformer n @@ bits ()

let findist_stream weights = findist_transformer weights @@ bits ()

class ['a] sampler (init : 'a Seq.t) = object (self)
  val mutable seq = init
  method gen () : 'a =
    match Seq.uncons seq with
    | None -> raise (ZarError "sampler.gen: out of samples")
    | Some (x, seq') ->
       seq <- seq';
       x
  method gen_n n : 'a list =
    List.init n (fun _ -> self#gen ())
end

let coin n d = coin_stream n d |> new sampler

let die n = die_stream n |> new sampler

let findist weights = findist_stream weights |> new sampler
