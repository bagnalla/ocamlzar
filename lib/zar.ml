open Core
open Internal
include Stream

(**********************************************************************)

let rec bits () : bool stream = SCons (Random.bool (), bits)

let seed = Random.self_init

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
