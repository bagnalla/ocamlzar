
type 'a stream =
  | SCons of 'a * (unit -> 'a stream)

let first : 'a stream -> 'a = function
  | SCons (x, _) -> x

let rest : 'a stream -> 'a stream = function
  | SCons (_, s) -> s ()

let rec map (f : 'a -> 'b) : 'a stream -> 'b stream = function
  | SCons (x, s) -> SCons (f x, fun _ -> map f (s ()))
