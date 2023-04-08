type 'a stream =
  | SCons of 'a * (unit -> 'a stream)

let first : 'a stream -> 'a = function
  | SCons (x, _) -> x

let rest : 'a stream -> 'a stream = function
  | SCons (_, s) -> s ()

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
