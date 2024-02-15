let take = Zar.take_list
let first = Zar.first
let rest = Zar.rest

let rec string_of_list (sep : string) (to_string : 'a -> string) = function
  | [] -> ""
  | x :: xs -> to_string x ^ sep ^ string_of_list sep to_string xs

(** Check that samplers can build and run. *)
let () =
  Random.init 23872;

  print_endline "====== bits";
  print_endline @@ String.trim @@ string_of_list " " string_of_bool @@
    take 10 @@ Zar.bits ();

  (* Coin. *)
  print_endline "\n====== coin";
  let coin = Zar.coin 2 3 in
  print_endline @@ string_of_bool @@ first coin;
  print_endline @@ string_of_bool @@ first (rest coin);
  print_endline @@ string_of_bool @@ first (rest (rest coin));
  print_endline @@ string_of_bool @@ first (rest (rest (rest coin)));
  print_endline @@ String.trim @@ string_of_list " " string_of_bool @@ take 10 coin;

  (* Die. *)
  print_endline "\n====== die";
  let die = Zar.die 100 in
  print_endline @@ string_of_int @@ first die;
  print_endline @@ String.trim @@ string_of_list " " string_of_int @@ take 10 die;

  (* Findist. *)
  print_endline "\n====== findist";
  let findist = Zar.findist [1; 2; 3; 4; 5] in
  print_endline @@ string_of_int @@ first findist;
  print_endline @@ String.trim @@ string_of_list " " string_of_int @@ take 10 findist
