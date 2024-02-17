(** Check that samplers can build and run. *)
let () =
  Zar.self_init ();

  (* Bit stream.*)
  print_endline "====== bits";
  Zar.bits () |> Seq.take 10 |> Seq.length |> string_of_int |> print_endline;

  (* Coin. *)
  print_endline "\n====== coin";
  Zar.coin_stream 2 3 |> Seq.take 10 |> Seq.length |> string_of_int |> print_endline;

  (* Die. *)
  print_endline "\n====== die";
  Zar.die_stream 100 |> Seq.take 10 |> Seq.length |> string_of_int |> print_endline;

  (* Findist. *)
  print_endline "\n====== findist";
  Zar.findist_stream [1; 2; 3; 4; 5] |> Seq.take 10 |> Seq.length |>
    string_of_int |> print_endline;

  (* Stateful sampler. *)
  print_endline "\n====== die sampler";
  let sampler = Zar.die 200 in
  List.init 10 (fun _ -> sampler#gen ()) |> List.length |>
    string_of_int |> print_endline
