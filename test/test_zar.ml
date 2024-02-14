(** Here we test the conversion functions z_of_int and int_of_z and
    check that samplers can build and run successfully. *)

open Alcotest
open QCheck_alcotest
open Zar__Core
open Zar__Internal
open Zar

let rec string_of_list (sep : string) (to_string : 'a -> string) = function
  | [] -> ""
  | x :: xs -> to_string x ^ sep ^ string_of_list sep to_string xs

(** Check that samplers can build and run. *)
let () =
  Zar.seed ();

  print_endline @@ string_of_list " " string_of_bool @@
    Zar.take 10 @@ Zar.bits ();

  (* Coin. *)
  let coin = Zar.coin 2 3 in
  print_endline @@ string_of_bool @@ first coin;
  print_endline @@ string_of_bool @@ first (rest coin);
  print_endline @@ string_of_bool @@ first (rest (rest coin));
  print_endline @@ string_of_bool @@ first (rest (rest (rest coin)));
  print_endline @@ string_of_list " " string_of_bool @@ Zar.take 10 coin;

  (* Die. *)
  let die = Zar.die 100 in
  print_endline @@ string_of_int @@ first die;
  print_endline @@ string_of_list " " string_of_int @@ Zar.take 10 die;

  (* Findist. *)
  let findist = Zar.findist [1; 2; 3; 4; 5] in
  print_endline @@ string_of_int @@ first findist;
  print_endline @@ string_of_list " " string_of_int @@ Zar.take 10 findist

(** Number of samples per QCheck test. *)
let gen_count = 10000

(** Lower and upper bounds on randomly generated ints. Divide by 2 to
    prevent overflow in 'double_z_of_int' and 'z_of_int_plus' tests. *)
let bound = Int.max_int / 2

let tests = ref []
let add_test nm t = tests := !tests @ [Alcotest.test_case nm `Quick t]
let add_qcheck t = tests := !tests @ [to_alcotest t]

let rec string_of_positive : positive -> string = function
  | XH -> "XH"
  | XO p -> "XO " ^ string_of_positive p
  | XI p -> "XI " ^ string_of_positive p
let string_of_z : z -> string = function
  | Z0 -> "Z0"
  | Zpos p -> "Zpos " ^ string_of_positive p
  | Zneg p -> "Zneg " ^ string_of_positive p
let z : z testable =
  let pp_z ppf x = Fmt.pf ppf "%s" (string_of_z x) in
  testable pp_z ( = )
let pos_gen =
  let open QCheck.Gen in
  sized_size (int_bound @@ Sys.word_size - 3) @@ fix (fun self n ->
               match n with
               | 0 -> return XH
               | _ -> oneof [map (fun x -> XO x) (self (n - 1));
                             map (fun x -> XI x) (self (n - 1))])
let z_gen =
  let open QCheck.Gen in
  frequency [1, return Z0;
             100, map (fun x -> Zpos x) pos_gen;
             100, map (fun x -> Zneg x) pos_gen]
let arbitrary_z =
  let open QCheck.Iter in
  let shrink_positive = function
  | XH -> QCheck.Iter.empty
  | XO p -> QCheck.Iter.return p
  | XI p -> QCheck.Iter.return p
  in
  let shrink_z = function
    | Z0 -> empty
    | Zpos p -> let* x = shrink_positive p in return (Zpos x)
    | Zneg p -> let* x = shrink_positive p in return (Zneg x)
  in
  QCheck.make z_gen ~print:string_of_z ~shrink:shrink_z

(** A few positives for testing. *)
let one = XH
let two = XO one
let three = XI one
let four = XO two
let five = XI two

let () = add_test "int_of_z" @@
           fun _ ->
           (check int) "" 0 (int_of_z Z0);
           (check int) "" 1 (int_of_z (Zpos one));
           (check int) "" 2 (int_of_z (Zpos two));
           (check int) "" 3 (int_of_z (Zpos three));
           (check int) "" 4 (int_of_z (Zpos four));
           (check int) "" 5 (int_of_z (Zpos five));
           (check int) "" (-1) (int_of_z (Zneg one));
           (check int) "" (-2) (int_of_z (Zneg two));
           (check int) "" (-3) (int_of_z (Zneg three));
           (check int) "" (-4) (int_of_z (Zneg four));
           (check int) "" (-5) (int_of_z (Zneg five))

let () = add_test "z_of_int" @@
           fun _ ->
           (check z) "" Z0 (z_of_int 0);
           (check z) "" (Zpos one) (z_of_int 1);
           (check z) "" (Zpos two) (z_of_int 2);
           (check z) "" (Zpos three) (z_of_int 3);
           (check z) "" (Zpos four) (z_of_int 4);
           (check z) "" (Zpos five) (z_of_int 5);
           (check z) "" (Zneg one) (z_of_int (-1));
           (check z) "" (Zneg two) (z_of_int (-2));
           (check z) "" (Zneg three) (z_of_int (-3));
           (check z) "" (Zneg four) (z_of_int (-4));
           (check z) "" (Zneg five) (z_of_int (-5))

(** z_of_int ∘ int_of_z = id. *)
let () = add_qcheck @@
           QCheck.Test.make ~name:"z_of_int_int_of_z" ~count:gen_count
             arbitrary_z
             (fun n -> z_of_int (int_of_z n) = n)

(** int_of_z ∘ z_of_int = id. *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"int_of_z_z_of_int" ~count:gen_count
                     (int_range (-bound) bound)
                     (fun n -> int_of_z (z_of_int n) = n))

(** ∀ n m, z_of_int n + z_of_int m = z_of_int (n + m). *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"z_of_int_plus" ~count:gen_count
                     (pair (int_range (-bound) bound) (int_range (-bound) bound))
                     (fun (n, m) -> Z.add (z_of_int n) (z_of_int m)
                                    = z_of_int (n + m)))

(** ∀ n m, int_of_z n + int_of_z m = int_of_z (n + m). *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"int_of_z_plus" ~count:gen_count
                     (pair arbitrary_z arbitrary_z)
                     (fun (n, m) -> int_of_z n + int_of_z m
                                    = int_of_z (Z.add n m)))

(** ∀ n, int_of_z (double n) = 2 * int_of_z n. *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"int_of_z_double" ~count:gen_count
                     arbitrary_z
                     (fun n -> int_of_z (Z.double n) = 2 * int_of_z n))

(** ∀ n, double (z_of_int n) = z_of_int (2 * n). *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"double_z_of_int" ~count:gen_count
                     (int_range (-bound) bound)
                     (fun n -> Z.double (z_of_int n) = z_of_int (2 * n)))

(** ∀ n m, ltb n m ⇔ int_of_z n < int_of_z m. *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"lt_int_of_z" ~count:gen_count
                     (pair arbitrary_z arbitrary_z)
                     (fun (n, m) -> Z.ltb n m == (int_of_z n < int_of_z m)))

(** ∀ n m, n < m ⇔ ltb (z_of_int n) (z_of_int m). *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"z_of_int_lt" ~count:gen_count
                     (pair (int_range (-bound) (bound))
                        (int_range (-bound) (bound)))
                     (fun (n, m) -> n < m == Z.ltb (z_of_int n) (z_of_int m)))

(* Zar.take *)

(* Using big enough numbers to ensure the chance of randomly generating the same
   sequence twice is low enough to be effectively zero. *)
let () = add_qcheck @@
           let () = Random.self_init () in
           let die = Zar.die 10_000 in
           QCheck.Test.make ~count:1000
             ~name:"repeated calls to [take] return different value streams with the same first value"
             (QCheck.int_range 50 100)
             (fun n ->
               let sample1 = Zar.take n die |> Array.of_list in
               let sample2 = Zar.take n die |> Array.of_list in
               (* "First" value is the same. *)
               sample1.(n - 1) = sample2.(n - 1) &&
                 (* But the stream of values are different. *)
                 sample1 <> sample2)

let () = add_qcheck @@
           let () = Random.self_init () in
           let die = Zar.die 10_000 in
           QCheck.Test.make ~count:1000
             ~name:"seeding [Random.init] with same seed returns same stream"
             QCheck.(tup2 (int_range 1 100) (int_range 1 999999))
             (fun (n, seed) ->
               Random.init seed;
               let sample1 = Zar.take n die in
               Random.init seed;
               let sample2 = Zar.take n die in
               sample1 = sample2)


(* let () = *)
(*   let zs = QCheck.Gen.generate ~n:20 z_gen in *)
(*   print_endline @@ string_of_list "\n" string_of_z zs *)

(** Run unit tests. *)
let () = Alcotest.run "zar" [ "zar", !tests ]
