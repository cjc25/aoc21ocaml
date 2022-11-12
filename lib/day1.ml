open! Core
open! Async

type result = unit

let rec fold2 ~init ~f = function
  | a :: b :: tl -> fold2 ~init:(f init a b) ~f (b :: tl)
  | _ -> init

let count_increases l =
  let f acc a b = if b > a then acc + 1 else acc in
  fold2 ~init:0 ~f l

let parta ls =
  List.map ls ~f:Int.of_string
  |> count_increases |> Int.to_string |> print_endline

let%expect_test "zero for empty" =
  parta [];
  return [%expect "0"]

let%expect_test "sample" =
  parta [ "199"; "200"; "208"; "210"; "200"; "207"; "240"; "269"; "260"; "263" ];
  return [%expect "7"]

let rec fold3 ~init ~f = function
  | a :: b :: c :: tl -> fold3 ~init:(f init a b c) ~f (b :: c :: tl)
  | _ -> init

let count_triple_increases l =
  (* build sums in reverse order... *)
  let add acc a b c = (a + b + c) :: acc in
  (* ...then count decreases instead of increases *)
  let count_dec acc a b = if a > b then acc + 1 else acc in
  fold3 ~init:[] ~f:add l |> fold2 ~init:0 ~f:count_dec

let partb ls =
  List.map ls ~f:Int.of_string
  |> count_triple_increases |> Int.to_string |> print_endline
