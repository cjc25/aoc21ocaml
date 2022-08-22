open! Core
open! Async

let count_increases l =
  let rec sum acc = function
    | f :: s :: tl ->
        if s > f then sum (acc + 1) (s :: tl) else sum acc (s :: tl)
    | _ -> acc
  in
  sum 0 l

let%expect_test "zero for empty" =
  count_increases [] |> Int.to_string |> print_endline;
  return [%expect "0"]

let sample = {|199
200
208
210
200
207
240
269
260
263|}

let to_int_list_exn l = String.split l ~on:'\n' |> List.map ~f:Int.of_string

let%expect_test "sample" =
  to_int_list_exn sample |> count_increases |> Int.to_string |> print_endline;
  return [%expect "7"]

let day1a ls =
  List.map ls ~f:Int.of_string
  |> count_increases |> Int.to_string |> print_endline

let sum3 l =
  let rec sum acc = function
    | f :: s :: t :: tl -> sum ((f + s + t) :: acc) (s :: t :: tl)
    | _ -> acc
  in
  sum [] l |> List.rev

let day1b ls =
  List.map ls ~f:Int.of_string
  |> sum3 |> count_increases |> Int.to_string |> print_endline
