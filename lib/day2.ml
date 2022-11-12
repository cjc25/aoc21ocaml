open! Core

type action = Forward of int | Down of int | Up of int

type result = int

let line_to_action l =
  let stringpair_to_action action scalestr =
    let scale = Int.of_string scalestr in
    match action with
    | "forward" -> Forward scale
    | "down" -> Down scale
    | "up" -> Up scale
    | _ -> failwithf "unexpected action %s" action ()
  in
  match String.split ~on:' ' l with
  | [ a; b ] -> stringpair_to_action a b
  | _ -> failwithf "unexpected line %s" l ()

let step2a (horiz, depth) l =
  match line_to_action l with
  | Forward f -> (horiz + f, depth)
  | Up u -> (horiz, depth - u)
  | Down d -> (horiz, depth + d)

let parta ls =
  let horiz, depth = List.fold ls ~init:(0, 0) ~f:step2a in
  horiz * depth

let sample =
  [ "forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2" ]

let%expect_test "2a sample" =
  parta sample |> Int.to_string |> print_endline;
  [%expect {| 150 |}]

let step2b (horiz, depth, aim) l =
  match line_to_action l with
  | Forward f -> (horiz + f, depth + (f * aim), aim)
  | Up u -> (horiz, depth, aim - u)
  | Down d -> (horiz, depth, aim + d)

let partb ls =
  let horiz, depth, _ = List.fold ls ~init:(0, 0, 0) ~f:step2b in
  horiz * depth

let%expect_test "2b sample" =
  partb sample |> Int.to_string |> print_endline;
  [%expect {| 900 |}]
