open! Core

type action = Forward of int | Down of int | Up of int

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

let step (horiz, depth) = function
  | Forward f -> (horiz + f, depth)
  | Up u -> (horiz, depth - u)
  | Down d -> (horiz, depth + d)

let day2a ls =
  let horiz, depth =
    List.map ls ~f:line_to_action |> List.fold ~init:(0, 0) ~f:step
  in
  horiz * depth |> Int.to_string |> print_endline

let%expect_test "2a sample" =
  day2a [ "forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2" ];
  [%expect {| 150 |}]
