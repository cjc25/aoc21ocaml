open Core

type result = int

module Xy = Types.Xy

let sample =
  {|0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2|}
  |> String.split_lines

let foldcoords startxy endxy ~init ~f =
  let incrx =
    match Int.compare (Xy.x startxy) (Xy.x endxy) with
    | 0 -> 0
    | i -> if i < 0 then 1 else -1
  in
  let incry =
    match Int.compare (Xy.y startxy) (Xy.y endxy) with
    | 0 -> 0
    | i -> if i < 0 then 1 else -1
  in
  let rec loop acc currxy =
    match Xy.x currxy = Xy.x endxy && Xy.y currxy = Xy.y endxy with
    | true -> f acc currxy
    | false ->
        let acc = f acc currxy in
        loop acc (Xy.x currxy + incrx, Xy.y currxy + incry)
  in
  loop init startxy

let map_update = Map.update ~f:(function Some ls -> ls + 1 | None -> 1)

let maplines ls unfixedf =
  let overlaps =
    List.fold ls ~init:Xy.Map.empty ~f:(fun acc line ->
        let toxy s =
          match String.split s ~on:',' with
          | [ x; y ] -> (Int.of_string x, Int.of_string y)
          | _ -> failwithf "unexpected coord %s" s ()
        in
        let startxy, endxy =
          match Input.split_on_string line ~sep:" -> " with
          | [ start; end_ ] -> (toxy start, toxy end_)
          | _ -> failwithf "unexpected line %s" line ()
        in
        match (Xy.x startxy = Xy.x endxy, Xy.y startxy = Xy.y endxy) with
        | false, false -> unfixedf acc startxy endxy
        | _, _ -> foldcoords startxy endxy ~init:acc ~f:map_update)
  in
  Map.fold overlaps ~init:0 ~f:(fun ~key:_ ~data:os acc ->
      match os > 1 with true -> acc + 1 | false -> acc)

let parta ls = maplines ls (fun acc _ _ -> acc)

let%expect_test "parta sample" =
  parta sample |> Int.to_string |> print_endline;
  [%expect {| 5 |}]

let partb ls =
  maplines ls (fun acc startxy endxy ->
      foldcoords startxy endxy ~init:acc ~f:map_update)

let%expect_test "partb sample" =
  partb sample |> Int.to_string |> print_endline;
  [%expect {| 12 |}]
