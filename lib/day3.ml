open Core

let bitct s = String.length s

let day3a lines =
  (* All lines have the same length *)
  let bits = List.hd_exn lines |> bitct in
  let ones_seen = Array.create ~len:bits 0 in
  let countstep totalct line =
    String.iteri
      ~f:(fun i c ->
        if Char.O.(c = '1') then ones_seen.(i) <- ones_seen.(i) + 1 else ())
      line;
    totalct + 1
  in
  let line_ct = List.fold ~init:0 ~f:countstep lines in
  let target = line_ct / 2 in
  let widen accum bit = Int.shift_left accum 1 |> Int.bit_or bit in
  let gamma, epsilon =
    Array.fold ones_seen ~init:(0, 0) ~f:(fun (gamma, epsilon) ones ->
        if ones >= target then (widen gamma 1, widen epsilon 0)
        else (widen gamma 0, widen epsilon 1))
  in
  gamma * epsilon

let sample =
  {|00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010|}
  |> String.split ~on:'\n'

let%expect_test "sample a" =
  day3a sample |> Int.to_string |> print_endline;
  [%expect {| 198 |}]

let rec ox_rating line_ct index lines =
  if line_ct = 1 then "0b" ^ List.hd_exn lines |> Int.of_string
  else
    let ones, zeroes =
      List.partition_tf lines ~f:(fun l -> Char.O.(String.get l index = '1'))
    in
    let onelen = List.length ones in
    let zerolen = List.length zeroes in
    if onelen * 2 >= line_ct then ox_rating onelen (index + 1) ones
    else ox_rating zerolen (index + 1) zeroes

let rec co_rating line_ct index lines =
  if line_ct = 1 then "0b" ^ List.hd_exn lines |> Int.of_string
  else
    let ones, zeroes =
      List.partition_tf lines ~f:(fun l -> Char.O.(String.get l index = '1'))
    in
    let onelen = List.length ones in
    let zerolen = List.length zeroes in
    if onelen * 2 >= line_ct then co_rating zerolen (index + 1) zeroes
    else co_rating onelen (index + 1) ones

let day3b lines =
  let total_ct = List.length lines in
  let ox = ox_rating total_ct 0 lines in
  let co = co_rating total_ct 0 lines in
  ox * co

let%expect_test "sample b" =
  day3b sample |> Int.to_string |> print_endline;
  [%expect {| 230 |}]
