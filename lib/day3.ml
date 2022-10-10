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

let partition_inplace lines index start end_ =
  let start = ref start and end_ = ref end_ in
  while !start < !end_ do
    match String.get lines.(!start) index with
    | '0' -> incr start
    | '1' ->
        decr end_;
        while Char.O.(String.get lines.(!end_) index = '1') && !start < !end_ do
          decr end_
        done;
        Array.swap lines !start !end_
    | c -> failwithf "unexpected char %c" c ()
  done;
  !start

let rec rating use_zeroes_fn lines index start end_ =
  let len = end_ - start in
  if len = 1 then "0b" ^ lines.(start) |> Int.of_string
  else
    let first_one = partition_inplace lines index start end_ in
    if use_zeroes_fn (2 * (first_one - start)) len then
      rating use_zeroes_fn lines (index + 1) start first_one
    else rating use_zeroes_fn lines (index + 1) first_one end_

let ox_rating lines start end_ = rating Int.( > ) lines 1 start end_
let co_rating lines start end_ = rating Int.( <= ) lines 1 start end_

let day3b lines =
  (* Do the first partitioning here, since ox and co will start on separate
     sides. *)
  let lines = Array.of_list lines in
  let line_ct = Array.length lines in
  let first_one = partition_inplace lines 0 0 line_ct in
  let ox, co =
    if 2 * first_one > line_ct then
      (ox_rating lines 0 first_one, co_rating lines first_one line_ct)
    else (ox_rating lines first_one line_ct, co_rating lines 0 first_one)
  in
  ox * co

let%expect_test "sample b" =
  day3b sample |> Int.to_string |> print_endline;
  [%expect {| 230 |}]
