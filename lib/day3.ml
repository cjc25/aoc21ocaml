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

let partition_tf l ~f =
  let l = fst l in
  List.fold l
    ~init:(([], 0), ([], 0))
    ~f:(fun (ts, fs) elem ->
      match f elem with
      | true -> ((elem :: fst ts, 1 + snd ts), fs)
      | false -> (ts, (elem :: fst fs, 1 + snd fs)))

let rec partition_ones_zeroes index l ~f =
  let line_ct = snd l in
  if line_ct = 1 then fst l |> List.hd_exn
  else
    let ones, zeroes =
      partition_tf l ~f:(fun s -> Char.O.(String.get s index = '1'))
    in
    let newl = f line_ct ones zeroes in
    partition_ones_zeroes (index + 1) newl ~f

let ox_rating lines =
  let f line_ct ones zeroes =
    if 2 * snd ones >= line_ct then ones else zeroes
  in
  "0b" ^ partition_ones_zeroes 0 lines ~f |> Int.of_string

let co_rating lines =
  let f line_ct ones zeroes =
    if 2 * snd ones >= line_ct then zeroes else ones
  in
  "0b" ^ partition_ones_zeroes 0 lines ~f |> Int.of_string

let day3b lines =
  let lines = (lines, List.length lines) in
  let ox = ox_rating lines in
  let co = co_rating lines in
  ox * co

let%expect_test "sample b" =
  day3b sample |> Int.to_string |> print_endline;
  [%expect {| 230 |}]
