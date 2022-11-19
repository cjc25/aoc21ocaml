open Core

type result = int

let run_array ls ct =
  let inits =
    List.hd_exn ls |> String.split ~on:',' |> List.map ~f:Int.of_string
  in
  let fish = Array.create ~len:9 0 in
  List.iter inits ~f:(fun i -> fish.(i) <- fish.(i) + 1);
  for _ = 1 to ct do
    let zeroes = fish.(0) in
    for i = 0 to 7 do
      Array.set fish i fish.(i + 1)
    done;
    Array.set fish 6 (fish.(6) + zeroes);
    Array.set fish 8 zeroes
  done;
  Array.fold fish ~init:0 ~f:(fun acc ct -> acc + ct)

let parta ls = run_array ls 80
let partb ls = run_array ls 256
let sample = {|3,4,3,1,2|} |> String.split_lines

let%expect_test "sample a" =
  parta sample |> Int.to_string |> print_endline;
  [%expect {| 5934 |}]

let%expect_test "sample b" =
  partb sample |> Int.to_string |> print_endline;
  [%expect {| 26984457539 |}]
