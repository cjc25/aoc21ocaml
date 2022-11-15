open Core

type result = int

let minimize line cost_f =
  let poses =
    String.split ~on:',' line
    |> List.fold ~init:Int.Map.empty ~f:(fun m s ->
           let i = Int.of_string s in
           Map.update m i ~f:(fun ct ->
               Option.value_map ct ~default:1 ~f:(fun ct -> ct + 1)))
  in
  let cost = ref Int.max_value in
  for p = Map.min_elt_exn poses |> fst to Map.max_elt_exn poses |> fst do
    let newcost =
      Map.fold poses ~init:0 ~f:(fun ~key:q ~data:ct acc ->
          acc + (cost_f p q * ct))
    in
    if newcost < !cost then cost := newcost else ()
  done;
  !cost

let parta ls = minimize (List.hd_exn ls) (fun a b -> Int.abs (a - b))

let partb ls =
  minimize (List.hd_exn ls) (fun a b ->
      let d = Int.abs (a - b) in
      d * (d + 1) / 2)

let sample = {|16,1,2,0,4,2,7,1,2,14|} |> String.split_lines

let%expect_test "a sample" =
  parta sample |> Int.to_string |> print_endline;
  [%expect {| 37 |}]

let%expect_test "b sample" =
  partb sample |> Int.to_string |> print_endline;
  [%expect {| 168 |}]
