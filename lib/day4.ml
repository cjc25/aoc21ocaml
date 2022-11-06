open Core

type entry = { num : int; marked : bool }

let entry_of_tok tok = { num = Int.of_string tok; marked = false }

(** Take a section and make it a board of entries. A board is represented in
    both row-major and column-major form. *)
let board_of_section section =
  let rows =
    List.map section ~f:(fun line ->
        Input.to_tokens line |> List.map ~f:entry_of_tok)
  in
  (rows, List.transpose_exn rows)

let score_board rep =
  List.fold rep ~init:0 ~f:(fun init entries ->
      List.fold entries ~init ~f:(fun score -> function
          | { num = _; marked = true } -> score
          | { num; marked = false } -> score + num))

(** The error case is when a board has won *)
let mark_board (rows, cols) draw =
  let mark_rep rep =
    List.fold_map ~init:false rep ~f:(fun entry_all_ok entries ->
        let entries_ok, entries =
          List.fold_map ~init:true entries ~f:(fun all_ok { num; marked } ->
              let marked = marked || num = draw in
              (all_ok && marked, { num; marked }))
        in
        (entry_all_ok || entries_ok, entries))
  in
  let won, rows = mark_rep rows in
  if won then Error (draw * score_board rows)
  else
    let won, cols = mark_rep cols in
    if won then Error (draw * score_board cols) else Ok (rows, cols)

let sample =
  String.split_lines
    {|7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
|}

let day4a ls =
  let sections = Input.to_sections ls in
  let draws =
    List.hd_exn sections |> List.hd_exn |> String.split ~on:','
    |> List.map ~f:Int.of_string
  in
  let init = List.tl_exn sections |> List.map ~f:board_of_section in
  List.fold_until draws
    ~finish:(fun _ -> failwith "no winner!")
    ~init
    ~f:(fun boards draw ->
      match
        List.map boards ~f:(fun board -> mark_board board draw) |> Result.all
      with
      | Ok boards -> Continue boards
      | Error score -> Stop score)

let%expect_test "a sample" =
  day4a sample |> printf "%d";
  [%expect {| 4512 |}]