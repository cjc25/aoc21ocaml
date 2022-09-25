open! Core
open! Core_bench
open! Async
open Aoc21ocaml

type day = Day1a | Day1b | Day2a | Day2b | Day3a | Day3b

let daynames =
  String.Map.of_alist_exn
    [
      ("1a", Day1a);
      ("1b", Day1b);
      ("2a", Day2a);
      ("2b", Day2b);
      ("3a", Day3a);
      ("3b", Day3b);
    ]

let runday b = function
  | Day1a ->
      let f = Filename.concat b "day1" in
      let%map lines = Reader.file_lines f in
      Day1.day1a lines
  | Day1b ->
      let f = Filename.concat b "day1" in
      let%map lines = Reader.file_lines f in
      Day1.day1b lines
  | Day2a ->
      let f = Filename.concat b "day2" in
      let%map lines = Reader.file_lines f in
      Day2.day2a lines |> Int.to_string |> print_endline
  | Day2b ->
      let f = Filename.concat b "day2" in
      let%map lines = Reader.file_lines f in
      Day2.day2b lines |> Int.to_string |> print_endline
  | Day3a ->
      let f = Filename.concat b "day3" in
      let%map lines = Reader.file_lines f in
      Day3.day3a lines |> Int.to_string |> print_endline
  | Day3b ->
      let f = Filename.concat b "day3" in
      let%map lines = Reader.file_lines f in
      Day3.day3b lines |> Int.to_string |> print_endline

let day_arg =
  Command.Arg_type.create (fun s ->
      match Map.find daynames s with
      | Some d -> d
      | None -> failwithf "bad day %s" s ())

let day_param =
  let open Command.Param in
  flag ~doc:"day part" "day" (one_or_more_as_list day_arg)

let base_dir_param =
  let open Command.Param in
  flag ~doc:"input files directory" "input-dir"
    (optional_with_default "inputs" string)

let aoc_cmd =
  Command.async ~summary:"run advent of code!"
    (let%map_open.Command d = day_param and base_dir = base_dir_param in
     fun () -> Deferred.List.iter d ~f:(fun d -> runday base_dir d))

(** make_benchmark infile f returns a benchmark for [Bench.Test.create_with_initialization] *)
let make_benchmark ~name infile f =
  Bench.Test.create_with_initialization ~name (fun `init ->
      let lines = Stdio.In_channel.read_lines infile in
      fun () -> f lines)

let d2a_bm () = make_benchmark ~name:"d2a" "inputs/day2" Day2.day2a
let d2b_bm () = make_benchmark ~name:"d2b" "inputs/day2" Day2.day2b
let d2_bm_command = Bench.make_command [ d2a_bm (); d2b_bm () ]

let () =
  Command.group ~summary:"Run once or benchmark the advent of code"
    [ ("aoc", aoc_cmd); ("bm", d2_bm_command) ]
  |> Command_unix.run