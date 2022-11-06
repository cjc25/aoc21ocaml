open! Core
open! Core_bench
open! Async
open Aoc21ocaml

type day = Day1a | Day1b | Day2a | Day2b | Day3a | Day3b | Day4a | Day4b

let daynames =
  String.Map.of_alist_exn
    [
      ("1a", Day1a);
      ("1b", Day1b);
      ("2a", Day2a);
      ("2b", Day2b);
      ("3a", Day3a);
      ("3b", Day3b);
      ("4a", Day4a);
      ("4b", Day4b);
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
  | Day4a ->
      let f = Filename.concat b "day4" in
      let%map lines = Reader.file_lines f in
      Day4.day4a lines |> Int.to_string |> print_endline
  | Day4b ->
      let f = Filename.concat b "day4" in
      let%map lines = Reader.file_lines f in
      Day4.day4b lines |> Int.to_string |> print_endline

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

let bm_of_day base_dir =
  let fn name = Filename.concat base_dir name in
  function
  | Day2a -> make_benchmark ~name:"d2a" (fn "day2") Day2.day2a
  | Day2b -> make_benchmark ~name:"d2b" (fn "day2") Day2.day2b
  | Day3a -> make_benchmark ~name:"d3a" (fn "day3") Day3.day3a
  | Day3b -> make_benchmark ~name:"d3b" (fn "day3") Day3.day3b
  | _ -> failwith "day not supported for benchmarking"

let display_config =
  Bench.Display_config.create ~display:Ascii_table.Display.column_titles
    ~ascii_table:true ()

let bm_cmd =
  Command.async ~summary:"benchmark advent of code days"
    (let%map_open.Command d = day_param and base_dir = base_dir_param in
     fun () ->
       List.map d ~f:(bm_of_day base_dir)
       |> Bench.bench ~display_config
       |> return)

let () =
  Command.group ~summary:"Run once or benchmark the advent of code"
    [ ("aoc", aoc_cmd); ("bm", bm_cmd) ]
  |> Command_unix.run