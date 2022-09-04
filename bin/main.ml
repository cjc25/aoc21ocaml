open! Core
open! Async
open Aoc21ocaml

type day = Day1a | Day1b | Day2a | Day2b

let daynames =
  String.Map.of_alist_exn
    [ ("1a", Day1a); ("1b", Day1b); ("2a", Day2a); ("2b", Day2b) ]

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
      Day2.day2a lines
  | Day2b ->
      let f = Filename.concat b "day2" in
      let%map lines = Reader.file_lines f in
      Day2.day2b lines

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

let () =
  Command.async ~summary:"run advent of code!"
    (let%map_open.Command d = day_param and base_dir = base_dir_param in
     fun () -> Deferred.List.iter d ~f:(fun d -> runday base_dir d))
  |> Command_unix.run