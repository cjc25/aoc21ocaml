open! Core
open! Async

type day = Day1a | Day1b

let runday b = function
  | Day1a ->
      let f = Filename.concat b "day1" in
      let%map lines = Reader.file_lines f in
      Aoc21ocaml.Day1.day1a lines
  | Day1b ->
      let f = Filename.concat b "day1" in
      let%map lines = Reader.file_lines f in
      Aoc21ocaml.Day1.day1b lines

let day_arg =
  Command.Arg_type.create (fun s ->
      match s with
      | "1a" -> Day1a
      | "1b" -> Day1b
      | _ -> failwithf "bad day %s" s ())

let day_param =
  let open Command.Param in
  flag ~doc:"day part" "day" (required day_arg)

let base_dir_param =
  let open Command.Param in
  flag ~doc:"input files directory" "input-dir"
    (optional_with_default "inputs" string)

(* XXX This is not quite right... *)
let () =
  Command.async ~summary:"run advent of code!"
    (let%map_open.Command d = day_param and base_dir = base_dir_param in
     fun () -> runday base_dir d)
  |> Command_unix.run
