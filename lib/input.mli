val to_sections : string list -> string list list
(** Converts a list of lines into a list of line sections, where sections are
    separated by a blank line. *)

val to_tokens : string -> string list
(** [to_tokens l] splits l into words on any number of spaces *)

val split_on_string : string -> sep:string -> string list
(** [split_on_string s ~sep] splits [s] on the sequence of chars [sep]. It does
so greedily. I.e. [split_on_string "abbbd" ~sep:"bb"] results in ["a"; "bd"]*)