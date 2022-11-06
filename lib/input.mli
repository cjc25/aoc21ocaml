val to_sections : string list -> string list list
(** Converts a list of lines into a list of line sections, where sections are
    separated by a blank line. *)

val to_tokens : string -> string list
(** [to_tokens l] splits l into words on any number of spaces *)
