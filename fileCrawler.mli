open Errable

type file = string * string

val files_from_dir : string -> file list errable

val write_file : file -> unit errable
