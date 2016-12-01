open Errable

(* a file is a tuple of its name and its base64 encoded contents. *)
type file = string * string

(* takes the name of a directory and creates a file list errable containing
 * the file names and their contents. Produces an Err describing the failure if it
 * fails *)
val files_from_dir : string -> file list errable

(* writes a file to disk, using the file name as the location to write to *)
val write_file : ?dir:string -> file -> unit errable

val subdirectories : string -> string list errable

module Grading : sig
  type res = {
    passed : int;
    failed : int;
    errored : int;
    other : int;
  }

  val ran_rex : Str.regexp
  val score_board : Str.regexp
  val success : Str.regexp

  val grade_results : string -> unit errable
  val grade_from_string : string -> res
end
