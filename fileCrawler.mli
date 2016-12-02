open Errable

(* a file is a tuple of its name and its base64 encoded contents. *)
type file = string * string

(* takes the name of a directory and creates a file list errable containing
 * the file names and their contents. Produces an Err describing the failure if it
 * fails *)
val files_from_dir : string -> file list errable

(* writes a file to disk, using the file name as the location to write to *)
val write_file : ?dir:string -> file -> unit errable

(* subdirectories [dir] returns Ok [list_of_directories_in_dir] or
   Err ([error while retrieving directories])*)
val subdirectories : string -> string list errable

(* Contains functions related to parsing grader output to create
   a file containing scores *)
module Grading : sig
  type res = {
    passed : int;
    failed : int;
    errored : int;
    other : int;
  }

(* Regex representing the line containing how many tests were run.
   if absent, tests were not able to run. *)
  val ran_rex : Str.regexp

(* Regex representing the score line for an OUnit test where there was
   at least one test that was not passed. We use this to determine final
   scores. *)
  val score_board : Str.regexp

  (* Regex representing a test that succeeded *)
  val success : Str.regexp

(* grade_results takes a results directory created by CommCtrl and creates
   a csv file called "aggregate.csv" in the current directory containing
   the scores of all tests run. *)
  val grade_results : string -> unit errable

  (* Converts the contents of the stdout from an OUnit test suite into a score *)
  val grade_from_string : string -> res
end
