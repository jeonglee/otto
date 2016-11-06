open Errable

type config =
  {
    base_port: int; (* the first port of the server ports *)
    test_dir : string; (* the directory containing student code *)
    common_dir : string; (* the directory containing files needed for testing, like unit tests. *)
    test_timeout : Message.timeout; (* the amount of time in seconds a client should run a test before timeout *)
    client_timeout : Message.timeout; (* the ammount of time in seconds before the C&C server assumes a client failed *)
    command_file : string; (* a file containing commands for testing. The output of these files becomes result.txt for student test code *)
  }

(* Commander handles command and control for a test cluster.
 * It pushes tests contained in a specified test directory to test bots
 * who request files from the command server for testing. 
 * Commander then aggregates the results. *)
module type Commander = sig
  type t

  (* make creates a new instance of Commander with the given config *)
  val make : config -> t errable

  (* Runs all of the tests specified in config, then returns Ok () on success, or
   * Err e where e is the reason testing failed. *)
  val main : t -> unit errable

  (* closes frees any system resources allocated during the usage of Commander *)
  val close : t -> t

end

module CommanderImpl : Commander
