open CommCtrl

let base_port = ref (5555)

let test_dir = ref "./tests"

let common_dir = ref "./common"

let test_timeout = ref (15)

let client_timeout = ref (45)

let command_file = ref "./commands"

let set_val =
  (:=)

let set_port = set_val base_port
let set_test_dir = set_val test_dir
let set_common_dir = set_val common_dir
let set_test_timeout = set_val test_timeout
let set_client_timeout = set_val client_timeout
let set_command_file = set_val command_file

let args =
  [
    ("-port", Arg.Set_int base_port,
     "The lowest port the server should use. Default 5555");
    ("-tests", Arg.Set_string test_dir,
     "The directory containing code to test.");
    ("-common", Arg.Set_string common_dir,
     "The directory containing files every test needs to run.");
    ("-test-timeout", Arg.Set_int test_timeout,
     "The amount of time in seconds a test should be allowed to run (default 15)");
    ("-client-timeout", Arg.Set_int client_timeout,
     "The amount of time a test should be allowed to run before the server declares a tester bot dead. (default 45)");
    ("-commands", Arg.Set_string command_file,
     "A file containing newline-separated commands to run in the shell during a test.");
    ("-debug", Arg.Bool (Util.set_debug), "Controls debug output")
  ]

let usage = "otto_server is the server component of the Otto autograder. Options are:"

let on_client_timeout ip =
  print_endline ("Bot at ip " ^ ip ^ " timed out.")

let on_client_connected ip =
  print_endline ("Bot at ip " ^ ip ^ " connected.")

let on_success key =
  print_endline ("Ran tests for key " ^ key ^ " successfully")

let on_failure key =
  print_endline ("Test for " ^ key ^ " timed out.")

let dbg = Util.debug_endline

let () =
  Arg.parse args (fun _ -> raise (Arg.Bad ("Invalid argument"))) usage;
  let bp = !base_port in
  let td = !test_dir in
  let cd = !common_dir in
  let tt = !test_timeout in
  let ct = !client_timeout in
  let cmf = !command_file in
  (if bp <= 0 || bp > (49151-5)
   then failwith ((string_of_int bp) ^ " is not a valid port"));
  (if (tt * 2) >= (ct)
   then print_endline
       "WARNING: It is recommended that client timeout be at least twice the test timeout.");

  let open Errable.M in
  let c = CommanderImpl.make {base_port=bp;
                              test_dir=td;
                              common_dir=cd;
                              test_timeout=tt;
                              client_timeout=ct;
                              command_file=cmf
                             }
  in
  let run_success =
    (fun () -> c
      >>> (dbg "cc"; CommanderImpl.set_client_connected_callback on_client_connected)
      >>> (dbg "ct"; CommanderImpl.set_client_timeout_callback on_client_timeout)
      >>> (dbg "of"; CommanderImpl.set_failure_callback on_failure)
      >>> (dbg "os"; CommanderImpl.set_success_callback on_success)
      >>= (fun c -> dbg "Starting main"; CommanderImpl.main c))
  in
  Util.try_finally (run_success) (fun () -> c >>> CommanderImpl.close
                                            |> (?!) |> ignore)
  >>= (fun _ -> FileCrawler.Grading.grade_results "./results")
  >>> (fun () -> print_endline "Successfully graded results.")
  |> (?!);
  print_endline "Success!"
