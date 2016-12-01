open Client

let base_port = ref (5555)

let remote_ip = ref "127.0.0.1"

let test_dir = ref "./tests"

let set_val =
  (:=)

let set_port = set_val base_port
let set_test_dir = set_val test_dir
let set_remote_ip = set_val remote_ip

let args =
  [
    ("-port", Arg.Set_int base_port,
     "The lowest port the server should use. Default 5555");
    ("-remote_ip", Arg.Set_string remote_ip,
     "The IP of the remote command-control server.");
    ("-debug", Arg.Bool (Util.set_debug), "Controls debug output")
  ]

let usage = "otto_client is the client component of the Otto autograder. Options are:"

let () =
  Arg.parse args (fun _ -> raise (Arg.Bad ("Invalid argument"))) usage;
  let bp = !base_port in
  let td = !test_dir in
  let ri = !remote_ip in
  (if bp <= 0 || bp > (49151-5)
   then failwith ((string_of_int bp) ^ " is not a valid port"));

  let open Errable.M in
  let c = ClientImpl.make {remote_port=bp;
                            test_dir=td;
                            remote_ip=ri
                          }
  in
  let run_success =
    (fun () -> c
      >>= ClientImpl.main)
  in
  Util.try_finally (run_success) (fun () -> c >>> ClientImpl.close
                                            |> (?!) |> ignore)
  |> (?!)
  |> ignore;
  print_endline "Success!"
