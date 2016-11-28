open Errable
open Comm

type config = {
  remote_port : int; (* Base port the C&C server is running on *)
  remote_ip : Message.ip; (* IP of the C&C server *)
  test_dir : string; (* Directory in which to download files and run tests *)
}

(* Pulls tests from the C&C server, runs them, and responds to the server with
 * a summary of the test results. *)
module type Client = sig
  type 'a t
    constraint 'a = [> `Sub | `Pull |`Req]

  (* make initializes a new client *)
  val make : config -> 'a t errable

  (* main runs the client until the testing server goes down or
   * broadcasts the fact that there are no more tests to run. *)
  val main : 'a t -> unit errable

  (* Frees any resources held by a client. *)
  val close : 'a t -> 'a t

end

module ClientImpl : Client = struct
  type 'a t = {
    conf        : config;
    remote_port : int;
    remote_ip   : Message.ip;
    test_dir    : string;
    sub         : 'a SubCtxt.t;    (* For subscribing to the heartbeat *)
    hb_resp     : 'a ReqCtxt.t;    (* For responding to the heartbeat *)
    pull        : 'a PullCtxt.t;   (* For pulling tests *)
    file_req    : 'a ReqCtxt.t;    (* For getting files to grade *)
    return      : 'a ReqCtxt.t     (* For returning graded results *)
  } constraint 'a = [> `Req | `Sub | `Pull ]

  let make conf =
    let o = {
      conf        = conf;
      remote_port = conf.remote_port;
      remote_ip   = conf.remote_ip;
      test_dir    = conf.test_dir;
      sub         = SubCtxt.make {port = conf.remote_port;
                                  remote_ip = conf.remote_ip};
      pull        = PullCtxt.make {port = conf.remote_port + 1;
                                  remote_ip = conf.remote_ip};
      file_req    = ReqCtxt.make {port = conf.remote_port + 2;
                                  remote_ip = conf.remote_ip};
      return      = ReqCtxt.make {port = conf.remote_port + 3;
                                  remote_ip = conf.remote_ip};
      hb_resp     = ReqCtxt.make {port = conf.remote_port + 4;
                                  remote_ip = conf.remote_ip};
    } in
    Ok o (*!!!!!! Do some checking here - just did Ok to make it compile for now !!!!!!!! *)


  (* execute pulled commands and returns unit if successful, and raises failure
   * otherwise. A command execution is 'successful' if its exit code is 0 *)
  let execute commands =
    let exit_codes = List.map Sys.command commands in
    let sum = List.fold_left (+) 0 exit_codes in
    if sum = 0 then () else failwith "failed to execute pulled commands"

  (* Takes in FileCrawler.files and turns them into
   * actual files in the current working directory *)
  let rec convert_files files =
    let errables = List.map FileCrawler.write_file files in
    List.iter (?!) errables

  (* Helper to make a new directory named netid containing all needed files *)
  let make_test_dir netid files =
    let open Unix in
    mkdir netid 0o770; (* not sure about the permissions *)
    chdir netid;
    convert_files files;
    ()

  (* Helper to extract all lines from an open in_channel *)
  let rec get_results channel acc =
    try
      let new_acc = acc ^ (input_line channel) in
      get_results channel new_acc
    with
      | _ -> acc

  (* Helper to set up and run tests for a given assignment *)
  let run_tests netid files commands =
    let old = Unix.dup Unix.stdout in
    let new_out = open_out netid in
    Unix.dup2 (Unix.descr_of_out_channel new_out) Unix.stdout;
    let cur = Unix.getcwd () in
    make_test_dir netid files;
    execute commands;
    Unix.chdir cur;
    let results_in = open_in netid in
    let results = get_results results_in "" in
    close_in results_in;
    flush stdout;
    Unix.dup2 old Unix.stdout;
    results

  let timeout t (u : unit) = failwith "unimplemented"
  (* TODO: helper function to implement timing out *)
  (* This should be run in its own thread during execute *)
  (* The thread should then be killed at the end of execute *)


  let get_ip =
    try (Ok (Unix.open_process_in "curl \"https://api.ipify.org\" 2>/dev/null"
      |> input_line))
    with
    | End_of_file -> Err End_of_file

  (* TODO: helper function for receiving and responding to heartbeats *)
  (* This will also be responsible for checking when grading is done *)
  (* When it is, it should set the value at [finished] to true *)
  let hb_handler c finished (u : unit) =
    let open Message in
    let check_if_done m =
      match m with
      | Heartbeat(time,d) when (d = true) -> finished:=true;
      | _ -> raise Comm.Invalid_ctxt
    in
    SubCtxt.connect check_if_done c.sub;
    let unpack_ip packed = match packed with
    | Ok ip -> ip
    | Err e -> raise e in
    let req = HeartbeatResp (unpack_ip get_ip) in
    match (ReqCtxt.send req c.hb_resp) with
    | Ok _ -> ()
    | Err e -> raise e

  let main c =
    (* TODO: set up a thread running the heartbeat check function *)
    let finished = ref false in
    let hb = Async.run_in_background (hb_handler c finished) in

    let rec main_loop c finished =
      let open Message in
      let netid = ref "" in
      let timeout = ref (-1) in
      let commands = ref [] in
      let do_on_pull = function
        | TestSpec(key,t,cmds) -> netid:=key; timeout:=t; commands:=cmds
        | _ -> raise Comm.Invalid_ctxt
      in
      let () = PullCtxt.connect do_on_pull c.pull in
      (*---------- everything for pull up to here ----------*)
      let files = ref [] in
      let req_mes = FileReq (!netid) in
      match ReqCtxt.send req_mes c.file_req with
      | Err e ->  Err e
      | Ok (Files f) -> files := f;
                        let results = run_tests (!netid) (!files) (!commands) in
                        let res_mes = TestCompletion (!netid, results) in
                        let ack = ReqCtxt.send res_mes c.return in
                        if !finished then Ok () else main_loop c finished
      | Ok _ -> failwith "unexpected response"
    in main_loop c finished

  let close c =
    let s = SubCtxt.close c.sub in
    let h = ReqCtxt.close c.hb_resp in
    let p = PullCtxt.close c.pull in
    let f = ReqCtxt.close c.file_req in
    let r = ReqCtxt.close c.return in
    let o = {
      conf        = c.conf;
      remote_port = c.remote_port;
      remote_ip   = c.remote_ip;
      test_dir    = c.test_dir;
      sub         = s;
      pull        = p;
      file_req    = f;
      return      = r;
      hb_resp     = h
    } in
    o


end
