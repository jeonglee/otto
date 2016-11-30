open Errable
open Errable.M
open Comm
open Util
open Async

type config = {
  remote_port : int; (* Base port the C&C server is running on *)
  remote_ip : Message.ip; (* IP of the C&C server *)
  test_dir : string; (* Directory in which to download files and run tests *)
}

let dbg = Util.debug_endline

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

let alarm_handler (delay : float) (pid : int option ref) () : unit =
  Thread.delay delay;
  match !pid with
  | Some pid -> dbg ("Killing subprocess " ^ (string_of_int pid));
      Unix.kill pid Sys.sigkill
  | None -> ()

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
    return      : 'a ReqCtxt.t;    (* For returning graded results *)
    finished    : bool ref;
    fin_lock    : Mutex.t;
    hb_thread   : Thread.t Once.t;
  } constraint 'a = [> `Req | `Sub | `Pull ]

  let get_ip =
    try (Ok (Unix.open_process_in "curl -s \"https://api.ipify.org\" 2>/dev/null"
      |> input_line))
    with
    | End_of_file -> Err End_of_file

  (* Helper function for receiving and responding to heartbeats *)
  (* This will also be responsible for checking when grading is done *)
  (* When it is, it should set the value at [finished] to true *)
  let hb_handler c (u : unit) =
    let open Message in
    let check_if_done m =
      dbg ("Received heartbeat: " ^ Message.marshal m);
      begin
        match m with
        | Heartbeat(time,d) when (d = true) ->
            dbg "Done! Quitting.";
            Mutex.lock c.fin_lock;
            c.finished:=true;
            Mutex.unlock c.fin_lock;
            PullCtxt.close c.pull |> ignore;
        | Heartbeat(time, d) -> dbg "Not done yet.";
        | _ -> raise Comm.Invalid_ctxt
      end;
      let unpack_ip packed = match packed with
        | Ok ip -> ip
        | Err e -> raise e in
      let req = HeartbeatResp (unpack_ip get_ip) in
      match (ReqCtxt.send req c.hb_resp) with
        | Ok _ -> ()
        | Err e -> raise e
    in
    SubCtxt.connect check_if_done c.sub


  let make conf =
    try
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
        finished    = ref false;
        fin_lock    = Mutex.create ();
        hb_thread   = Once.make ();
      } in
      Once.set (!-> (hb_handler o)) o.hb_thread;
      Ok o
    with e -> Err e

  (* Takes in FileCrawler.files and turns them into
   * actual files in the current working directory *)
  let rec convert_files netid files =
    let errables = List.map (FileCrawler.write_file) files in
    List.iter (?!) errables

  (* Helper to extract all lines from an open in_channel *)
  let rec get_results channel acc =
    try
      let new_acc = acc ^ (input_line channel) ^ "\n" in
      get_results channel new_acc
    with
      | _ -> acc

  (* execute pulled commands and returns unit if successful, and raises failure
   * otherwise. A command execution is 'successful' if its exit code is 0 *)
  let execute commands timeout inp outp =
    let pid_ref = ref None in
    let _ = (!-> (alarm_handler timeout pid_ref)) in
    let rec exec = function
      | [] -> true
      | h::t ->
          match Util.Str.split_whitespace h with
          | [] -> exec t
          | h::a ->
              begin
                let (p,args) = h, (Array.of_list a) in
                let pid = Unix.create_process p args inp outp outp in
                pid_ref := Some pid;
                dbg ("Waiting for " ^ h);
                let res =
                  match Unix.waitpid [] pid with
                  | (_, Unix.WEXITED i) -> dbg ("Exited "^(string_of_int i)); i
                  | (_, Unix.WSIGNALED i) -> dbg("Signal "^(string_of_int i)); i
                  | (_, Unix.WSTOPPED i) -> dbg ("Stopped "^(string_of_int i));
                      i in
                pid_ref := None;
                dbg ("Done waiting for " ^ h);
                Unix.write_substring outp
                  ("\nEND" ^ h ^ "\n") 0 (String.length h + 5)
                |> ignore;
                if res <> 0
                then false
                else exec t
              end
    in
    exec commands

  (* Helper to set up and run tests for a given assignment *)
  let run_tests netid timeout files commands =
    let cur = Unix.getcwd () in
    dbg ("Writing files for " ^ netid);
    convert_files netid files;
    dbg ("Done.");
    let (read,write) = Unix.pipe () in
    Unix.chdir ("./tests" ^ Filename.dir_sep ^ netid);
    dbg ("Executing tests for " ^ netid);
    if execute commands (float_of_int timeout) Unix.stdin write
    then
      begin
        dbg ("Done with test execution for " ^ netid);
        Unix.chdir cur;
        Unix.close write;
        let results_in = Unix.in_channel_of_descr (read) in
        let results = get_results results_in "" in
        dbg ("Finished reading results.");
        close_in results_in;
        B64.encode results
      end
    else
      (Unix.close write;
       B64.encode "Failed")


  let main c =
    dbg "Entered main loop.";
    if !(c.finished) then Ok () else
    let open Message in
    let netid = ref "" in
    let timeout = ref (-1) in
    let commands = ref [] in
    let do_on_pull = function
      | TestSpec(key,t,cmds) ->
            netid:=key; timeout:=t; commands:=cmds;
            dbg ("Received test for key: " ^ key);
            begin
              let files = ref [] in
              let req_mes = FileReq (!netid) in
              match ReqCtxt.send req_mes c.file_req with
              | Err e ->  raise e
              | Ok (Files f) -> files := f;
                  let results = run_tests (!netid) (!timeout) (!files) (!commands) in
                  let res_mes = TestCompletion (!netid, results) in
                  dbg "Sending completed test data...";
                  ignore (?! (ReqCtxt.send res_mes c.return));
              | Ok _ -> raise (Failure "unexpected response")
            end
      | _ -> raise Comm.Invalid_ctxt
    in
    try
      Ok (PullCtxt.connect do_on_pull c.pull)
    with
    | e -> Err e

  let close c =
    dbg "Closing...";
    let s = SubCtxt.close c.sub in
    let h = ReqCtxt.close c.hb_resp in
    let f = ReqCtxt.close c.file_req in
    let r = ReqCtxt.close c.return in
    let o = {
      conf        = c.conf;
      remote_port = c.remote_port;
      remote_ip   = c.remote_ip;
      test_dir    = c.test_dir;
      sub         = s;
      pull        = c.pull;
      file_req    = f;
      return      = r;
      hb_resp     = h;
      finished    = c.finished;
      fin_lock    = c.fin_lock;
      hb_thread   = c.hb_thread;
    } in
    ??? (Once.coerce o.hb_thread);
    Mutex.lock o.fin_lock;
    o.finished := true;
    Mutex.unlock o.fin_lock;
    o

end
