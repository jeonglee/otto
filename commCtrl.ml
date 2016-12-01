open Errable
open Comm
open Async
open Message
open Util

type config =
  {
    base_port: int; (* the first port of the server ports *)
    test_dir : string; (* the directory containing student code *)
    common_dir : string; (* the directory containing files needed for testing, like unit tests. *)
    test_timeout : Message.timeout; (* the amount of time in seconds a client should run a test before timeout *)
    client_timeout : Message.timeout; (* the ammount of time in seconds before the C&C server assumes a client failed *)
    command_file : string; (* a file containing commands for testing. The output of these files becomes result.txt for student test code *)
  }

let dbg = Util.debug_endline

(* Commander handles command and control for a test cluster.
 * It pushes tests contained in a specified test directory to test bots
 * who request files from the command server for testing.
 * Commander then aggregates the results. *)
module type Commander = sig
  type 'a t
    constraint 'a = [> `Pub | `Push |`Rep]

  (* make creates a new instance of Commander with the given config *)
  val make : config -> 'a t errable

  (* Runs all of the tests specified in config, then returns Ok () on success, or
   * Err e where e is the reason testing failed. *)
  val main : 'a t -> unit errable

  (* closes frees any system resources allocated during the usage of Commander *)
  val close : 'a t -> 'a t

  val set_success_callback :
    (Message.test_key -> unit) -> 'a t -> 'a t
  val set_failure_callback :
    (Message.test_key -> unit) -> 'a t -> 'a t
  val set_client_timeout_callback :
    (Message.ip -> unit) -> 'a t -> 'a t
  val set_client_connected_callback :
    (Message.ip -> unit) -> 'a t -> 'a t


end

module OrdIP : (Set.OrderedType with type t = ip) = struct
  type t = ip

  let compare = String.compare
end

module OrdStr : (Set.OrderedType with type t = string) = struct
  type t = ip

  let compare = String.compare
end

module IPMap : (Map.S with type key = ip) = Map.Make(OrdIP)

(* A mutable data structure which manages ips and alerts the user to client
   connections and timeouts *)
module IPTimeMap = struct
  type t = {
    map : time IPMap.t ref;
    lock : Mutex.t;
  }

  (* Creates a new IPTimeMap *)
  let empty () = {map=ref IPMap.empty;lock=Mutex.create ()}

(* Adds a new entry and calls the given callback function if the connection
   wasn't previously in the map
*)
  let add_ip ?callback:(c=(fun ip -> ())) (ip : ip) (t : t) : unit =
    Mutex.lock t.lock;
    let m = !(t.map) in
    let new_entry = not (IPMap.mem ip m) in
    t.map := IPMap.add ip (Unix.time ()) m;
    Mutex.unlock t.lock;
    if new_entry then (c ip) else ()

(* Removes old entries from the map, and calls the timeout callback for each
   entry removed.
*)
  let cleanup ?callback:(c=(fun ip -> ())) (timeout : time) (t : t) : unit =
    Mutex.lock t.lock;
    let m = !(t.map) in
    let now = Unix.time () in
    let old_ips = IPMap.filter (fun _ t -> now -. t > timeout) m
                  |> IPMap.bindings |> List.split |> fst in
    let new_map =
      List.fold_left (fun acc ip -> IPMap.remove ip acc) m old_ips in
    t.map := new_map;
    Mutex.unlock t.lock;
    List.iter c old_ips

(* Returns a list of all currently connected ips (that haven't been removed
   since the last cleanup) *)
  let connected_ips t =
    Mutex.lock t.lock;
    let m = !(t.map) in
    let o = IPMap.bindings m |> List.split |> fst in
    Mutex.unlock t.lock;
    o

end

module StrSet : (Set.S with type elt = string) = Set.Make(OrdStr)
module StrMap : (Map.S with type key = string) = Map.Make(OrdStr)

(* A default no-op callback *)
let default_callback _ = ()

module CommanderImpl : Commander = struct

  type 'a t = {

    compl_lock : Mutex.t;
    completed_cond : Condition.t;

    completed : bool ref;

    conf : config;
    die : bool ref;

    hb_pub : 'a PubCtxt.t;
    test_push : 'a PushCtxt.t;
    file_serv : 'a RespCtxt.t;
    result_serv : 'a RespCtxt.t;
    hb_serv : 'a RespCtxt.t;

    hb_thread : Thread.t Once.t;
    file_thread : Thread.t Once.t;
    res_thread : Thread.t Once.t;
    hbres_thread : Thread.t Once.t;
    test_push_thread : Thread.t Once.t;

    hb_result : unit errable Once.t;
    hbres_result : unit errable Once.t;
    res_result : unit errable Once.t;
    file_result : unit errable Once.t;
    test_push_result : unit errable Once.t;

    total_assignments : int;

    live_bots : IPTimeMap.t;

    attempts : int StrMap.t ref;

    assignment_lock : Mutex.t;
    finished : StrSet.t ref;
    not_assigned : StrSet.t ref;

    write_lock : Mutex.t;
    commands : command list;

    common_files : FileCrawler.file list;

    on_success : test_key -> unit;
    on_failure : test_key -> unit;
    on_client_timeout : ip -> unit;
    on_client_connected : ip -> unit;
  } constraint 'a = [> `Pub | `Push |`Rep]

  let wait_for_completion c =
    Mutex.lock (c.compl_lock);
    while not (!(c.completed)) do
        Condition.wait (c.completed_cond) (c.compl_lock)
    done;
    Mutex.unlock (c.compl_lock)

  let failed_tests t =
    (StrMap.filter (fun _ a -> a >= 3) !(t.attempts))

  let fin t =
    let failed = List.length (StrMap.bindings (failed_tests t)) in
    (StrSet.cardinal !(t.finished)) + failed >= t.total_assignments

  let new_heartbeat don =
    Heartbeat (Unix.time (), don)

  let close c =
    print_endline "Closing.";
    Util.debug_endline "Freeing resources...";
    (* let ft = float_of_int c.conf.client_timeout in *)
    print_endline "Broadcasting end of testing message...";
    Thread.delay (15.0 +. 1.); (* Let the publisher send another heartbeat before dying *)
    print_endline "Done.";
    c.die := true;
    let hb_pub = (PubCtxt.close c.hb_pub) in
    let push = (PushCtxt.close c.test_push) in
    let fs = (RespCtxt.close c.file_serv) in
    let res = RespCtxt.close c.result_serv in
    let hb_serv = RespCtxt.close c.hb_serv in
    ??? (Once.coerce c.hb_thread);
    ??? (Once.coerce c.file_thread);
    ??? (Once.coerce c.res_thread);
    ??? (Once.coerce c.hbres_thread);
    Mutex.lock c.compl_lock;
    c.completed := true;
    Condition.broadcast c.completed_cond;
    Mutex.unlock c.compl_lock;
    {c with hb_pub = hb_pub; test_push = push;
            file_serv = fs; result_serv = res;
            hb_serv = hb_serv
    }

  let set_failure_callback c t =
    {t with on_failure = c}

  let set_success_callback c t =
    {t with on_success = c}

  let set_client_timeout_callback c t =
    {t with on_client_timeout = c}

  let set_client_connected_callback c t =
    {t with on_client_connected = c}

  let serve_heartbeat c () =
    while not !(c.die) do
      Util.debug_endline "Sending heartbeat";
      (* let timeout = (float_of_int c.conf.client_timeout) in *)
      let timeout = 15.0 in
      IPTimeMap.cleanup ~callback:(c.on_client_timeout) timeout (c.live_bots);
      PubCtxt.send (new_heartbeat (fin c)) (c.hb_pub);
      Thread.delay (timeout);
    done

  (* Not thread-safe *)
  let get_next_assignment c =
    let free_assignments = !(c.not_assigned) in
    if StrSet.cardinal free_assignments > 0
    then (Some (StrSet.choose free_assignments))
    else None

  let alarm_thread c assignment () =
    Thread.delay (float_of_int c.conf.client_timeout);
    Mutex.lock c.assignment_lock;
    let f = !(c.finished) in
    if not (StrSet.mem assignment f)
    then
      begin
        c.not_assigned := StrSet.add assignment (!(c.not_assigned));
        let attempts_made = StrMap.find assignment (!(c.attempts)) in
        c.attempts := StrMap.add assignment (attempts_made+1) (!(c.attempts));
        c.on_failure (assignment ^ ":" ^ (string_of_int attempts_made))
      end;
    Mutex.unlock c.assignment_lock

  let make_test_spec key c =
    TestSpec (key,c.conf.test_timeout,c.commands)

  let serve_pusher c () =
    let open Util.Maybe in
    print_endline "Allowing clients to connect...";
    Thread.delay (3.);
    print_endline "Starting testing.";
    while not !(c.die) do
      Thread.yield ();
      Util.debug_endline "Pushing tests";
      Mutex.lock c.assignment_lock;
      let a = get_next_assignment c
      >>= (fun a ->
            c.not_assigned := StrSet.remove a !(c.not_assigned);
            let attempts_made = StrMap.find a (!(c.attempts)) in
            if attempts_made >= 3 then None else return a)
      in
      Mutex.unlock c.assignment_lock;
      Mutex.lock c.compl_lock;
      if fin c
      then (c.completed := true; Condition.broadcast c.completed_cond);
      Mutex.unlock c.compl_lock;
      a >>=
      (fun key ->
         PushCtxt.push (make_test_spec key c) c.test_push;
         print_endline ("Sent test spec for " ^ key);
         return (!-> (alarm_thread c key))
      ) |> ignore;
      (match a with Some _ -> () | _ -> Thread.delay 3.0);
    done

  let serve_heartbeat_response c () =
    let respond m rf =
      Util.debug_endline ("Received heartbeat response: " ^ Message.marshal m);
      let open Util.Maybe in
      (match m with
       | HeartbeatResp ip -> rf (Files []); return ip
       | _ -> None)
      >>> (fun x ->
          IPTimeMap.add_ip ~callback:(c.on_client_connected) x c.live_bots)
      |> ignore
    in
    RespCtxt.serve respond c.hb_serv

  let serve_results c () =
    let respond m rf =
      Util.debug_endline ("Received results response: " ^ Message.marshal m);
      Mutex.lock c.assignment_lock;
      let open Util.Maybe in
      (match m with
       | TestCompletion (key, res) -> rf (Files []); return (key,res)
       | _ -> None)
      >>> (fun result_data ->
          (* TODO: Ensure the client is calling the result file something
             appropriate *)
          ignore (FileCrawler.write_file ~dir:("./results") result_data);
          let key = (fst result_data) in
          c.not_assigned := StrSet.remove key (!(c.not_assigned));
          c.finished := StrSet.add key (!(c.finished));
          c.on_success key;
          Mutex.lock c.compl_lock;
          if fin c
          then (c.completed := true; Condition.broadcast c.completed_cond);
          Mutex.unlock c.compl_lock;
        )
      |> ignore;
      Mutex.unlock c.assignment_lock;
    in
    RespCtxt.serve respond c.result_serv

  let get_common_files c =
    let open Errable.M in
    ?! (FileCrawler.files_from_dir c.common_dir)

  let prep_files key c =
    let open Errable.M in
    let key_dir = c.conf.test_dir ^ Filename.dir_sep ^ key in
    let fs = ?! (FileCrawler.files_from_dir key_dir) in
    let com = c.common_files in
    List.map (fun (n,con) ->
        (Util.Str.replace_substr c.conf.common_dir key_dir n,con)) com
    |> List.rev_append fs

  let serve_files c () =
    let respond m rf =
      Util.debug_endline ("Received file request: " ^ Message.marshal m);
      let files =
        try
          (let open Util.Maybe in
          (match m with
           | FileReq (key) -> return key
           | _ -> None)
          >>> (fun key ->
              Ok (prep_files key c))
          |> coerce)
        with
        | e -> Err e
      in
      match files with
      | Ok f -> dbg ("Sending files: " ^ (Message.marshal (Files f)));
          rf (Files f); dbg "Sent."
      | Err e -> dbg "Failed to send files"; rf (Files []); raise e
    in
    RespCtxt.serve respond c.file_serv

  let main c =
    try
      Util.debug_endline "Entering main";
      let open Errable.M in
      Util.debug_endline ("Starting file server.");
      Once.set (!-> (serve_files c)) c.file_thread;
      Util.debug_endline ("Starting results server.");
      Once.set (!-> (serve_results c)) c.res_thread;
      Util.debug_endline ("Starting heartbeat response server.");
      Once.set (!-> (serve_heartbeat_response c)) c.hbres_thread;
      Util.debug_endline ("Starting heartbeat.");
      Once.set (!-> (serve_heartbeat c)) c.hb_thread;
      Util.debug_endline ("Starting test pusher.");
      Once.set (!-> (serve_pusher c)) c.test_push_thread;
      Util.debug_endline ("Ready to start serving.");
      wait_for_completion c;
      (* ?! (Once.coerce c.hbres_result);
      ?! (Once.coerce c.hb_result);
      ?! (Once.coerce c.file_result);
      ?! (Once.coerce c.res_result); *)
      Ok ()
    with
    | e -> Err e

  let initial_tests dir =
    let open Errable.M in
    let subs = FileCrawler.subdirectories dir in
    subs >>=
    fun subdirs ->
      let attempts =
        List.fold_left
          (fun acc key -> StrMap.add key 0 acc) StrMap.empty subdirs in
      let not_assigned =
        List.fold_left
          (fun acc key -> StrSet.add key acc) StrSet.empty subdirs in
      return (attempts, not_assigned)

  let load_commands com_file =
    Util.read_all_lines com_file

  let make conf =
    let open Errable.M in
    let c = {
    conf = conf;
    die = ref false;
    compl_lock = Mutex.create ();
    completed_cond = Condition.create ();
    completed = ref false;
    test_push = PushCtxt.make {port=conf.base_port+1};
    hb_pub = PubCtxt.make {port=conf.base_port};
    file_serv = RespCtxt.make {port=conf.base_port+2};
    result_serv = RespCtxt.make {port=conf.base_port+3};
    hb_serv = RespCtxt.make {port=conf.base_port+4};

    hb_thread = Once.make ();
    hbres_thread = Once.make ();
    res_thread = Once.make ();
    file_thread = Once.make ();
    test_push_thread = Once.make ();

    hb_result = Once.make ();
    hbres_result = Once.make ();
    res_result = Once.make ();
    file_result = Once.make ();
    test_push_result = Once.make ();

    total_assignments = -1;
    live_bots = IPTimeMap.empty ();

    attempts = ref (StrMap.empty);

    assignment_lock = Mutex.create ();
    finished = ref (StrSet.empty);
    not_assigned = ref (StrSet.empty);

    write_lock = Mutex.create ();

    on_success = default_callback;
    on_failure = default_callback;
    on_client_timeout = default_callback;
    on_client_connected = default_callback;
    commands = load_commands (conf.command_file);
    common_files = get_common_files conf;
    } in
    initial_tests (conf.test_dir)
    >>= (fun (a,n) ->
        c.attempts := a;
        c.not_assigned := n;
        print_endline
          ("Grading " ^ (string_of_int (StrSet.cardinal n)) ^ " assignments.");
        return {c with total_assignments = StrSet.cardinal n})

end
