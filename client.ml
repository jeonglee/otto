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
  type t

  (* make initializes a new client *)
  val make : config -> t errable

  (* main runs the client until the testing server goes down or
   * broadcasts the fact that there are no more tests to run. *)
  val main : t -> unit errable

  (* Frees any resources held by a client. *)
  val close : t -> t

end

module ClientImpl : Client = struct
  type t = {
    port : int;
    remote_ip : Message.ip;
    test_dir : string;
    sub : SubCtxt.t;          (* For subscribing to the heartbeat *)
    hb_resp : RespCtxt.t;     (* For responding to the heartbeat *)
    pull : PullCtxt.t;        (* For pulling tests *)
    file_req : ReqCtxt.t;     (* For getting and returning files *)
  }

  let make conf =
    let client_conf = {
      port = conf.port;
      remote_ip = conf.remote_ip;
    }

    let o = {
      port      = conf.port;
      remote_ip = conf.remote_ip;
      test_dir  = conf.test_dir;
      sub       = SubCtxt.make client_conf;
      hb_resp   = RespCtxt.make client_conf;
      pull      = PullCtxt.make client_conf;
      file_req  = ReqCtxt.make client_conf;
    } in
    o

  let main c = failwith "unimplemented"

  let close c =
    let c1 = SubCtxt.close c in
    let c2 = RespCtxt.close c1 in
    let c3 = PullCtxt.close c2 in
    let c4 = ReqCtxt.close c3 in
    c4

end

