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
    remote_port : int;
    remote_ip : Message.ip;
    test_dir : string;
    sub : SubCtxt.t;          (* For subscribing to the heartbeat *)
    hb_resp : RespCtxt.t;     (* For responding to the heartbeat *)
    pull : PullCtxt.t;        (* For pulling tests *)
    file_req : ReqCtxt.t;     (* For getting files to grade *)
    return : ReqCtxt.t        (* For returning graded results *)
  }

  let make conf =
    let o = {
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
      hb_resp     = RespCtxt.make {port = conf.remote_port + 4}
    } in
    o

  let main c = failwith "unimplemented"

  let close c =
    let s = SubCtxt.close c.sub in
    let h = RespCtxt.close c.hb_resp in
    let p = PullCtxt.close c.pull in
    let f = ReqCtxt.close c.file_req in
    let r = ReqCtxt.close c.return in
    let o = {
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

