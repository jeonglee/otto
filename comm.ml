exception Invalid_ctxt
exception Timeout

open Errable

let dbg = Util.debug_endline

type server_config =
  {
    port : int
  }

type client_config =
  {
    port : int;
    remote_ip : Message.ip
  }

(* A module representing a requester in the requester-responder
 * paradigm. *)
module type RequesterContext = sig
  type 'a t
    constraint 'a = [> `Req]

  (* [make init] creates a requester context *)
  val make : client_config -> 'a t
  (* [send mess t] sends a message using an initialized context t.
   * it should block until it receives a response or errors out. *)
  val send : Message.mes -> 'a t -> Message.mes errable

  (* [close t] frees whatever resources were opened in making the context *)
  val close : 'a t -> 'a t
end

(* Represents a responder in the requester-responder paradigm *)
module type ResponderContext = sig
  type 'a t
    constraint 'a = [> `Rep]

  (* [make init] creates a responder context *)
  val make : server_config -> 'a t

  (* [serve f t] blocks and serves a function with [f] as the method of
   *  responding to requests. *)
  val serve : (Message.mes -> (Message.mes -> unit) -> unit) -> 'a t -> unit

  (* [close t] frees whatever resources were opened in making the context *)
  val close : 'a t -> 'a t
end

(* Represents a publisher in the pub/sub paradigm. A publisher broadcasts
 * a message to all its subscribers *)
module type PublisherContext = sig
  type 'a t
    constraint 'a = [> `Pub]

  (* [make init] creates a publisher context *)
  val make : server_config -> 'a t

  (* [send m t] broadcasts [m] via context [t] to any subscribers *)
  val send : Message.mes -> 'a t -> unit

  (* [close t] frees whatever resources were opened in making the context *)
  val close : 'a t -> 'a t
end

(* Represents a subscriber in the pub/sub paradigm. A subscriber
 * receives all events broadcast by a publisher after the subscriber
 * connects to the publisher. *)
module type SubscriberContext = sig
  type 'a t
    constraint 'a = [> `Sub]

  (* [make init] creates a subscriber context *)
  val make : client_config -> 'a t

  (* [connect f t] connects to a publisher and calls f when the publisher
   * sends a message *)
  val connect : (Message.mes -> unit) -> 'a t -> unit

  (* [close t] frees whatever resources were opened in making the context *)
  val close : 'a t -> 'a t
end

(* Represents a pusher in the push/pull paradigm.
 * Broadcasts a message to a puller whose identity is known *)
module type PusherContext = sig
  type 'a t
    constraint 'a = [> `Push]

  (* [make init] creates a pusher context *)
  val make : server_config -> 'a t

  (* [push m t] broadcasts a message [m] to a certain puller [t] *)
  val push : Message.mes -> 'a t -> unit

  (* [close t] frees whatever resources were opened in making the context *)
  val close : 'a t -> 'a t
end

(* Represents a puller in the push/pull paradigm. All pullers
 * compete to receive any events broadcast by a pusher while the
 * pullers are connected to the pusher. *)
module type PullerContext = sig
  type 'a t
    constraint 'a = [> `Pull]

  (* [make init] creates a pullers context *)
  val make : client_config -> 'a t

  (* [connect f t] connects to a pusher and calls f when the pusher
   * sends a message *)
  val connect : (Message.mes -> unit) -> 'a t -> unit

  (* [close t] frees whatever resources were opened in making the context *)
  val close : 'a t -> 'a t
end

open Message

module Sock = ZMQ.Socket

let ctxt () = ZMQ.Context.create ()

let five_seconds = 5 * 1000

let make_conn_str ip port =
  "tcp://" ^ ip ^ ":" ^ (string_of_int port)

let make_bind_str port =
  "tcp://*:" ^ (string_of_int port)

let cleanup ctxt = ZMQ.Context.terminate ctxt

type 'a conn_type = {sock : 'a Sock.t; ctxt : ZMQ.Context.t}

let make_conn kind =
  let c = ctxt() in
  let s = Sock.create c kind in
  {sock=s;ctxt=c}

let clean_conn c =
  Sock.close c.sock;
  cleanup c.ctxt

open Unix
open Util

module ReqCtxt : RequesterContext = struct
  type 'a t = {
    port : int;
    hostname : string;
    conn : 'a conn_type;
  }
  constraint 'a = [> `Req]

  let make (conf : client_config) =
    let o = {
      port = conf.port;
      hostname = conf.remote_ip;
      conn = make_conn Sock.req;
    } in
    Util.debug_endline "Creating req ctxt";
    Sock.connect o.conn.sock (make_conn_str o.hostname o.port);
    Sock.set_send_timeout o.conn.sock five_seconds;
    Util.debug_endline "Created req context";
    o

  let close (c : [>`Req] t) =
    clean_conn c.conn; c

  let send m (c : [>`Req] t) =
    try
      dbg "Sending request";
      Sock.send c.conn.sock (Message.marshal m);
      dbg "Waiting for response";
      let resp = Sock.recv c.conn.sock in
      dbg "Received.";
      Message.unmarshal resp
    with
    | e -> Err e
end

module RespCtxt : ResponderContext = struct
  type 'a t = {
    port : int;
    conn : 'a conn_type;
    die : bool ref;
    die_lock : Mutex.t;
  }
  constraint 'a = [> `Rep]

  let make (conf : server_config) =
    let o = {
      port = conf.port;
      conn = make_conn Sock.rep;
      die = ref false;
      die_lock = Mutex.create ();
    } in
    Util.debug_endline "Creating resp ctxt";
    Sock.bind o.conn.sock (make_bind_str conf.port);
    Sock.set_send_timeout o.conn.sock five_seconds;
    Sock.set_receive_timeout o.conn.sock five_seconds;
    Util.debug_endline "Created resp context";
    o

  let close (c : [>`Rep] t) =
    Mutex.lock c.die_lock;
    c.die := true;
    Mutex.unlock c.die_lock;
    c

  let serve (f: Message.mes -> (Message.mes -> unit) -> unit) (c : [>`Rep] t) =
    let cbf m = Sock.send c.conn.sock (marshal m) in
    let rec loop () : unit =
      begin
        try
          dbg "Waiting for request";
          let req = Sock.recv c.conn.sock in
          (match (unmarshal req) with
          | Ok m -> dbg "Received request"; f m cbf
          | _ -> ());
        with
        | Unix_error (EAGAIN, "zmq_msg_recv", "") -> ()
      end;

      Mutex.lock c.die_lock;
      begin
        if (!(c.die) = true)
        then Mutex.unlock (c.die_lock)
        else (Mutex.unlock (c.die_lock); loop ())
      end
    in
    try_finally loop (fun () -> clean_conn c.conn)

end

module PubCtxt : PublisherContext = struct
  type 'a t = {
    port : int;
    conn : 'a conn_type;
  }
    constraint 'a = [> `Pub]

  let make (conf : server_config) =
    let o = {
      port = conf.port;
      conn = make_conn Sock.pub;
    } in
    Util.debug_endline "Creating pub ctxt";
    Sock.bind o.conn.sock (make_bind_str conf.port);
    Sock.set_send_timeout o.conn.sock five_seconds;
    Util.debug_endline "Created pub context";
    o

  let send m t =
    let messtr = (marshal m) in
    try
      dbg "Publishing...";
      Sock.send t.conn.sock messtr;
      dbg "Published";
    with
    | Unix_error (EAGAIN, _,_) -> raise Timeout

  let close t =
    clean_conn t.conn; t

end

module SubCtxt : SubscriberContext = struct
  type 'a t = {
    port : int;
    hostname : string;
    conn : 'a conn_type;
    die : bool ref;
    die_lock : Mutex.t;
  }
  constraint 'a = [> `Sub]

  let make (conf : client_config) =
    let o = {
      port = conf.port;
      hostname = conf.remote_ip;
      conn = make_conn Sock.sub;
      die = ref false;
      die_lock = Mutex.create ();
    } in
    Util.debug_endline "Creating sub ctxt";
    Sock.connect o.conn.sock (make_conn_str o.hostname o.port);
    Sock.subscribe o.conn.sock "";
    Sock.set_receive_timeout o.conn.sock five_seconds;
    Util.debug_endline "Created sub context";
    o

  let close c =
    Mutex.lock c.die_lock;
    c.die := true;
    Mutex.unlock c.die_lock;
    c

  let connect f c =
    let rec loop () : unit =
      begin
        try
          dbg "Waiting for publication...";
          let req = Sock.recv c.conn.sock in
          dbg "Received publication";
          (match (unmarshal req) with
           | Ok m -> (f m)
           | _ -> ());
        with
        | Unix_error (EAGAIN, "zmq_msg_recv", "") -> ()
      end;

      Mutex.lock c.die_lock;
      begin
        if (!(c.die) = true)
        then Mutex.unlock (c.die_lock)
        else (Mutex.unlock (c.die_lock); loop ())
      end
    in
    try_finally loop (fun () -> clean_conn c.conn)
end

module PushCtxt : PusherContext = struct
  type 'a t = {
    port : int;
    conn : 'a conn_type;
  }
    constraint 'a = [> `Push]

  let make (c : server_config) =
    let o = {
      port = c.port;
      conn = make_conn Sock.push;
    } in
    Util.debug_endline "Creating push ctxt";
    Sock.bind o.conn.sock (make_bind_str o.port);
    Util.debug_endline "Created push context";
    o

  let push (m : Message.mes) (t: 'a t) =
    Util.debug_endline "Sending push";
    Sock.send t.conn.sock (Message.marshal m);
    dbg "Sent push"

  let close t =
    clean_conn t.conn; t
end

(* TODO (this will need to loop, similar to resp) *)
module PullCtxt : PullerContext = struct
  type 'a t = {
    port : int;
    hostname : string;
    conn : 'a conn_type;
    die : bool ref;
    die_lock : Mutex.t;
  }
    constraint 'a = [> `Pull]

  let make (conf : client_config) =
    let o = {
      port = conf.port;
      hostname = conf.remote_ip;
      conn = make_conn Sock.pull;
      die = ref false;
      die_lock = Mutex.create ();
    } in
    Util.debug_endline "Creating pull ctxt";
    Sock.connect o.conn.sock (make_conn_str o.hostname o.port);
    Sock.set_send_timeout o.conn.sock five_seconds;
    Sock.set_receive_timeout o.conn.sock five_seconds;
    Util.debug_endline "Created pull ctxt";
    o

  let close (c: [> `Pull] t) =
    Mutex.lock c.die_lock;
    c.die := true;
    Mutex.unlock c.die_lock;
    c

  let connect (f : Message.mes -> unit) (t : 'a t) =
    (* hostname is the remote from the perspective of the puller *)
    (* pull the data from the port on the remote *)
    (* call f when there is data pushed from the remote *)
    let rec loop () : unit =
      begin
        try
          dbg "Pulling...";
          let pull = Sock.recv t.conn.sock in
          (match (Message.unmarshal pull) with
          | Ok m -> dbg "Pulled."; f m
          | _ -> ());
        with
        | Unix_error (EAGAIN, "zmq_msg_recv", "") -> ()
      end;

      Mutex.lock t.die_lock;
      begin
        if (!(t.die) = true)
        then Mutex.unlock (t.die_lock)
        else (Mutex.unlock (t.die_lock); loop ())
      end
    in
    try_finally loop (fun () -> clean_conn t.conn)

end
