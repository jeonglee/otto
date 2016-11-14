exception Invalid_ctxt
exception Timeout

open Errable

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

let ctxt = ZMQ.Context.create ()

let five_seconds = 5 * 1000

let make_conn_str ip port =
  "tcp://" ^ ip ^ ":" ^ (string_of_int port)

let make_bind_str port =
  "tcp://*:" ^ (string_of_int port)

let cleanup () =
  ZMQ.Context.terminate ctxt

let () = at_exit cleanup

module ReqCtxt : RequesterContext = struct
  type 'a t = {
    port : int;
    hostname : string;
    sock : 'a Sock.t;
  }
  constraint 'a = [> `Req]

  let make (conf : client_config) =
    let o = {
      port = conf.port;
      hostname = conf.remote_ip;
      sock = Sock.create ctxt Sock.req;
    } in
    Sock.connect o.sock (make_conn_str o.hostname o.port);
    Sock.set_send_timeout o.sock five_seconds;
    Sock.set_receive_timeout o.sock five_seconds;
    o

  let close (c : [>`Req] t) =
    Sock.close c.sock; c

  let send m (c : [>`Req] t) =
    try
      Sock.send c.sock (Message.marshal m);
      let resp = Sock.recv c.sock in
      Message.unmarshal resp
    with
    | e -> Err e
end

module RespCtxt : ResponderContext = struct
  type 'a t = {
    port : int;
    sock : 'a Sock.t;
    die : bool ref;
    die_lock : Mutex.t;
  }
  constraint 'a = [> `Rep]

  let make (conf : server_config) =
    let o = {
      port = conf.port;
      sock = Sock.create ctxt Sock.rep;
      die = ref false;
      die_lock = Mutex.create ();
    } in
    Sock.bind o.sock (make_bind_str conf.port);
    Sock.set_send_timeout o.sock five_seconds;
    Sock.set_receive_timeout o.sock five_seconds;
    o

  let close (c : [>`Rep] t) =
    Mutex.lock c.die_lock;
    c.die := true;
    Mutex.unlock c.die_lock;
    c

  let serve (f: Message.mes -> (Message.mes -> unit) -> unit) (c : [>`Rep] t) =
    let cbf m = Sock.send c.sock (marshal m) in
    let rec loop () =
      let req = Sock.recv c.sock in
      (match (unmarshal req) with
      | Ok m -> f m cbf
      | _ -> ());
      Mutex.lock c.die_lock;
      if !(c.die)
      then (Mutex.unlock c.die_lock; Sock.close c.sock)
      else Mutex.unlock c.die_lock; loop ()
    in
    loop ()

end

module PubCtxt : PublisherContext = struct
  type 'a t = {
    port : int;
    sock : 'a Sock.t;
  }
    constraint 'a = [> `Pub]

  let make c =
    failwith "Unimplemented"

  let send m t =
    failwith "Unimplemented"

  let close t =
    failwith "Unimplemented"

end

module PushCtxt : PusherContext = struct
  type 'a t = {
    port : int;
    sock : 'a Sock.t;
  }
    constraint 'a = [> `Push]

  let make c =
    failwith "Unimplemented"

  let push m t =
    failwith "Unimplemented"

  let close t =
    failwith "Unimplemented"

end

module SubCtxt : SubscriberContext = struct
  type 'a t = {
    port : int;
    hostname : string;
    sock : 'a Sock.t;
  }
  constraint 'a = [> `Sub]

  let make (conf : client_config) =
    failwith "Unimplemented"

  let close c =
    failwith "Unimplemented"

  let connect f t =
    failwith "Unimplemented"
end

module PullCtxt : PullerContext = struct
  type 'a t = {
    port : int;
    hostname : string;
    sock : 'a Sock.t;
  }
    constraint 'a = [> `Pull]

  let make (conf : client_config) =
    failwith "Unimplemented"

  let close c =
    failwith "Unimplemented"

  let connect f t =
    failwith "Unimplemented"
end
