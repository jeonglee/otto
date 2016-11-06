exception Invalid_ctxt
exception Timeout

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
  type t

  (* [make init] creates a requester context *)
  val make : 'a -> t
  (* [send mess t] sends a message using an initialized context t.
   * it should block until it receives a response or errors out. *)
  val send : Message.mes -> t -> Message.mes

  (* [close t] frees whatever resources were opened in making the context *)
  val close : t -> t
end

(* Represents a responder in the requester-responder paradigm *)
module type ResponderContext = sig
  type t

  (* [make init] creates a responder context *)
  val make : server_config -> t

  (* [serve f t] blocks and serves a function with [f] as the method of
   *  responding to requests. *)
  val serve : (Message.mes -> unit) -> t -> unit

  (* [close t] frees whatever resources were opened in making the context *)
  val close : t -> t
end

module type PublisherContext = sig
  type t

  (* [make init] creates a publisher context *)
  val make : 'a -> t

  (* [serve t] blocks and waits to broadcast when send is called on this context *)
  val serve : server_config -> unit

  (* [send m t] broadcasts [m] via context [t] to any subscribers *)
  val send : Message.mes -> t -> unit

  (* [close t] frees whatever resources were opened in making the context *)
  val close : t -> t
end

(* Represents a subscriber in the pub/sub paradigm. A subscriber
 * receives all events broadcast by a publisher after the subscriber
 * connects to the publisher. *)
module type SubscriberContext = sig
  type t

  (* [make init] creates a subscriber context *)
  val make : 'a -> t

  (* [connect f t] connects to a publisher and calls f when the publisher
   * sends a message *)
  val connect : (Message.mes -> unit) -> t -> unit

  (* [close t] frees whatever resources were opened in making the context *)
  val close : t -> t
end

module type PusherContext = sig
  type t

  (* [make init] creates a pusher context *)
  val make : 'a -> t

  val serve : server_config -> unit

  val push : Message.mes -> t -> unit

  val close : t -> t
end

(* Represents a puller in the push/pull paradigm. All pullers
 * compete to receive any events broadcast by a pusher while the
 * pullers are connected to the pusher. *)
module type PullerContext = sig
  type t

  (* [make init] creates a pullers context *)
  val make : 'a -> t

  (* [connect f t] connects to a pusher and calls f when the pusher
   * sends a message *)
  val connect : (Message.mes -> unit) -> t -> unit

  (* [close t] frees whatever resources were opened in making the context *)
  val close : t -> t
end



module ReqCtxt : RequesterContext
module RespCtxt : ResponderContext
module PubCtxt : PublisherContext
module SubCtxt : SubscriberContext
module PushCtxt : PullerContext
module PullCtxt : PusherContext
