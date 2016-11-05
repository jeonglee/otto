open Errable

type config = {
  remote_port : int;
  remote_ip : Message.ip;
  test_dir : string;
}

module type Client = sig
  type t

  val make : config -> t errable

  val main : t -> unit errable

  val close : t -> t

end

module ClientImpl : Client
