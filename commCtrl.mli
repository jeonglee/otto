open Errable

type config =
  {
    base_port: int;
    dir: string;
    test_dir : string;
    common_dir : string;
    test_timeout : Message.timeout;
    client_timeout : Message.timeout;
    command_file : string;
  }

module type Commander = sig
  type t

  val make : config -> t errable

  val main : t -> unit errable

  val close : t -> t

end

module CommanderImpl : Commander
