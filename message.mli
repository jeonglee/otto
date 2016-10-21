exception Bad_message

(* node_status represents the status of a node with respect
 * to node balancing.
 * [Ok] represents a node which is not Busy
 * [Busy] represents a node which is handling the maximum number of bots
 * it is configured for. *)
type node_status =
  | Ok
  | Busy

(* mes represents the types of messages that can be sent between participants
 * on the botnet *)
type mes =
  | Query of string
  | NodeStatus of node_status
  | Command of string
  | Nodes of string list * float
  | Heartbeat of float
  | HeartbeatResp of string

(* [marshal m] converts a message of type mes into the JSON string
 * representing it *)
val marshal : mes -> string

(* [unmarshal s] converts a message from its JSON serialization into
 * a mes. Raises Bad_message if the JSON is not a valid message *)
val unmarshal : string -> mes
