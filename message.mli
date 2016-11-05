open Errable

exception Bad_message

type ip = string
type test_key = string
type timeout = int
type time = float
type command = string

(* mes represents the types of messages that can be sent between participants
 * on the botnet *)
type mes =
  | HeartbeatResp of ip
  | TestCompletion of test_key * string
  | FileReq of test_key
  | Heartbeat of time * bool
  | TestSpec of test_key * timeout * command list
  | Files of FileCrawler.file list

(* [marshal m] converts a message of type mes into the JSON string
 * representing it *)
val marshal : mes -> string

(* [unmarshal s] converts a message from its JSON serialization into
 * a mes. Raises Bad_message if the JSON is not a valid message *)
val unmarshal : string -> mes errable
