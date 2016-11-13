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

(* TODO: add commas somehow*)

let rec get_clst lst acc = match lst with
  | h::t -> get_clst t (acc ^ "\"" ^ h ^ "\",")
  | [] -> if acc = "" then "" else String.sub acc 0 (String.length acc - 1)

(* TODO: add commas somehow*)

let rec get_flst lst acc = match lst with
  | (n, c)::t -> get_flst t (acc ^ "{\"" ^ n ^ "\":\"" ^ c ^ "\"},")
  | [] -> if acc = "" then "" else String.sub acc 0 (String.length acc - 1)

(* [marshal m] converts a message of type mes into the JSON string
 * representing it *)
let marshal = function
  | HeartbeatResp ip ->
    "{\"heartbeat_resp\":\"" ^ ip ^ "\"}"
  | TestCompletion (key, str) ->
    "{\"netid\":\"" ^ key ^ "\",\"results\":\"" ^ str ^ "\"}"
  | FileReq key -> "{\"files\":\"" ^ key ^ "\"}"
  | Heartbeat (t, d) ->
    "{\"heartbeat\":" ^ (string_of_float t) ^ ",\"done\":" ^ (string_of_bool d) ^ "}"
  | TestSpec (key, timeout, lst) ->
    let clst = get_clst lst "" in
    "{\"key\":\"" ^ key ^ "\",\"timeout\":" ^ (string_of_int timeout) ^ ",\"commands\":[" ^ clst ^ "]}"
    (* TODO: Add in get_clst for commands field, update docs *)
  | Files lst ->
    let flst = get_flst lst "" in
    "[" ^ flst ^ "]"

(* [unmarshal s] converts a message from its JSON serialization into
 * a mes. Raises Bad_message if the JSON is not a valid message *)
let unmarshal = function
  | _ -> failwith "Unimplemented"
