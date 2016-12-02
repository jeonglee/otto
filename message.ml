open Yojson.Basic.Util
open Yojson.Basic
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

let coerce_timeout (t : timeout) : int = t
let coerce_ip (t : ip) : string = t
let coerce_time (t : time) : float = t
let coerce_test_key (t : test_key) : string = t
let coerce_com (t : command) : string = t

(* [marshal m] converts a message of type mes into the JSON string
 * representing it *)
let marshal m =
  let js =
    match m with
    | HeartbeatResp ip ->
        (`Assoc ["heartbeat_resp",(`String (coerce_ip ip))])
    | TestCompletion (key, str) ->
        (`Assoc [("netid",(`String (coerce_test_key key))); ("results",(`String str))])
    | FileReq key -> `Assoc [("files",`String (coerce_test_key key))]
    | Heartbeat (t, d) ->
        (`Assoc [("heartbeat",`Float t);("done",`Bool d)])
    | TestSpec (key, timeout, lst) ->
        (let cml = List.map (fun s -> `String (coerce_com s)) lst in
        `Assoc [("key",`String (coerce_test_key key));
                ("timeout",`Int (coerce_timeout timeout));
                ("commands",`List cml)])
    | Files lst -> (`List (List.map (fun (f,c) -> `Assoc [(f,`String c)]) lst))
  in
  to_string js

let rec extract_files jlst acc =
  match jlst with
  | (`Assoc [(s, `String contents)])::tl -> extract_files tl ((s, contents)::acc)
  | [] -> List.rev acc
  | _ -> raise Bad_message

(* [unmarshal s] converts a message from its JSON serialization into
 * a mes. Raises Bad_message if the JSON is not a valid message *)
let unmarshal (m:string) : mes errable =
  let j =
    begin
      try Yojson.Basic.from_string m
      with _ -> `Null
    end
  in
  match j with
  | `Assoc [("heartbeat_resp", `String s)] -> Ok(HeartbeatResp s)
  | `Assoc [("netid", `String k);("results", `String r)] -> Ok(TestCompletion (k, r))
  | `Assoc [("files", `String k)] -> Ok(FileReq k)
  | `Assoc [("heartbeat", `Float t);("done", `Bool d)] -> Ok(Heartbeat (t, d))
  | `Assoc [("key", `String k);("timeout", `Int t);("commands", `List lst)] ->
    let clst = convert_each Util.to_string (`List lst) in
    Ok(TestSpec (k, t, clst))
  | `Assoc _ -> Err Bad_message
  | `List lst ->
    begin
      try let f = extract_files lst [] in Ok (Files f)
      with e -> Err e
    end
  | _ -> Err Bad_message
