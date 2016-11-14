open OUnit
open Message
open Errable

(* TODO: add test cases using base64 encoding (where appropriate) *)

let heartbeatresp = HeartbeatResp "123.456.78.90"
let testcomp = TestCompletion ("abc12", "Passed all tests")
let filereq = FileReq "abc12"
let heartbeat = Heartbeat (10.5, false)
let testspec = TestSpec ("common", 60, [])
let files = Files []

let testspec2 = TestSpec ("common", 60, ["ls"; "cd ~"])
let files2 = Files [("message.ml", "type ip = string");
                    ("file.ml", "type file = string * string")]


let msg_tests = "Message tests" >::: [

  "marshal heartbeatresp" >::
    (fun _ -> assert_equal
      "{\"heartbeat_resp\":\"123.456.78.90\"}"
      (marshal heartbeatresp));

  "marshal testcomp" >::
    (fun _ -> assert_equal
      "{\"netid\":\"abc12\",\"results\":\"Passed all tests\"}"
      (marshal testcomp));

  "marshal filereq" >::
    (fun _ -> assert_equal
      "{\"files\":\"abc12\"}"
      (marshal filereq));

  "marshal heartbeat" >::
    (fun _ -> assert_equal
      "{\"heartbeat\":10.5,\"done\":false}"
      (marshal heartbeat));

  "marshal testspec" >::
    (fun _ -> assert_equal
      "{\"key\":\"common\",\"timeout\":60,\"commands\":[]}"
      (marshal testspec));

  "marshal testspec2" >::
    (fun _ -> assert_equal
      "{\"key\":\"common\",\"timeout\":60,\"commands\":[\"ls\",\"cd ~\"]}"
      (marshal testspec2));

  "marshal files" >::
    (fun _ -> assert_equal
      "[]"
      (marshal files));

  "marshal files2" >::
    (fun _ -> assert_equal
      "[{\"message.ml\":\"type ip = string\"},{\"file.ml\":\"type file = string * string\"}]"
      (marshal files2));

  "unmarshal heartbeatresp" >::
    (fun _ -> assert_equal (Ok heartbeatresp)
      (unmarshal "{\"heartbeat_resp\":\"123.456.78.90\"}"));

  "unmarshal testcomp" >::
    (fun _ -> assert_equal (Ok testcomp)
      (unmarshal "{\"netid\":\"abc12\",\"results\":\"Passed all tests\"}"));

  "unmarshal filereq" >::
    (fun _ -> assert_equal (Ok filereq)
      (unmarshal "{\"files\":\"abc12\"}"));

  "unmarshal heartbeat" >::
    (fun _ -> assert_equal (Ok heartbeat)
      (unmarshal "{\"heartbeat\":10.5,\"done\":false}"));

  "unmarshal testspec" >::
    (fun _ -> assert_equal (Ok testspec)
      (unmarshal "{\"key\":\"common\",\"timeout\":60,\"commands\":[]}"));

  "unmarshal testspec2" >::
    (fun _ -> assert_equal (Ok testspec2)
      (unmarshal "{\"key\":\"common\",\"timeout\":60,\"commands\":[\"ls\",\"cd ~\"]}"));

  "unmarshal files" >::
    (fun _ -> assert_equal (Ok files)
      (unmarshal "[]"));

  "unmarshal files2" >::
    (fun _ -> assert_equal (Ok files2)
      (unmarshal
      "[{\"message.ml\":\"type ip = string\"},{\"file.ml\":\"type file = string * string\"}]"));

  "unmarshal empty string" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal ""));

  "unmarshal empty braces" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{}"));

  "unmarshal bad heartbeatresp: misspelled field" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"heartbeat_res\":\"123.456.78.90\"}"));

  "unmarshal bad heartbeatresp: wrong field type" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"heartbeat_resp\":123}"));

  "unmarshal bad testcomp: misspelled field" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"neti\":\"abc12\",\"results\":\"Passed all tests\"}"));

  "unmarshal bad testcomp: missing field" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"netid\":\"abc12\"}"));

  "unmarshal bad testcomp: wrong field type 1" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"netid\":12,\"results\":\"Passed all tests\"}"));

  "unmarshal bad testcomp: wrong field type 2" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"netid\":\"abc12\",\"results\":2}"));

  "unmarshal bad filereq: misspelled field" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"file\":\"abc12\"}"));

  "unmarshal bad filereq 2: wrong field type" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"files\":5}"));

  "unmarshal bad heartbeat: misspelled field" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"heartbea\":10.5,\"done\":false}"));

  "unmarshal bad heartbeat: missing field" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"heartbeat\":0.0}"));

  "unmarshal bad heartbeat: wrong field type" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"heartbeat\":\"hello\",\"done\":false}"));

  "unmarshal bad testspec: misspelled field 1" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"keey\":\"common\",\"timeout\":60,\"commands\":[]}"));

  "unmarshal bad testspec: missing field 1" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"key\":\"common\",\"timeout\":60}"));

  "unmarshal bad testspec: missing field 2" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"key\":\"common\",\"commands\":[\"ls\",\"cd ~\"]}"));

  "unmarshal bad testspec: misspelled field 2" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"keey\":\"common\",\"timeout\":60,\"commands\":[\"ls\",\"cd ~\"]}"));

  "unmarshal bad testspec: wrong field type 1" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"key\":5,\"timeout\":60,\"commands\":[\"ls\",\"cd ~\"]}"));

  "unmarshal bad testspec: wrong field type 2" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"key\":\"common\",\"timeout\":60,\"commands\":\"ls\"}"));

  "unmarshal bad files: not a file list" >::
    (fun _ -> assert_equal (Err Bad_message)
      (unmarshal "{\"message.ml\":\"type ip = string\"}"));

  (* There may be a problem with how we're running tests (aka test.ml) *)
  (* Or not, but why would unmarshal tests not work here but work in utop?? *)
  (* Also, should an empty file list raise Bad_message or not?? *)

]

let () = Test.add_test msg_tests