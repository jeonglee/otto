open OUnit
open Errable
open Comm
open Message
open Async

let test_serve m f =
  f m

let run_request_server () =
  let o = RespCtxt.make {port=9999} in o

let test_echo me =
  let res = run_request_server () in
  let rest = !-> (fun () -> RespCtxt.serve test_serve res) in
  let req = ReqCtxt.make {port=9999;remote_ip="localhost"} in
  let response = ReqCtxt.send me req in
  ignore (ReqCtxt.close req);
  ignore (RespCtxt.close res);
  ??? rest;
  ?! response

let test = "Comm Tests" >::: [
    "Echo test" >:: (fun _ ->
        assert_equal (FileReq "Hey") (test_echo (FileReq "Hey")))
  ]

let () = Test.add_test test
