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
  let resp1 = ReqCtxt.send me req in
  let resp2 = ReqCtxt.send me req in
  print_endline "Close 1";
  ignore (ReqCtxt.close req);
  print_endline "Close 2";
  ignore (RespCtxt.close res);
  print_endline "Close 3";
  ??? rest;
  (?! resp1, ?! resp2)

let test = "Comm Tests" >::: [
    "Echo test" >:: (fun _ ->
        assert_equal (FileReq "Hey", FileReq "Hey") (test_echo (FileReq "Hey")))
  ]

let () = Test.add_test test
