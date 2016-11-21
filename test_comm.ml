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
  ignore (ReqCtxt.close req);
  ignore (RespCtxt.close res);
  ??? rest;
  (?! resp1, ?! resp2)


let counter = ref 0
let counter_mut = Mutex.create ()
let update_counter =
  fun () ->
    begin
      Mutex.lock counter_mut;
      incr counter;
      let i = !counter in
      Mutex.unlock counter_mut;
      i
    end

let test_pub_sub () =
  let pub = PubCtxt.make {port=9999} in
  let sub1 = SubCtxt.make {port=9999;remote_ip="localhost"} in
  let sub1t = !-> (fun () -> SubCtxt.connect (fun _ -> ignore (update_counter ())) sub1) in
  let sub2 = SubCtxt.make {port=9999;remote_ip="localhost"} in
  let sub2t = !-> (fun () -> SubCtxt.connect (fun _ -> ignore (update_counter ())) sub2) in
  Thread.delay 5.0;
  PubCtxt.send (Heartbeat (1.0, false)) pub;
  Thread.delay 5.0;
  ignore (SubCtxt.close sub1);
  ignore (SubCtxt.close sub2);
  ignore (PubCtxt.close pub);
  ??? sub1t;
  ??? sub2t;
  update_counter ()


let test = "Comm Tests" >::: [


    "Pub/Sub test" >:: (fun _ ->
        assert_equal ~printer:string_of_int 3 (test_pub_sub ()));

        "Echo test" >:: (fun _ ->
            assert_equal (FileReq "Hey", FileReq "Hey") (test_echo (FileReq "Hey")));
  ]

let () = Test.add_test test
