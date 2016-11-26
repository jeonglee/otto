open OUnit
open Errable
open FileCrawler
open Unix
open Str


let fails _ = Err Not_found

let files_from_dir_tests =
   "files_from_dir tests" >::: [
      "non-existent directory" >:: (fun _ ->
          assert_raises Not_found (fun _ -> (files_from_dir "abc") |> (?!)));
      "non-existent directory" >:: (fun _ ->
          assert_equal (Err Not_found) (files_from_dir "abc"));
      "testFileCrawler" >:: (fun _ ->
          assert_equal
          [("testFileCrawler/test.txt", "aGVsbG8gd29ybGQhCg==");
          ("testFileCrawler/one.txt", "");
          ("testFileCrawler/new1.txt", "Y2FtZWw=");
          ("testFileCrawler/new.txt", "b3R0bw==")]
          ((files_from_dir "testFileCrawler") |> (?!)));
    ]

let file_to_string f =
  let in_channel = open_in f in
  let len = in_channel_length in_channel in
  let s = Bytes.create len in
  really_input in_channel s 0 len;
  close_in in_channel;
  Bytes.to_string s

let write_file_tests =
   "write_file_tests tests" >::: [
      "write to existent file" >:: (fun _ ->
          let _ = (write_file ~dir:"testFileCrawler" ("one.txt", "hello")) in
          assert_equal
            (file_to_string "testFileCrawler/one.txt")
            ("hello"));
      "write to existent file (type)" >:: (fun _ ->
          assert_equal
            (Ok ())
            (write_file ~dir:"testFileCrawler" ("one.txt", "hello")));
      "write empty to existent file" >:: (fun _ ->
          let _ = (write_file ~dir:"testFileCrawler" ("one.txt", "")) in
          assert_equal
            (file_to_string "testFileCrawler/one.txt")
            (""));
      "write to nonexistent file" >:: (fun _ ->
          let _ = (write_file ~dir:"testFileCrawler" ("new.txt", "otto")) in
          assert_equal
            (file_to_string "testFileCrawler/new.txt")
            ("otto"));
      "write to nonexistent file (type)" >:: (fun _ ->
          assert_equal
            (Ok ())
            (write_file ~dir:"testFileCrawler" ("new1.txt", "camel")));
    ]

let () = Test.add_test files_from_dir_tests
let () = Test.add_test write_file_tests
