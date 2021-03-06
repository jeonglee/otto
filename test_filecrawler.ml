open OUnit
open Errable
open Errable.M
open FileCrawler
open Unix
open Str


let fails _ = Err Not_found

let print_l l =
  List.map (fun (f,c) -> f ^ ":" ^ c) l
  |> String.concat "\n"

let files_from_dir_tests =
   "files_from_dir tests" >::: [
      "non-existent directory" >:: (fun _ ->
         assert_raises (Unix.Unix_error(Unix.ENOENT,"opendir", "abc"))
           (fun _ -> (files_from_dir "abc") |> (?!)));
      "non-existent directory" >:: (fun _ ->
          assert_equal (Err (Unix.Unix_error(Unix.ENOENT,"opendir", "abc")))
            (files_from_dir "abc"));
      "testFileCrawler" >:: (fun _ ->
          assert_equal ~printer:print_l
            (List.sort compare [("testFileCrawler/test.txt", "aGVsbG8gd29ybGQhCg==");
              ("testFileCrawler/one.txt", "");
              ("testFileCrawler/new1.txt", "Y2FtZWw=");
              ("testFileCrawler/new.txt", "b3R0bw==")])
            ((files_from_dir "testFileCrawler") |> (?!) |> List.sort compare));
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
         let _ = (write_file ~dir:"testFileCrawler" ("one.txt", "aGVsbG8=")) in
          assert_equal
            (file_to_string "testFileCrawler/one.txt")
            ("hello"));
      "write to existent file (type)" >:: (fun _ ->
          assert_equal
            (Ok ())
            (write_file ~dir:"testFileCrawler" ("one.txt", "aGVsbG8=")));
      "write empty to existent file" >:: (fun _ ->
          let _ = (write_file ~dir:"testFileCrawler" ("one.txt", "")) in
          assert_equal
            (file_to_string "testFileCrawler/one.txt")
            (""));
      "write to nonexistent file" >:: (fun _ ->
          let _ = (write_file ~dir:"testFileCrawler" ("new.txt", "b3R0bw==")) in
          assert_equal
            (file_to_string "testFileCrawler/new.txt")
            ("otto"));
      "write to nonexistent file (type)" >:: (fun _ ->
          assert_equal
            (Ok ())
            (write_file ~dir:"testFileCrawler" ("new1.txt", "Y2FtZWw=")));
    ]

let () = Test.add_test files_from_dir_tests
let () = Test.add_test write_file_tests
