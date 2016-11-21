open OUnit
open Errable
open FileCrawler

let fails _ = Err Not_found

let files_from_dir_tests =
   "files_from_dir tests" >::: [
      "non-existent directory" >:: (fun _ ->
          assert_equal (Err Not_found) (files_from_dir "abc"));
      (* "directory- 0 levels" >:: [
          assert_equal (Ok [("test.txt","hello world!")]) (files_from_dir "testFileCrawler");
      ] *)
    ]

let () = Test.add_test files_from_dir_tests
