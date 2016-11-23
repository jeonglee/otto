open OUnit
open Errable
open FileCrawler

let fails _ = Err Not_found

let files_from_dir_tests =
   "files_from_dir tests" >::: [
      "non-existent directory" >:: (fun _ ->
          assert_raises Not_found (fun _ -> (files_from_dir "abc") |> (?!)));
      "non-existent directory" >:: (fun _ ->
          assert_equal (Err Not_found) (files_from_dir "abc"));
      "testFileCrawler" >:: (fun _ ->
          assert_equal [("testFileCrawler/test.txt","hello world!\n")] ((files_from_dir "testFileCrawler") |> (?!)));
    ]

let () = Test.add_test files_from_dir_tests
