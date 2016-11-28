open OUnit
open Impl

let tests = "adder tests" >::: [
    "add one" >:: (fun _ ->
        assert_equal 3 (add_two 1));

    "add two" >:: (fun _ ->
        assert_equal 4 (add_two 2))
]

let _ = run_test_tt_main tests
