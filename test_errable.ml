open OUnit
open Errable
open Errable.M

let errable_add x y = Ok (x + y)
let fails _ = Err Not_found

let errable_tests =
    "Errable Tests" >::: [
      "Functional test" >:: (fun _ ->
          assert_equal 2 ((Ok 1) >>= (errable_add 1) |> (?!)));

      "Functional test >>>" >:: (fun _ ->
          assert_equal 2 ((Ok 1) >>> ((+) 1) |> (?!)));

      "Functional chain test" >:: (fun _ ->
          assert_equal 3 ((Ok 1) >>= (errable_add 1) >>> ((+) 1) |> (?!)));

      "Failure test" >:: (fun _ ->
          assert_raises Not_found (fun _ -> fails 1 |> (?!)));

      "Chain failure test" >:: (fun _ ->
          assert_raises Not_found (fun _ -> (Ok 1) >>= (errable_add 1) >>> ((+) 1) >>= fails |> (?!)))

    ]

let () = Test.add_test errable_tests
