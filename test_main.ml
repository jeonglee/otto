open OUnit
open Message

open Test_errable
open Test_message

let _ = OUnit.run_test_tt_main ("Unit tests" >::: (Test.get_tests ()))
