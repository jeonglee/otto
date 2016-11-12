open OUnit
open Message

let _ = OUnit.run_test_tt_main ("Unit tests" >::: (Test.get_tests ()))
