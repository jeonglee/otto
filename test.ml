
let tests = ref []

let add_test test =
  tests := test::(!tests)

let add_tests lst =
  tests := (!tests) @ lst

let get_tests () = !tests
