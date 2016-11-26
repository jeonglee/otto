(* Represents the output of a function that can fail.
 * useful for putting errors explicit in the type of
 * functions *)
type 'a errable =
  | Ok of 'a
  | Err of exn

module M : (Util.Monad with type 'a t = 'a errable)
