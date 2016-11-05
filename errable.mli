(* Represents the output of a function that can fail.
 * useful for putting errors explicit in the type of
 * functions *)
type 'a errable =
  | Ok of 'a
  | Err of exn

val (>>=) : 'a errable -> ('a -> 'b errable) -> 'b errable
val (>>>) : 'a errable -> ('a -> 'b) -> 'b errable
val (?!) : 'a errable -> 'a
