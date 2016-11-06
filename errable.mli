(* Represents the output of a function that can fail.
 * useful for putting errors explicit in the type of
 * functions *)
type 'a errable =
  | Ok of 'a
  | Err of exn

(* a >>= f takes an errable and passes it into function f, producing another errable *)
val (>>=) : 'a errable -> ('a -> 'b errable) -> 'b errable

(* a >>> f takes an errable and passes it into the non-errable function f,
 * producing an errable if f raises and exception. *)
val (>>>) : 'a errable -> ('a -> 'b) -> 'b errable

(* ?!a extracts the value from an errable, or raises the exception inside it. *)
val (?!) : 'a errable -> 'a
