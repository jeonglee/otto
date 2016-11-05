type 'a errable =
  | Ok of 'a
  | Err of exn

(*
val (>>=) : 'a errable -> ('a -> 'b errable) -> 'b errable
val (>>>) : 'a errable -> ('a -> 'b) -> 'b errable
*)

let (>>=) (e : 'a errable) (f : 'a -> 'b errable) =
  match e with
  | Ok a -> f a
  | Err e -> Err e

let (>>>) e f =
  match e with
  | Ok a ->
      begin
        try
          Ok (f a)
        with
        | e -> Err e
      end
  | Err e -> Err e

let (?!) = function
  | Ok a -> a
  | Err e -> raise e
