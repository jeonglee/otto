(* Represents the output of a function that can fail.
 * useful for putting errors explicit in the type of
 * functions *)
 type 'a errable =
   | Ok of 'a
   | Err of exn

module M : (Util.Monad with type 'a t = 'a errable) = struct
  type 'a t = 'a errable

  let (>>=) (e : 'a t) (f : 'a -> 'b t) =
    match e with
    | Ok a -> f a
    | Err e -> Err e

  let (?!) = function
    | Ok a -> a
    | Err e -> raise e

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

  let coerce = (?!)
  let bind = (>>=)
  let fmap = (>>>)
  let join t = coerce t
  let return x = Ok x
end
