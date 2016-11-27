let try_finally (f : unit -> 'a) (final : unit -> unit) =
  try
    let r = f () in
    final (); r
  with
  | e -> final (); raise e

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fmap : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
  val coerce : 'a t -> 'a

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>>) : 'a t -> ('a -> 'b) -> 'b t
  val (?!) : 'a t -> 'a
end

module type ONCE = sig
  type 'a t

  val make : unit -> 'a t
  val is_val : 'a t -> bool
  val coerce : 'a t -> 'a
  val set : 'a -> 'a t -> unit
end

module Once : ONCE = struct
  type 'a t = {contents : 'a option ref; lock : Mutex.t}

  let make () = {contents = ref None; lock = Mutex.create ()}

  let is_val o =
    Mutex.lock (o.lock);
    let res =
      match !(o.contents) with
      | Some _ -> true
      | None -> false
    in
    Mutex.unlock (o.lock);
    res

  let coerce o =
    Mutex.lock (o.lock);
    try_finally (
      fun () ->
        match !(o.contents) with
        | None -> failwith "Can't coerce empty Once.t"
        | Some x -> x
    ) (fun () -> Mutex.unlock (o.lock))

  let set x o =
    Mutex.lock (o.lock);
    try_finally (
      fun () ->
        match !(o.contents) with
        | None -> o.contents := (Some x)
        | Some _ -> failwith "Already set"
    ) (fun () -> Mutex.unlock (o.lock))

end

module Maybe : (Monad with type 'a t = 'a option) = struct
  type 'a t = 'a option

  let return x = Some x

  let bind x f =
    match x with
    | Some x -> f x
    | None -> None

  let fmap x f =
    bind x (fun y -> Some (f y))

  let join = function
    | Some x -> x
    | None -> None

  let coerce = function
    | Some x -> x
    | None -> failwith "option did not contain value"

  let (>>=) = bind
  let (>>>) = fmap
  let (?!) = coerce
end

let read_all_lines filename =
  let c = open_in filename in
  let rec read_lines lines ch =
    try
      read_lines ((input_line ch)::lines) ch
    with
    | End_of_file -> List.rev lines
  in
  let o = read_lines [] c in
  close_in_noerr c;
  o

let remove_extension fname =
  let parts = Str.split (Str.regexp ".") fname in
  match parts with
  | [] -> ""
  | h::t -> h
