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
