(* Spawns a thread and runs the given function on it. *)
val run_in_background : (unit -> unit) -> Thread.t

(* Same as run_in_background *)
val (!->) : (unit -> unit) -> Thread.t

(* Equivalent to Thread.kill *)
val (!!!) : Thread.t -> unit

(* Equivalent to Thread.join *)
val (???) : Thread.t -> unit
