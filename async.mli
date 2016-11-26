val run_in_background : (unit -> unit) -> Thread.t

val (!->) : (unit -> unit) -> Thread.t
val (!!!) : Thread.t -> unit
val (???) : Thread.t -> unit
