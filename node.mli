
module type Node = sig
    type t
    
    (* Create a new node *)
    val make : 'a -> t

    (* [num_bots t] returns the current number of bots
     * being tracked by t *)
    val num_bots : t -> int

    (* [status t] returns the current load status of the node *)
    val status : t -> Message.node_status

    (* [add_bot ip t] adds the bot at ip [ip] to [t] as being currently tracked *)
    val add_bot : string -> t -> t

    (* [clean_bots t] cleans out any timed-out or failed bots from the node *)
    val clean_bots : t -> t
end
