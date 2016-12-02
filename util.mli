(* try_finally f e runs the function f, then, regardless if f
   terminates with an error or not, executes e. Errors are propagated after,
   and the result of f is  returned. *)
val try_finally : (unit -> 'a) -> (unit -> unit) -> 'a

(* Converts a file into the lines contained inside. *)
val read_all_lines : string -> string list

(* Removes the extension from a filename. file.txt -> file *)
val remove_extension : string -> string

(* represents the canonical Monad, but with an added function for coercing the
value contained inside Monad. *)
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

(* Once represents a value that can be set once, and no more.
   Additional attempts to set a value return an error.
   Attempting to coerce an empty Once.t raises an error. *)
module type ONCE = sig
  type 'a t

  val make : unit -> 'a t
  val is_val : 'a t -> bool
  val coerce : 'a t -> 'a
  val set : 'a -> 'a t -> unit
end

module Once : ONCE

(* The same as the Maybe monad used in recitation. *)
module Maybe : Monad with type 'a t = 'a option

(* Controls debug output. If set to true, all calls to debug_endline
   will print to stdout. Otherwise, they will be silent. *)
val set_debug : bool -> unit
val debug_endline : string -> unit

(* Additional functions for working with strings and regular expressions *)
module Strs : sig
  (* [split delim s] splits a string s with delim as the delimiting regex *)
  val split : string -> string -> string list
  (* Same as split, but splits on whitespace. *)
  val split_whitespace : string -> string list

(* [replace_substr tbr rw s] replaces the regex matches of tbr with rw in the
   string s *)
  val replace_substr : string -> string -> string -> string
end

(* Functions for dealing with Unix processes. *)
module Proc : sig
  (* Returns a list of the descendent processes of the given process id. *)
  val desc_proc_ids : int -> int list
  (* Kills all descendent processes of the process with the given id *)
  val kill_desc_proc : int -> unit
  (* Collects all zombie processes under the current process. *)
  val wait_on_all_proc : unit -> unit
end
