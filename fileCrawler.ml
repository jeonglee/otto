exception Not_found
open Unix
open Str

type file = string * string

let slash = Filename.dir_sep

let rec construct_file_list handle file dir =
  let next_file = try (Some (readdir handle)) with End_of_file -> None in
  [dir ^ slash ^ file] @
  match next_file with
  | Some f -> (construct_file_list handle f dir)
  | None   -> []

let files_from_dir dir =
  let handle =
  (try
    (Unix.opendir dir)
  with
    | Unix.Unix_error(_,_,_) -> raise Not_found) in
  let file_list =
  (try
    (construct_file_list handle (Unix.readdir handle) dir)
  with
    | End_of_file -> Unix.closedir handle; []) in "yes"


let write_file (name,contents) =
  failwith "Unimplemented"
