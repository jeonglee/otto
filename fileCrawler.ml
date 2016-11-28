open Unix
open Str
open Errable
open Errable.M

type file = string * string

(* [file_to_string f] returns the contents of the file [f] as a string
 * requires: [f] is a valid file path *)
let file_to_string f =
  let in_channel = open_in f in
  let len = in_channel_length in_channel in
  let s = Bytes.create len in
  really_input in_channel s 0 len;
  close_in in_channel;
  s |> Bytes.to_string |> B64.encode

(* see Filename.dir_sep *)
let slash = Filename.dir_sep

(* let rec construct_file_list handle file dir =
  let name      = dir ^ slash ^ file in
  let content   = file_to_string file in
  let next_file = try Some (readdir handle) with End_of_file -> None in
  (name,content)::match next_file with
                  | Some f -> construct_file_list handle f dir
                  | None   -> closedir handle; [] *)

(* let files_from_dir dir =
  let handle = try Ok (opendir dir) with Unix_error(_,_,_) -> Err Not_found in
               match handle with
               | Err e -> Err e
               | Ok h ->
                   let file1  = try Ok (readdir h) with End_of_file -> Err Not_found in
               match file1 with
               | Err e -> closedir h; Err e
               | Ok f ->
                   Ok (construct_file_list (?!handle) (?!file1) dir) *)

(* [construct_file_list dir lst h] constructs a list of files in [dir], ignoring
 * anything within the directory that raises a Sys_error when we try to
 * Pervasives.open_in it (ex. a sub-directory like "." or "..") *)
let rec construct_file_list dir lst h =
  let file = try Some (readdir h) with End_of_file -> closedir h; None in
  match file with
  | None -> lst
  | Some f -> let name = dir ^ slash ^ f in
              let c = try Some (file_to_string name) with Sys_error _ -> None in
              match c with
              | None -> construct_file_list dir lst h
              | Some content -> construct_file_list dir ((name,content)::lst) h

(* [files_from_dir dir] attempts to open a handle on the directory [dir] and
 * uses it to construct a file list using construct_file_list *)
let files_from_dir dir =
  let handle = try Ok (opendir dir) with Unix_error(_,_,_) -> Err Not_found in
  handle >>> (construct_file_list dir [])

(* [write_file ?dir (name,contents)] writes to the specified file in the given
 * directory [dir]. If the file specified does not exist, it is created. If the
 * directory specified does not exist, it is created.
 * requires:
 * [contents] is a string encoded in base64*)
let write_file ?dir:(d=".") (name,contents) =
  let path = d ^ slash ^ name  in
  let dirname = Filename.dirname path in
  let _ = Sys.command ("mkdir -p " ^ dirname) in
  let out_channel = open_out path in
  output_string out_channel (B64.decode contents);
  try (Ok (close_out out_channel)) with (Sys_error err) -> Err (Sys_error err)

let subdirectories path =
  try
    let files = Sys.readdir path |> Array.to_list in
    List.filter (fun f ->
        path ^ Filename.dir_sep ^ f
        |> Sys.is_directory) files
    |> return
  with
  | e -> Err e
