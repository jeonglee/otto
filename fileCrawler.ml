open Unix
open Str
open Errable

type file = string * string

let file_to_string f =
  let in_channel = open_in f in
  let len = in_channel_length in_channel in
  let s = Bytes.create len in
  really_input in_channel s 0 len;
  close_in in_channel;
  Bytes.to_string s

let slash = Filename.dir_sep

let rec construct_file_list handle file dir =
  let name      = dir ^ slash ^ file in
  let content   = file_to_string file in
  let next_file = try Some (readdir handle) with End_of_file -> None in
  (name,content)::match next_file with
                  | Some f -> construct_file_list handle f dir
                  | None   -> closedir handle; []

let files_from_dir dir =
  let handle = try Ok (opendir dir) with Unix_error(_,_,_) -> Err Not_found in
               match handle with
               | Err e -> Err e
               | Ok h ->
                   let file1  = try Ok (readdir h) with End_of_file -> Err Not_found in
               match file1 with
               | Err e -> closedir h; Err e
               | Ok f ->
                   Ok (construct_file_list (?!handle) (?!file1) dir)

let write_file ?dir:(d=".") (name,contents) =
  let path = d ^ slash ^ name  in
  let out_channel = open_out path in
  let _ = output_string out_channel contents in
  try (Ok (close_out out_channel)) with (Sys_error err) -> Err (Sys_error err)
