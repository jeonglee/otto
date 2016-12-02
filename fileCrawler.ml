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
  Util.debug_endline ("Opening directory " ^ dir);
  let handle = try Ok (opendir dir) with e -> Err e in
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
  Util.debug_endline ("Tried to write " ^ name);
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

module Grading = struct

  type res = {
    passed : int;
    failed : int;
    errored : int;
    other : int;
  }

  let string_of_res r =
    let soi = string_of_int in
    (soi r.passed) ^ "," ^ (soi r.failed)
    ^ "," ^ (soi r.errored) ^ "," ^ (soi r.other)

  let ran_rex = Str.regexp "Ran: +\\([0-9]+\\) +tests.+"
  let score_board = "FAILED: Cases: +\\([0-9]+\\) +Tried: +\\([0-9]+\\)+ +Errors: "
                    ^ "+\\([0-9]+\\) +Failures: +\\([0-9]+\\) "
                    ^ "+Skip: +\\([0-9]+\\) +Todo:"
                    ^ " +\\([0-9]+\\) +Timeouts: +\\([0-9]+\\).*" |> Str.regexp
  let success = Str.regexp "OK"

  let grade_from_string fc =
    let start = ((String.length fc)-1) in
    try
      Str.search_backward ran_rex fc start |> ignore;
      let tests_run = Str.matched_group 1 fc |> int_of_string in
      let all_passed =
        try
          Str.search_backward success fc start |> ignore;
          true
        with
        | Not_found -> false
      in
      if all_passed
      then {passed=tests_run;failed=0;errored=0;other=0}
      else
        begin
          Str.search_backward score_board fc start |> ignore;
          let errored = Str.matched_group 3 fc |> int_of_string in
          let failed = Str.matched_group 4 fc |> int_of_string in
          let skipped = Str.matched_group 5 fc |> int_of_string in
          let todo = Str.matched_group 6 fc |> int_of_string in
          let timeouts = Str.matched_group 7 fc |> int_of_string in
          let passed = tests_run -
                       (errored + failed + skipped + todo + timeouts) in
          {passed;failed;errored;other=(skipped+todo+timeouts)}
        end
    with
    | Not_found -> {passed=0;failed=0;errored=0;other=0}

  let grade_results dir =
    let files = files_from_dir dir in
    let open Errable.M in
    files
    >>> (fun f ->
        (* Compared to Sys.remove, this fails silently. *)
        Sys.command "rm aggregate.csv > /dev/null" |> ignore;
        f)
    >>> (List.filter (fun (f,c) -> f <> "aggregate.csv"))
    >>> (List.map (fun (f,c) -> Filename.basename f, B64.decode c))
    >>> (List.map (fun (f,c) -> f, grade_from_string c))
    >>> (List.map (fun (f,r) -> f ^ "," ^ (string_of_res r)))
    >>> (String.concat "\n")
    >>> (fun rs -> "key,passed,failed,errored,other\n" ^ rs)
    >>> (fun rs -> ("aggregate.csv",B64.encode rs))
    >>= (write_file)

end
