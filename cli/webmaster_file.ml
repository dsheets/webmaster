(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Printf

let (/) = Filename.concat

let page_size = 4096

let copy in_file out_file =
  let ic = open_in_bin  in_file in
  let oc = open_out_bin out_file in
  let buf = Bytes.create page_size in
  let rec copy_more () =
    match input ic buf 0 page_size with
    | 0 -> ()
    | len -> output oc buf 0 len; copy_more ()
  in
  copy_more ();
  close_in ic;
  close_out oc

let rec read_files acc dh =
  match
    try Some (Unix.readdir dh)
    with End_of_file -> None
  with Some file -> read_files (file::acc) dh | None -> acc

let rec all_files base acc dh =
  let files = read_files [] dh in
  List.fold_left (fun acc -> function
  | "." | ".." -> acc
  | dirent ->
    let file = Filename.concat base dirent in
    try
      let dh = Unix.opendir file in
      let acc = all_files file acc dh in
      Unix.closedir dh;
      acc
    with
    | Unix.Unix_error (Unix.ENOTDIR, _, _) -> file::acc
    | Unix.Unix_error (Unix.ENOENT,  _, _) -> (* dangling symlink or race *)
      acc
  ) acc files

let in_dir path f =
  let cwd = Unix.getcwd () in
  Unix.chdir path;
  try let r = f () in Unix.chdir cwd; r
  with e -> Unix.chdir cwd; raise e

let rec ensure_directory_exists ~perm path =
  try Unix.access path []
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    let dir = Filename.dirname path in
    ensure_directory_exists ~perm dir;
    Unix.(mkdir path perm)

let rec ensure_model_directory_exists ~model_path path =
  try Unix.access path []
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    let model_dir = Filename.dirname model_path in
    let dir = Filename.dirname path in
    ensure_model_directory_exists ~model_path:model_dir dir;
    Unix.(mkdir path ((stat model_path).st_perm))

let transforms ~force path output =
  let dh = Unix.opendir path in
  let files = in_dir path (fun () -> all_files "" [] dh) in
  let () = Unix.closedir dh in
  let transforms  = List.rev_map (fun file ->
    file, path / file, output / file
  ) files in
  if force then Some transforms
  else
    let error = List.fold_left (fun error (_,_,out) ->
      try Unix.access out [];
          eprintf "destination %s exists\n" out;
          true
      with Unix.Unix_error (Unix.ENOENT, _, _) -> error
    ) false transforms in
    if error then None else Some transforms

let check ~cmd warnings =
  if warnings
  then eprintf "%s completed successfully with some warnings\n%!" cmd

let map_directory ~force ~cmd process path output =
  match transforms ~force path output with
  | Some transforms ->
    let warnings = List.fold_left (fun warnings (file, in_file, out_file) ->
      let in_dir = Filename.dirname in_file in
      let out_dir = Filename.dirname out_file in
      ensure_model_directory_exists ~model_path:in_dir out_dir;
      process file in_file out_file || warnings
    ) false transforms in
    `Ok (check ~cmd warnings)
  | None -> `Error (false, "some files would be overwritten")

let output_type path output = match path, output with
  | `Missing _, _ -> None
  | `File path, (`Missing output | `File output) -> Some (`File output)
  | `File path, `Dir output ->
    let file = Filename.basename path in
    Some (`File (Filename.concat output file))
  | `Dir  path, `File output -> None
  | `Dir  path, (`Dir output | `Missing output) -> Some (`Dir output)

let output_of_input ~force ~cmd process path output = match path, output with
  | `Missing _, _ -> failwith "impossible :-("
  | `File path, `Missing output ->
    let file = Filename.basename path in
    `Ok (check ~cmd (process file path output))
  | `File path, `File output when force ->
    let file = Filename.basename path in
    `Ok (check ~cmd (process file path output))
  | `File path, `File output ->
    `Error (false, "destination "^output^" exists")
  | `File path, `Dir output ->
    let file = Filename.basename path in
    `Ok (check ~cmd (process file path (Filename.concat output file)))
  | `Dir  path, `File output ->
    `Error (false, "cannot process directory into file")
  | `Dir  path, `Dir output -> map_directory ~force ~cmd process path output
  | `Dir  path, `Missing output ->
    let perm = Unix.((stat path).st_perm) in
    Unix.mkdir output perm;
    map_directory ~force ~cmd process path output
