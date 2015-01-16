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
open Webmaster_file

type common = Webmaster_cli.common = {
  force : bool;
}

let relativize_file ~root in_file out_file = Urlfun.(
  let to_base = match revert_path root in_file with
    | [] -> ""
    | ss -> String.concat "" (List.map (fun s -> s ^ "/") ss)
  in
  let make_relative = unlift_with_warning (make_relative ~to_base) in
  try
    let warns = warning_map_file_hrefs make_relative in_file out_file in
    List.iter (fun w ->
      eprintf "%s: %s\n" in_file (string_of_warning w);
    ) (List.rev warns);
    warns <> []
  with (Unknown_file_type _) ->
    if in_file <> out_file then copy in_file out_file; false
)

let relativize ({ force }) root (_output_links,output) (_path_links,path) =
  let cmd = "relativize" in
  let process _file = relativize_file ~root in
  output_of_input ~force ~cmd process path output
