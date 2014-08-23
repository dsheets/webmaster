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

let resolve_file ~base ~root ~has_host in_file out_file = Urlfun.(
  let (_, from_root) = remove_common_prefix root (Neturl.split_path in_file) in
  let from_root = String.concat "/" from_root in
  let base = Uri.(resolve "http" base (of_string from_root)) in
  let resolve = unlift_with_warning (fun url ->
    match has_host, Uri.host url with
    | true, None -> url
    | _,    _    -> Uri.resolve "http" base url
  ) in
  try
    let warns = warning_map_file_hrefs resolve in_file out_file in
    List.iter (fun w ->
      eprintf "%s: %s\n" in_file (string_of_warning w);
    ) (List.rev warns);
    warns <> []
  with (Unknown_file_type _) ->
    if in_file <> out_file then copy in_file out_file; false
)

let resolve ({ force }) base root has_host (_olinks,output) (_plinks,path) =
  let cmd = "resolve" in
  let process = resolve_file ~base ~root ~has_host in
  output_of_input ~force ~cmd process path output
