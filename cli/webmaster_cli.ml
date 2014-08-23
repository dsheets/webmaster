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

open Cmdliner

type 'a ret = [
| `Error of bool * string
| `Help of [ `Groff | `Pager | `Plain ] * string option
| `Ok of 'a
]

type resource = [
| `File of string
| `Dir of string
| `Missing of string
]
type path = [ resource | `Link of string * path ]

type common = {
  force : bool;
}

let global_option_section = "COMMON OPTIONS"

let rec resource_of_path : path -> string list * resource = function
  | (`Dir _ | `File _ | `Missing _ ) as r -> [],r
  | `Link (link_path, target) ->
    let to_end, v = resource_of_path target in
    link_path::to_end, v

let map f = Term.(app (pure f))

let ret_map f t = Term.ret (map f t)

let bind_ok f = function
  | `Ok v -> f v
  | (`Error (_,_) | `Help _) as other -> other

let split_path = function "" -> [] | p -> Neturl.split_path p

let root = Arg.(map split_path (value (
  let docv = "ROOT" in
  let doc = "the directory to treat as root" in
  opt dir ""
  & info ~docv ~doc ["root"]
)))

let rec to_path ~must_exist path = Unix.(
  try match (stat path).st_kind with
  | S_DIR -> `Ok (`Dir path)
  | S_REG -> `Ok (`File path)
  | S_LNK -> bind_ok
    (fun chain -> `Ok (`Link (path, chain)))
    (to_path ~must_exist (Unix.readlink path))
  | S_CHR | S_BLK | S_FIFO | S_SOCK ->
    `Error (false, "unsupported file type")
  with Unix_error (e,_,_) ->
    if must_exist
    then `Error (false, path^": "^(error_message e))
    else `Ok (`Missing path)
)

let path_arg ~doc ?(must_exist=true) arg names =
  Arg.(map resource_of_path (ret_map (to_path ~must_exist) (required (
    let docv = "PATH" in
    arg (some (if must_exist then file else string)) None
    & info ~docv ~doc names
  ))))

let path ~doc k = path_arg ~doc Arg.(pos k) []

let output = path_arg ~doc:"the output path" ~must_exist:false Arg.opt ["o"]

let base = Arg.(map Uri.of_string (value (
  let docv = "BASE" in
  let doc = "the URI reference to use as base" in
  opt string ""
  & info ~docv ~doc ["base"]
)))

let has_host = Arg.(value (
  let docv = "HAS_HOST" in
  let doc = "only process URLs with the host component" in
  flag & info ~docv ~doc ["has-host"]
))

let force = Arg.(value (
  let docv = "FORCE" in
  let doc = "force the execution of the command" in
  flag & info ~docs:global_option_section ~docv ~doc ["f"]
))

let create_common force = {
  force;
}

let common = Term.(pure create_common $ force)
