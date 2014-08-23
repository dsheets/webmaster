(*
 * Copyright (c) 2013-2014 Christophe Troestler
 *                        <christophe.troestler@umons.ac.be>
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

(** {1 Folding Map Functions for URLs in Files} *)

type ('a,'d) fold_map = 'a -> 'd -> 'a * 'd

type 'a xmlm_tree = ('a Xmlm.frag) as 'a

type warning =
| Xml_parse_error of Xmlm.pos * string
| Whitespace_url of string
| Unnormal_url of string * string

exception Unknown_file_type of string

val string_of_warning : warning -> string

val fold_map_html_hrefs :
  ('a,string) fold_map -> 'a -> Nethtml.document list
  -> 'a * Nethtml.document list

val fold_map_html_frag_hrefs :
  ('a, string) fold_map -> 'a -> Nethtml.document -> 'a * Nethtml.document

val fold_map_html_file_hrefs :
  ('a, string) fold_map -> 'a -> string -> string -> 'a

val fold_map_xhtml_frag_hrefs :
  ('a, string) fold_map -> 'a -> 'c xmlm_tree -> 'a * 'c xmlm_tree

val fold_map_xhtml_file_hrefs :
  ('a, string) fold_map -> 'a -> string -> string -> 'a

val fold_map_css_file_hrefs :
  ('a, string) fold_map -> 'a -> string -> string -> 'a

(** [warning_map_file_hrefs fn in_file out_file] reads [in_file] for
    URLs, applies [fn] to them as strings, and writes the results in
    place to [out_file]. The whole of [in_file] is read into memory so
    [in_file] and [out_file] can be the same name. Raises
    [Unknown_file_type filename] when no map function exists for the
    given file type. *)
val warning_map_file_hrefs :
  (warning list, string) fold_map -> string -> string -> warning list

(** {1 URL Parsing with Warnings} *)
val unlift_with_warning : (Uri.t -> Uri.t) -> (warning list,string) fold_map

(** {1 Auxiliary Path Functions} *)

val remove_common_prefix :
  string list -> string list -> (string list * string list)

val revert_path : string list -> string -> string list

val make_relative : to_base:string -> Uri.t -> Uri.t
