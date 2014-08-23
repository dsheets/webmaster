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

open Printf

type ('a,'d) fold_map = 'a -> 'd -> 'a * 'd

type 'a xmlm_tree = ('a Xmlm.frag) as 'a

(* Higher order functions over all URL references in web files. *)

exception Unknown_file_type of string

type warning =
| Xml_parse_error of Xmlm.pos * string
| Whitespace_url of string
| Unnormal_url of string * string

let html_doctype = "<!DOCTYPE html>\n"
let xhtmlns = "http://www.w3.org/1999/xhtml"

let is_html ns = ns = "" || ns = xhtmlns

let string_of_warning = function
  | Xml_parse_error ((line,char), msg) ->
    sprintf "XML parse error at line %d character %d: %s\n  using HTML parser"
      line char msg
  | Whitespace_url url_s ->
    sprintf "URL whitespace error for <%s>" url_s
  | Unnormal_url (orig, norm) ->
    sprintf "URL <%s> interpreted as <%s>" orig norm

let fold_map_url_in_attrs f a rewrites attrs =
  let a, attrs = List.fold_left (fun (a,attrs) ((attr,v) as avp) ->
    if List.mem attr rewrites
    then let a, v = f a v in a, (attr,v)::attrs
    else a, avp::attrs
  ) (a,[]) attrs in
  a, List.rev attrs

let fold_map_html_tag_hrefs f a = function
  | (("a" | "link") as e, attrs) ->
    let a, attrs = fold_map_url_in_attrs f a ["href"] attrs in
    a, (e, attrs)
  | (("img" | "script") as e, attrs) ->
    let a, attrs = fold_map_url_in_attrs f a ["src"] attrs in
    a, (e, attrs)
  | tag -> a, tag

let rec fold_map_html_hrefs f a html =
  let a, rhs = List.fold_left (fun (a,hs) h ->
    let a, h = fold_map_html_frag_hrefs f a h in
    (a, h::hs)
  ) (a,[]) html in
  a, List.rev rhs
and fold_map_html_frag_hrefs f a = Nethtml.(function
  | Data _ as d -> a, d
  | Element (tag, attrs, sub) ->
    let a, (tag, attrs) = fold_map_html_tag_hrefs f a (tag, attrs) in
    let a, sub = fold_map_html_hrefs f a sub in
    a, Element (tag, attrs, sub)
)

let fold_map_html_file_hrefs f a in_file out_file =
  let fh = open_in in_file in
  let html = Nethtml.parse_document
    (Lexing.from_channel fh)
    ~dtd:Htmlm.Nethtml.relaxed_html40_dtd
  in
  close_in fh;
  let a, html = fold_map_html_hrefs f a html in
  let fh = open_out out_file in
  let out = new Netchannels.output_channel fh in
  (* Nethtml does not represent <!DOCTYPE html>, add it. *)
  out#output_string html_doctype;
  Nethtml.write out html;
  out#output_string "\n";
  out#close_out ();
  a

let fold_map_xhtml_tag_hrefs f a = function
  | ((ns,(("a" | "link") as e)),attrs) when is_html ns ->
    let a, attrs = fold_map_url_in_attrs f a ["","href"] attrs in
    a, ((ns,e),attrs)
  | ((ns,(("img" | "script") as e)),attrs) when is_html ns ->
    let a, attrs = fold_map_url_in_attrs f a ["","src"] attrs in
    a, ((ns,e),attrs)
  | tag -> a, tag

let rec fold_map_xhtml_hrefs f a xml =
  let a, rxs = List.fold_left (fun (a,xs) x ->
    let a, x = fold_map_xhtml_frag_hrefs f a x in
    (a, x::xs)
  ) (a, []) xml in
  a, List.rev rxs
and fold_map_xhtml_frag_hrefs f a = function
  | `Data _ as d -> a, d
  | `El (tag, sub) ->
    let a, tag = fold_map_xhtml_tag_hrefs f a tag in
    let a, sub = fold_map_xhtml_hrefs f a sub in
    a, `El (tag, sub)

let fold_map_xhtml_file_hrefs f a in_file out_file =
  let fh = open_in in_file in
  let _, tree = Xmlm.input_doc_tree
    ~el:(fun tag children -> `El (tag, children))
    ~data:(fun s -> `Data s)
    (Xmlm.make_input (`Channel fh))
  in
  close_in fh;
  let a, tree = fold_map_xhtml_hrefs f a [tree] in
  let fh = open_out out_file in
  output_string fh html_doctype;
  Htmlm.Xhtmlm.output_doc_tree
    (Xmlm.make_output ~decl:false (`Channel fh))
    (List.hd tree);
  output_string fh "\n";
  close_out fh;
  a

let fold_map_css_file_hrefs f a in_file out_file =
  let fh = open_in in_file in
  let ch = new Netchannels.input_channel fh in
  let css = Netchannels.string_of_in_obj_channel ch in
  close_in fh;
  let url_re = Str.regexp
    "url(\"\\([^\"]*\\)\")|url('\\([^']*\\)')|url(\\([^()]*\\))"
  in
  let acc_ref = ref a in
  let css = Str.global_substitute url_re (fun css ->
    let a, url = f !acc_ref (Str.matched_group 1 css) in
    acc_ref := a;
    Printf.sprintf "url(%s)" url
  ) css in
  let out = new Netchannels.output_channel (open_out out_file) in
  out#output_string css;
  out#close_out();
  !acc_ref

let warning_map_file_hrefs f in_file out_file =
  if   Filename.check_suffix in_file ".html"
    || Filename.check_suffix in_file ".xhtml"
  then
    try
      fold_map_xhtml_file_hrefs f [] in_file out_file
    with Xmlm.Error (pos, error) ->
      fold_map_html_file_hrefs f
        [Xml_parse_error (pos, Xmlm.error_message error)] in_file out_file
  else if Filename.check_suffix in_file ".css"
  then fold_map_css_file_hrefs f [] in_file out_file
  else raise (Unknown_file_type in_file)

let unlift_with_warning uri_map = fun warns url_s ->
  let turl_s = String.trim url_s in
  let warns = if url_s = turl_s then warns
    else (Whitespace_url url_s)::warns
  in

  let uri = Uri.of_string turl_s in
  let uri_s = Uri.to_string uri in
  let warns = if turl_s = uri_s then warns
    else (Unnormal_url (turl_s, uri_s))::warns
  in

  warns, Uri.to_string (uri_map uri)

(* Path functions TODO: move to Uri *)

let rec remove_common_prefix p1 p2 =
  match p1, p2 with
  | d1 :: p1, d2 :: p2 when d1 = d2 -> remove_common_prefix p1 p2
  | _ -> p1, p2

let revert_path =
  let rec revert filename =
    match filename with
    | [] | [ _ ]-> [] (* the last component is a filename or "" *)
    | _ :: tl -> ".." :: revert tl in
  fun to_base filename ->
    let filename = Neturl.norm_path (Neturl.split_path filename) in
    let to_base, filename = remove_common_prefix to_base filename in
    revert filename @ to_base

(* Concrete functions *)

(* Convert absolute links to relative ones. *)
let make_relative ~to_base url = match Uri.host url with
  | Some _ -> url
  | None -> match Uri.path url with
    | "" -> url
    | path ->
      if path.[0] = '/'
      then
        let path = String.sub path 1 (String.length path - 1) in
        Uri.with_path url (to_base ^ path)
      else url
