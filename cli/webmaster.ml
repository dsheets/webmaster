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

(* CLI *)

open Cmdliner
open Webmaster_cli

let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
  `S "AUTHORS";
  `P "David Sheets <sheets@alum.mit.edu>"; `Noblank;
  `P "Christophe Troestler <christophe.troestler@umons.ac.be>";
  `S "BUGS";
  `P "Browse and report new issues at"; `Noblank;
  `P "<https://github.com/dsheets/webmaster/issues>.";
]

let relativize_cmd =
  let doc = "relativize (X)HTML and CSS resources" in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,relativize) command maps (X)HTML and CSS files \
        to change absolute path references into relative path references.";
  ] @ help_sections
  in
  let the_path = path ~doc:"the file or directory tree to relativize" 0 in
  Term.(ret (pure Webmaster_relativize.relativize
               $ common $ root $ output $ the_path),
        info "relativize" ~doc ~sdocs:global_option_section ~man)

let resolve_cmd =
  let doc = "resolve URLs in (X)HTML and CSS resources" in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,resolve) command maps (X)HTML and CSS files \
        to resolve URI references against a provided base URI reference.";
  ] @ help_sections
  in
  let the_path = path ~doc:"the file or directory tree to resolve" 0 in
  Term.(ret (pure Webmaster_resolve.resolve
               $ common $ base $ root $ has_host $ output $ the_path),
        info "resolve" ~doc ~sdocs:global_option_section ~man)

let default_cmd =
  let doc = "maintain a collection of WWW resources" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,webmaster) consists of a number of useful functions over files \
        that are commonly found on the web like (X)HTML, CSS, and Markdown.";
  ] @ help_sections
  in
  let exec_name = Filename.basename Sys.argv.(0) in
  let no_cmd_err _ = `Error (true, "No command specified.") in
  Term.(ret (pure no_cmd_err $ common),
        info exec_name ~version:"0.1.0" ~sdocs:global_option_section
          ~doc ~man)

;;

match Term.eval_choice default_cmd [
  relativize_cmd;
  resolve_cmd;
] with
| `Ok () | `Version | `Help -> exit 0
| `Error _ -> exit 1
