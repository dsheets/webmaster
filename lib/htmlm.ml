(*
 * Copyright (c) 2013-2014 Christophe Troestler
 *                        <christophe.troestler@umons.ac.be>
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

module Nethtml = struct
  let relaxed_html40_dtd =
    (* Allow <font> inside <pre> because blogspot uses it! :-( *)
    let constr = `Sub_exclusions([ "img"; "object"; "applet"; "big"; "small";
                                   "sub"; "sup"; "basefont"],
                                 `Inline) in
    let dtd = Nethtml.relaxed_html40_dtd in
    ("pre", (`Block, constr)) :: List.remove_assoc "pre" dtd
end

module Xhtmlm = struct
  type tree = [ `Data of string | `El of Xmlm.tag * 'a list ] as 'a

  let html_void_elements = [
    "img";
    "input";
    "link";
    "meta";
    "br";
    "hr";
    "source";
    "wbr";
    "param";
    "embed";
    "base";
    "area";
    "col";
    "track";
    "keygen";
  ]

  let rec generate_signals signals = function
    | `Data s -> (`Data s)::signals
    | `El (tag, children) ->
      let signals = (`El_start tag)::signals in
      let signals = List.fold_left generate_signals signals children in
      match signals with
      | `El_start ((_, tag),_) :: _ when List.mem tag html_void_elements ->
        `El_end::signals
      | [] | (`Data _ | `Dtd _ | `El_end)::_ -> `El_end::signals
      | `El_start _ :: _ -> `El_end::(`Data "")::signals

  let output_doc_tree output tree =
    Xmlm.output output (`Dtd None);
    let signals = generate_signals [] tree in
    List.(iter (Xmlm.output output) (rev signals))
end
