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

val ( / ) : string -> string -> string
val page_size : int

val copy : string -> string -> unit

val read_files : string list -> Unix.dir_handle -> string list

val in_dir : string -> (unit -> 'a) -> 'a

val ensure_directory_exists : perm:int -> string -> unit

val ensure_model_directory_exists : model_path:string -> string -> unit

val transforms :
  force:bool -> string -> string -> (string * string * string) list option

val check : cmd:string -> bool -> unit

open Webmaster_cli

val output_type :
  resource -> resource -> [ `File of string | `Dir of string ] option

val output_of_input :
  force:bool -> cmd:string -> (string -> string -> string -> bool)
  -> resource -> resource -> unit ret
