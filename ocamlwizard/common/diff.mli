(**************************************************************************)
(*                                                                        *)
(*  Ocamlwizard-Binannot                                                  *)
(*  Tiphaine Turpin                                                       *)
(*  Copyright 2011 INRIA Saclay - Ile-de-France                           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Reading modified text files. *)

(** This module calls the diff -f command. *)

(** A chunk of a modified file is either an unmodified portion or
    modified one, which has an old and a new versions. Chunks may
    contain newline characters. *)
type chunk =
  | Same of string
  | Changed of string * string

(** Read a modified file as a list of chunks, given the old and the
    new versions of the file. Absent files are treated as empty by
    default. *)
val read_modified_file : ?empty_absent:bool -> string -> string -> chunk list

(* Cut a modified file, only keeping the count first characters of the
   new version. *)
val cut_new : int -> chunk list -> chunk list

(* Print a modified file in a somewhat readable form. *)
val print_modified : out_channel -> chunk list -> unit
