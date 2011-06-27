(**************************************************************************)
(*                                                                        *)
(*  Ocamlwizard                                                           *)
(*  David Baudet and Mohamed Iguernelala                                  *)
(*  Copyright 2008 INRIA Saclay - Ile-de-France                           *)
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

exception Exit_typing

type refactor_option =
  | Rename of (int * int) * string * string * string
  | Depend | Qualif
type command =
    Nothing
  | Completion
  | Compile
  | Locate
  | Refactor of refactor_option

val match_annot : bool ref
val match_depth : int ref
val debug : bool ref
val ocaml_compiler : string
val printer : Interface.tprinter ref
val _parser : Interface.tparser ref
val kind : string ref
val compile_only : bool ref
val dot_test : bool ref
val include_dirs : string list ref
val fic_source : string ref
val pos : string ref
val absolute_filenames : bool ref
val qualid : Longident.t ref
val loc : (int * int) ref
val expand_loc : (int * int) ref
val root_dir : string ref
val search_dirs : string -> string list
val i_dirs : unit -> string list
val command : command ref
val compile_index : int ref
val ignore_auto_save : bool ref
val options : (string * Arg.spec * string) list
val usage : string
val anonymous : string -> unit
