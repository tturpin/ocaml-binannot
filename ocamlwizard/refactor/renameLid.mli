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

(** The core of renaming. *)

(** Rename all occurrences of any of a list of idents with the same
    name in a longident, given the environmemt in which this longident
    makes sense, and make sure that the new longident still reffers to
    the same thing (i.e., is not subject to masking by other
    equally-named elements). *)
val rename_in_lid :
  Env.path_sort -> Resolve.global_ident list -> string ->
  Env.t -> Env.path_sort -> Resolve.source_file -> Longident.t -> Longident.t option

(** Check that no existing occurrence of the new name appearing in a
    longident would be captured by one of the renamed idents if we
    applied the given renaming. *)
val check_lid :
  Env.path_sort -> Resolve.global_ident list -> string ->
  Env.t -> Env.path_sort -> Resolve.source_file -> Longident.t -> unit
