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

(** Finding names (definitions and references) in a typedtree. *)

val get_occurrences :
  Typedtree.structure -> (Location.t * (Env.t * Resolve.specifics)) list

val locate_name :
  [ `signature of Typedtree.signature | `structure of Typedtree.structure ] ->
  int * int ->
  [ `pattern of Typedtree.pattern
  | `structure_item of Typedtree.structure_item ]
