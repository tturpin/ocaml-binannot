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

(** Identifying idents which cannot be renamed independently of each
    other (due to signature matching in particular). *)

(* should not be here *)
val sig_item_id : Types.signature_item -> Ident.t

module ConstraintSet : Set.S
  with type elt = Types.signature * Types.signature

module IncludeSet : Set.S
  with type elt = Types.signature * Ident.t list

(** Collect the set of signature inclusion constraints and include
    statements for a structure. *)
val collect_signature_inclusions :
  Typedtree.structure -> ConstraintSet.t * IncludeSet.t

(** Return the minimal set of idents which may be renamed and contains
    a given id, as well as the "implicit" bindings of signature
    elements to those idents. *)
val propagate_renamings :
  Resolve.specifics -> Ident.t -> ConstraintSet.t -> IncludeSet.t ->
  Ident.t list
  * ([ `certain | `maybe ] * Types.signature * Ident.t) list
    (* means id is bound to sg.(name id), unless we were wrong about the sort. *)

val check_renamed_implicit_references :
  Resolve.specifics -> Ident.t list -> string ->
  ([ `certain | `maybe ] * Types.signature * Ident.t) list -> unit

val check_other_implicit_references :
  Resolve.specifics -> Ident.t list -> string ->
  ConstraintSet.t -> IncludeSet.t -> unit
