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

type signature = Resolve.ident_context * Types.signature

module ConstraintSet : Set.S
  with type elt = signature * signature

module IncludeSet : Set.S
  with type elt = signature * Ident.t list

(** Collect binding constraints for a set of files, to indicate which
    idents must be renamed simultaneously. *)
val constraints_all_files :
  Env.t -> Env.path_sort -> string ->
  (Resolve.source_file *
     (TypedtreeOps.typedtree * 'a * 'b * 'c * Types.signature))
    list -> ConstraintSet.t * IncludeSet.t
(* means id is bound to sg.(name id), unless we were wrong about the sort. *)

(** Return the minimal set of idents which may be renamed and contains
    a given id, as well as the "implicit" bindings of signature
    elements to those idents. *)
val propagate :
  Env.path_sort -> Resolve.global_ident ->
  (Resolve.source_file *
     (TypedtreeOps.typedtree * 'a * 'b * 'c * Types.signature))
    list -> ConstraintSet.t -> IncludeSet.t ->
  Resolve.global_ident list
  * ([ `certain | `maybe ] * signature * Resolve.global_ident) list

(** Check the implicit bindings for capture. *)
val check_renamed_implicit_references :
  Env.path_sort -> Resolve.global_ident list -> string ->
  ([ `certain | `maybe ] * signature * Resolve.global_ident) list -> unit

val check_other_implicit_references :
  Env.path_sort -> Resolve.global_ident list -> string ->
  ConstraintSet.t -> IncludeSet.t -> unit
