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

type signature = Resolve.ident_context * Types.signature

module ConstraintSet : Set.S
  with type elt = signature * signature

module IncludeSet : Set.S
  with type elt = signature * Ident.t list

(*
(** Collect the set of signature inclusion constraints and include
  statements for a structure. *)
  val collect_signature_inclusions :
  (ConstraintSet.t * IncludeSet.t) TypedtreeOps.sfun
*)

(** Return the minimal set of idents which may be renamed and contains
    a given id, as well as the "implicit" bindings of signature
    elements to those idents. *)
val constraints_all_files :
  Env.t -> Env.path_sort -> Ident.t ->
  (Resolve.source_file *
     (TypedtreeOps.typedtree * 'a * 'b * 'c * Types.signature))
    list -> ConstraintSet.t * IncludeSet.t
(* means id is bound to sg.(name id), unless we were wrong about the sort. *)

val propagate :
  Resolve.source_file -> Env.path_sort -> Ident.t ->
  (Resolve.source_file *
     (TypedtreeOps.typedtree * 'a * 'b * 'c * Types.signature))
    list -> ConstraintSet.t -> IncludeSet.t ->
  Resolve.global_ident list
  * ([ `certain | `maybe ] * Types.signature * Resolve.global_ident) list

val check_renamed_implicit_references :
  Env.path_sort -> Resolve.global_ident list -> string ->
  ([ `certain | `maybe ] * Types.signature * Resolve.global_ident) list -> unit

val check_other_implicit_references :
  Env.path_sort -> Resolve.global_ident list -> string ->
  ConstraintSet.t -> IncludeSet.t -> unit
