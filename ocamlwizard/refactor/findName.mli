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

open Typedtree

(*
val get_occurrences :
  structure -> (Location.t * (Env.t * [ `exp_ident | `mod_ident ])) list
*)

val get_lids :
  string -> Typedtree.structure ->
  (Location.t * Longident.t * (Env.t * Resolve.specifics)) list

val locate_renamed_id :
  [ `signature of Typedtree.signature | `structure of Typedtree.structure ] ->
  int * int ->
  Resolve.specifics * Ident.t
