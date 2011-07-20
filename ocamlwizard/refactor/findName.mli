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

(** Collect all the longident occurrences appearing in a file, with
    their location, sort, and lookup environment. *)
val get_lids :
  Location.t Longident.LongidentTbl.t -> Env.lid2env -> (Longident.t -> bool) ->
  TypedtreeOps.typedtree ->
  (Location.t * Longident.t * (Env.t * Env.path_sort)) list
