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

val contains : Location.t -> int * int -> bool

(** Return the innermost subtree whose locations contains a given
    character number interval [a, b[.

    Warning: most node kinds are missing ! *)
val locate : [`outermost | `innermost] -> int * int ->
  TypedtreeOps.typedtree -> TypedtreeOps.node

(** Simliar to locate, but we return the first node along a path (in
    the sense of the given priority) for which the parameter function
    returns some result. *)
val locate_map : [`outermost | `innermost] -> (TypedtreeOps.node -> 'a option) ->
  int * int -> TypedtreeOps.typedtree -> 'a

(*
(** The same as locate, but with exceptions *)
val locate_map_exn : [`outermost | `innermost] -> (node -> 'a) ->
  int * int -> 'a sfun
*)

(** Return the location of a longident. Objects are not implemented. *)
val longident :
  Location.string_table -> int * int ->
  [ `signature of Typedtree.signature | `structure of Typedtree.structure ] ->
  Env.path_sort * Ident.t

(** Return the location of the definition of an ident. *)
val ident_def :
(*
  [ `signature of Typedtree.signature | `structure of Typedtree.structure ] ->
*)
  Location.string_table -> Ident.t -> Location.t
