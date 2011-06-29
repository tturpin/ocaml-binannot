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

(** Searching int Typedtrees *)

open Typedtree

(** The type of functions that can apply either to a structure or a
    signature. *)
type 'a sfun =
  [ `structure of Typedtree.structure | `signature of Typedtree.signature]
    -> 'a

(*
(** The same, but as a record of two functions. *)
type 'a funs = {
  structure : Typedtree.structure -> 'a;
  signature : Typedtree.signature -> 'a
}
*)

(** Generic interface with functors *)

module type FindArgument = sig
  type t
  module IteratorArgument :
    functor (Action : sig val found : t -> unit end) -> IteratorArgument
end

(*
(** The functions to provide to Find. *)
module type FindArgument = sig
  type t
  val pattern : pattern -> t option
  val expression : expression -> t option
end

(** Default functions that do nothing: include this and overwrite only
    what you need. *)
module DefaultFindArgument :
  functor (T : sig type t end) -> FindArgument
  with type t = T.t
*)

(** Find and find_all. *)
module Find :
  functor (T : FindArgument) -> sig
    val find : T.t sfun
    val find_all : T.t list sfun
(*
    val find' : T.t funs
    val find_all' : T.t list funs
*)
  end


(** Finding only one sort of nodes: *)

val find_pattern : (Typedtree.pattern -> 'a option) -> 'a sfun
val find_expression : (Typedtree.expression -> 'a option) -> 'a sfun

(** Return the innermost subtree whose locations contains a given
    character number interval [a, b[. *)
val locate_innermost :
  [ `signature of signature | `structure of structure ] ->
  int * int -> [
    `pattern of pattern
  | `expression of expression
  | `structure_item of structure_item
  ]
