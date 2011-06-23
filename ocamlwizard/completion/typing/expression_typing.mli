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

(** This module provides main operations on annots files such as parsing
    an existing annot file or looking for a type corresponding with 
    a given range *)

(** Get the type of the expression with given location in an typedtree. *)
val locate_expression : Typedtree.structure -> Location.t -> Typedtree.expression
(*
val type_of_pat : Typedtree.structure -> Location.t -> Typedtree.pattern
*)

type expansion_place =
  | Pat of Typedtree.pattern
  (* expand this wildcard or variable *)
  | Args of Typedtree.pattern * Path.t * Types.constructor_description
  (* expand the arguments of this constructor pattern *)

val locate_expansion_place : Typedtree.structure -> int * int -> expansion_place

val expansion_type : expansion_place -> Env.t * Types.type_desc
