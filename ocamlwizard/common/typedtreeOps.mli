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

(** The common type for all typedtree nodes. *)
type node = [
  `structure of structure
| `value_description of value_description
| `type_declaration of type_declaration
| `exception_declaration of exception_declaration
| `pattern of pattern
| `expression of expression
| `package_type of package_type
| `signature of signature
| `signature_item of signature_item
| `modtype_declaration of modtype_declaration
| `module_type of module_type
| `module_expr of module_expr
| `with_constraint of with_constraint
| `class_expr of class_expr
| `class_signature of class_signature
| `class_description of class_description
| `class_type_declaration of class_type_declaration
| `class_infos of unit class_infos
| `class_type of class_type
| `class_type_field of class_type_field
| `core_type of core_type
| `core_field_type of core_field_type
| `class_structure of class_structure
| `class_field of class_field
| `structure_item of structure_item
| `binding of pattern * expression
| `bindings of Asttypes.rec_flag
]

(** Return the constructor name, as a string. *)
val node_kind : node -> string

(** Traverse a typedtree, calling the provided enter and leave
    functions just before and just after each node, respectively. *)
val iterator : enter:(node -> unit) -> leave:(node -> unit) -> unit sfun

(** Find the innermost node for which some condition holds. *)
val find_map : [`outermost | `innermost] -> (node -> 'a option) -> 'a sfun

(** Find all nodes satisfying some condition. *)
val find_all_map : (node -> 'a option) -> 'a list sfun

(** Return the innermost subtree whose locations contains a given
    character number interval [a, b[.

    Warning: most node kinds are missing ! *)
val locate : [`outermost | `innermost] -> int * int -> node sfun


(** Finding only one sort of nodes: *)

val find_pattern :
  [`outermost | `innermost] -> (Typedtree.pattern -> 'a option) -> 'a sfun
val find_expression :
  [`outermost | `innermost] -> (Typedtree.expression -> 'a option) -> 'a sfun

module NodeTbl : Hashtbl.S with type key = node

type father_table = node NodeTbl.t

val reverse : father_table sfun
