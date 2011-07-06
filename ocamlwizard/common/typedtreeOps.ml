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

open Typedtree
open Util

type 'a sfun =
  [ `structure of Typedtree.structure | `signature of Typedtree.signature]
    -> 'a

type 'a funs = {
  structure : structure -> 'a;
  signature : signature -> 'a
}

module MakeIterator
  (Arg : IteratorArgument) = struct

    include MakeIterator (Arg)

    let process = function
      | `structure s -> iter_structure s
      | `signature s -> iter_signature s

    let process' = {
      structure = iter_structure;
      signature = iter_signature
    }

end

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

let node_kind = function
  | `structure _ -> "structure"
  | `value_description _ -> "value_description"
  | `type_declaration _ -> "type_declaration"
  | `exception_declaration _ -> "exception_declaration"
  | `pattern _ -> "pattern"
  | `expression _ -> "expression"
  | `package_type _ -> "package_type"
  | `signature _ -> "signature"
  | `signature_item _ -> "signature_item"
  | `modtype_declaration _ -> "modtype_declaration"
  | `module_type _ -> "module_type"
  | `module_expr _ -> "module_expr"
  | `with_constraint _ -> "with_constraint"
  | `class_expr _ -> "class_expr"
  | `class_signature _ -> "class_signature"
  | `class_description _ -> "class_description"
  | `class_type_declaration _ -> "class_type_declaration"
  | `class_infos _ -> "class_infos"
  | `class_type _ -> "class_type"
  | `class_type_field _ -> "class_type_field"
  | `core_type _ -> "core_type"
  | `core_field_type _ -> "core_field_type"
  | `class_structure _ -> "class_structure"
  | `class_field _ -> "class_field"
  | `structure_item _ -> "structure_item"
  | `binding _ -> "binding"
  | `bindings _ -> "bindings"

let iterator ~enter ~leave =
  let module Iterator = MakeIterator(struct

    let enter_structure x = enter (`structure x)
    let enter_value_description x = enter (`value_description x)
    let enter_type_declaration x = enter (`type_declaration x)
    let enter_exception_declaration x = enter (`exception_declaration x)
    let enter_pattern x = enter (`pattern x)
    let enter_expression x = enter (`expression x)
    let enter_package_type x = enter (`package_type x)
    let enter_signature x = enter (`signature x)
    let enter_signature_item x = enter (`signature_item x)
    let enter_modtype_declaration x = enter (`modtype_declaration x)
    let enter_module_type x = enter (`module_type x)
    let enter_module_expr x = enter (`module_expr x)
    let enter_with_constraint x = enter (`with_constraint x)
    let enter_class_expr x = enter (`class_expr x)
    let enter_class_signature x = enter (`class_signature x)
    let enter_class_description x = enter (`class_description x)
    let enter_class_type_declaration x = enter (`class_type_declaration x)
    let enter_class_infos x = enter (`class_infos { x with ci_expr = () })
    let enter_class_type x = enter (`class_type x)
    let enter_class_type_field x = enter (`class_type_field x)
    let enter_core_type x = enter (`core_type x)
    let enter_core_field_type x = enter (`core_field_type x)
    let enter_class_structure x = enter (`class_structure x)
    let enter_class_field x = enter (`class_field x)
    let enter_structure_item x = enter (`structure_item x)
    let enter_binding x y = enter (`binding (x, y))
    let enter_bindings x = enter (`bindings x)

    let leave_structure x = leave (`structure x)
    let leave_value_description x = leave (`value_description x)
    let leave_type_declaration x = leave (`type_declaration x)
    let leave_exception_declaration x = leave (`exception_declaration x)
    let leave_pattern x = leave (`pattern x)
    let leave_expression x = leave (`expression x)
    let leave_package_type x = leave (`package_type x)
    let leave_signature x = leave (`signature x)
    let leave_signature_item x = leave (`signature_item x)
    let leave_modtype_declaration x = leave (`modtype_declaration x)
    let leave_module_type x = leave (`module_type x)
    let leave_module_expr x = leave (`module_expr x)
    let leave_with_constraint x = leave (`with_constraint x)
    let leave_class_expr x = leave (`class_expr x)
    let leave_class_signature x = leave (`class_signature x)
    let leave_class_description x = leave (`class_description x)
    let leave_class_type_declaration x = leave (`class_type_declaration x)
    let leave_class_infos x = leave (`class_infos { x with ci_expr = () })
    let leave_class_type x = leave (`class_type x)
    let leave_class_type_field x = leave (`class_type_field x)
    let leave_core_type x = leave (`core_type x)
    let leave_core_field_type x = leave (`core_field_type x)
    let leave_class_structure x = leave (`class_structure x)
    let leave_class_field x = leave (`class_field x)
    let leave_structure_item x = leave (`structure_item x)
    let leave_binding x y = leave (`binding (x, y))
    let leave_bindings x = leave (`bindings x)

  end)
  in
  Iterator.process

let find_all_map cond s =
  let l = ref [] in
  let enter x =
    match cond x with
      | Some x -> l := x :: !l
      | None -> ()
  and leave _ = () in
  iterator ~enter ~leave s;
  List.rev !l

let find_map priority (type a) cond s =
  let module M = struct exception Found of a end in
  let visit x =
    match cond x with
      | Some x -> raise (M.Found x)
      | None -> ()
  in
  let enter, leave = match priority with
    | `innermost -> ignore, visit
    | `outermost -> visit, ignore
  in
  try
    iterator ~leave ~enter s;
    raise Not_found
  with
      M.Found x -> x

let contains loc (b', e') =
  let b, e = Util.get_c_num loc in
  b <= b' && e' <= e

let locate priority loc =
  find_map priority
    (function t ->
      if
	(match t with
	  | `pattern p -> contains p.pat_loc
	  | `expression e -> contains e.exp_loc
	  | `structure_item i -> contains i.str_loc
	  | `signature_item i -> contains i.sig_loc
	  | _ -> function _ -> false)
	  loc
      then Some t
      else None)

let find_pattern priority cond =
  find_map priority (function `pattern p -> cond p | _ -> None)

let find_expression priority cond =
  find_map priority (function `expression e -> cond e | _ -> None)

module NodeTbl = Hashtbl.Make
  (struct
    type t = node
    let equal = ( == )
    let hash = Hashtbl.hash
   end)

type father_table = node NodeTbl.t

let reverse s =
  let t = NodeTbl.create 1000
  and path = ref [] in
  let enter n =
    (match !path with
      | f :: _ -> NodeTbl.add t n f
      | _ -> ());
    path := n :: !path
  and leave _ =
    path :=
      match !path with
	| _ :: p -> p
	| _ -> assert false
  in
  iterator ~enter ~leave s;
  t
