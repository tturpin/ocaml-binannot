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

open Util
open Location
open Types
open Typedtree
open TypedtreeOps
open Env

let contains loc (b', e') =
  let b, e = Util.get_c_num loc in
  b <= b' && e' <= e

let locate_map priority f loc =
  find_map priority
    (function t ->
      if
	(match t with
	  | `module_expr e -> contains e.mod_loc
	  | `module_type t -> contains t.mty_loc
	  | `pattern p -> contains p.pat_loc
	  | `expression e -> contains e.exp_loc
	  | `structure_item i -> contains i.str_loc
	  | `signature_item i -> contains i.sig_loc
	  | `type_declaration d -> contains d.typ_loc
	  | _ -> function _ -> false)
	  loc
      then f t
      else None)

let locate priority = locate_map priority (function x -> Some x)

let locate_field loc pid ploc fs tfs =
  let f, _ =
    List.find
      (function _, tf -> contains (ploc tf) loc)
      (List.combine fs tfs)
  in
  pid f

let locate_id loc pid ploc decls =
  pid
    (List.find
       (function d -> contains (ploc d) loc)
       decls)

let ident_def   table id = StringTbl.find table (Ident.name id)

(* Missing: Tstr_class_type, class_infos, Tmeth_val, cstr_meths,
   Tcf_inher, Tcf_val, Tcf_let *)
let longident table loc =
  locate_map `innermost
    (function n ->
      try Some (match n with

	(* Values *)
	| `pattern {pat_desc = Tpat_var id}
	| `pattern {pat_desc = Tpat_alias (_, TPat_alias id)}
	| `expression {exp_desc = Texp_for (id, _, _, _, _)}
	| `structure_item {str_desc = Tstr_primitive (id, _)}
	| `signature_item {sig_desc = Tsig_value (id, _)} -> Value, id

	| `class_expr {cl_desc =
	    Tcl_fun (_, _, bs, _, _) | Tcl_let (_, _, bs, _)} ->
	  Value, locate_id loc fst (function id, _ -> ident_def table id) bs

	    (* Modules *)
	| `structure_item {str_desc = Tstr_module (id, _)}
	| `signature_item {sig_desc = Tsig_module (id, _)}
	| `module_expr {mod_desc = Tmod_functor (id, _, _)}
	| `module_type {mty_desc = Tmty_functor (id, _, _)}
	| `expression {exp_desc = Texp_letmodule (id, _, _)} -> Module, id

	| `structure_item {str_desc = Tstr_recmodule mods} ->
	  Module,
	  locate_id loc (function id, _, _ -> id)
	    (function id, _, _ -> ident_def table id) mods

	| `signature_item {sig_desc = Tsig_recmodule mods} ->
	  Module,
	  locate_id loc fst (function id, _ -> ident_def table id) mods

	    (* Module types *)
	| `structure_item {str_desc = Tstr_modtype (id, _)}
	| `signature_item {sig_desc = Tsig_modtype (id, _)} -> Modtype, id

	    (* Types *)
	| `structure_item {str_desc = Tstr_type types}
	| `signature_item {sig_desc = Tsig_type types} ->
	  Type, locate_id loc fst (function _, d -> d.typ_loc) types

	    (* Constructors, fields, and exceptions *)
	| `type_declaration d ->
	  (match d.typ_type.type_kind, d.typ_kind with
	    | Type_variant cs, Ttype_variant tcs ->
	      Constructor,
	      locate_field loc fst (function _, _, loc -> loc) cs tcs
	    | Type_record (fs, _), Ttype_record tfs ->
	      Label,
	      locate_field loc
		(function id, _, _ -> id) (function _, _, _, loc -> loc)
		fs tfs
	    | Type_abstract, Ttype_abstract -> raise Not_found
	    | _ -> assert false)

	| `structure_item {str_desc = Tstr_exception (id, _)}
	| `structure_item {str_desc = Tstr_exn_rebind (id, _)}
	| `signature_item {sig_desc = Tsig_exception (id, _)} -> Constructor, id

	| _ -> raise Not_found)
      with Not_found -> None)
    loc
