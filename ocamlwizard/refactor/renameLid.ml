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

open Longident
open Resolve

(* Rename the ident id of type renamed_kind in the longident lid of kind sort *)
let rec rename_in_lid
    renamed_kind
    (ids : Ident.t list)
    (name : string)
    (env : Env.t)
    kind
    (lid : Longident.t) =
  let rename = rename_in_lid renamed_kind ids name env module_ops in
  match renamed_kind.sort, lid with
    | _, Lident i ->
      if kind.sort = renamed_kind.sort && resolves_to kind env lid ids then (
	check kind ids name env (Env.summary env) ~renamed:true;
	Some (Lident name)
      ) else
	None
    | _, Ldot (pref, n) ->
      let n' =
	if kind.sort = renamed_kind.sort && resolves_to kind env lid ids then (
	  let _, t = Env.lookup_module pref env in
	  check_in_sig kind ids name (modtype_signature env t) ~renamed:true;
	  Some name
	) else
	  None
      and pref' = rename pref in
      (match pref', n' with
	| None, None -> None
	| None, Some n -> Some (Ldot(pref, n))
	| Some pref, None -> Some (Ldot(pref, n))
	| Some pref, Some n -> Some (Ldot(pref, n)))
    | `Module, Lapply (lid, lid') ->
      (match rename lid, rename lid' with
	| None, None -> None
	| Some lid, None -> Some (Lapply (lid, lid'))
	| None, Some lid' -> Some (Lapply (lid, lid'))
	| Some lid, Some lid' -> Some (Lapply (lid, lid')))
    | _, Lapply _ -> None

let rec check_lid renamed_kind ids name env kind lid =
  let check_lid = check_lid renamed_kind ids name env module_ops in
  match lid with
    | Lident i ->
      if kind.sort = renamed_kind.sort && i = name then
	check kind ids name env (Env.summary env) ~renamed:false
    | Ldot (pref, n) ->
      check_lid pref;
      if kind.sort = renamed_kind.sort && n = name then
	let _, t = Env.lookup_module pref env in
	check_in_sig kind ids name (modtype_signature env t) ~renamed:false
    | Lapply (lid, lid') ->
      if renamed_kind.sort = `Module then (
	check_lid lid;
	check_lid lid'
      )

(* The following it an attempt to solve the renaming in two steps,
   (for module paths, then for arbitrary paths) but it does not seem
   to simplify the second step, so we do all cases at the same time. *)
(*
(* Rename a module name in an extended module path. *)
let rec rename_in_ext_mod_path
    (env : Env.t)
    (id : Ident.t)
    (name' : string)
    (lid : Longident.t) =
  let rename = rename_in_ext_mod_path env id name' in
  match lid with
    | Lident i ->
      if resolves_to module_ops env lid id then (
	check module_ops id name' env (Env.summary env);
	Some (Lident name')
      ) else
	None
    | Ldot (lid', n) ->
      let n' =
	if resolves_to module_ops env lid id then
	  Some name'
	else
	  None
      and lid' = rename lid' in
      (match lid', n' with
	| None, None -> None
	| None, Some n -> Some (Ldot(lid, n))
	| Some lid, None -> Some (Ldot(lid, n))
	| Some lid, Some n -> Some (Ldot(lid, n)))
    | Lapply (lid, lid') ->
      (match rename lid, rename lid' with
	| None, None -> None
	| Some lid, None -> Some (Lapply (lid, lid'))
	| None, Some lid' -> Some (Lapply (lid, lid'))
	| Some lid, Some lid' -> Some (Lapply (lid, lid')))
*)
(*
let rec rename_in_lid
    renamed_kind
    (env : Env.t)
    (id : Ident.t)
    (name' : string)
    kind
    (lid : Longident.t) =
  match renamed_kind.sort with
    | `Module -> rename_in_ext_mod_path env id name'
    | _ ->
      match lid with
	| Lident i ->
      let p, _ = renamed_kind.lookup lid env in
      if kind.sort = renamed_kind.sort &&
	resolves_to renamed_kind env id p then (
	  check_value id name' env (Env.summary env);
	  Some (Lident name')
	) else
  	  None
    | _, Ldot (lid, n) ->
      let p, _ = renamed_kind.lookup lid env in
      if kind.sort = renamed_kind.sort && field_resolves_to kind env p n id then
	Some (Ldot(lid, name'))
      else
	None
    | _, Lapply _ -> invalid_arg "rename_in_lid"
*)
