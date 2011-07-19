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
open Util

(* Rename the ident id of type renamed_kind in the longident lid of kind sort *)
let rec rename_in_lid renamed_kind ids name env kind modname lid =
  let old = Ident.name (snd (List.hd ids)) in
  let rename = rename_in_lid renamed_kind ids name env Env.Module modname in
  let modname = `source modname in
  match renamed_kind, lid with
    | _, Lident i ->
      if kind = renamed_kind && resolves_to kind env modname lid ids then (
	check kind ~fst:old ~snd:name env (Env.summary env) ~renamed:true;
	Some (Lident name)
      ) else
	None
    | _, Ldot (pref, n) ->
      let n' =
	if kind = renamed_kind && resolves_to kind env modname lid ids then (
	  let _, t = wrap_lookup lid_to_str "module" Env.lookup_module pref env in
	  check_in_sig kind ~fst:old ~snd:name (modtype_signature env t) ~renamed:true;
	  Some name
	) else
	  None
      and pref' = rename pref in
      (match pref', n' with
	| None, None -> None
	| None, Some n -> Some (Ldot(pref, n))
	| Some pref, None -> Some (Ldot(pref, n))
	| Some pref, Some n -> Some (Ldot(pref, n)))
    | Env.Module, Lapply (lid, lid') ->
      (match rename lid, rename lid' with
	| None, None -> None
	| Some lid, None -> Some (Lapply (lid, lid'))
	| None, Some lid' -> Some (Lapply (lid, lid'))
	| Some lid, Some lid' -> Some (Lapply (lid, lid')))
    | _, Lapply _ -> None

let rec check_lid renamed_kind old name env kind lid =
  let check_lid = check_lid renamed_kind old name env Env.Module in
  match lid with
    | Lident i ->
      if kind = renamed_kind && i = name then
	check kind ~fst:old ~snd:name env (Env.summary env) ~renamed:false
    | Ldot (pref, n) ->
      check_lid pref;
      if kind = renamed_kind && n = name then
	let _, t = wrap_lookup lid_to_str "module" Env.lookup_module pref env in
	check_in_sig kind ~fst:old ~snd:name (modtype_signature env t) ~renamed:false
    | Lapply (lid, lid') ->
      if renamed_kind = Env.Module then (
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
  match renamed_kind with
    | `Module -> rename_in_ext_mod_path env id name'
    | _ ->
      match lid with
	| Lident i ->
      let p, _ = renamed_kind.lookup lid env in
      if kind = renamed_kind &&
	resolves_to renamed_kind env id p then (
	  check_value id name' env (Env.summary env);
	  Some (Lident name')
	) else
  	  None
    | _, Ldot (lid, n) ->
      let p, _ = renamed_kind.lookup lid env in
      if kind = renamed_kind && field_resolves_to kind env p n id then
	Some (Ldot(lid, name'))
      else
	None
    | _, Lapply _ -> invalid_arg "rename_in_lid"
*)
