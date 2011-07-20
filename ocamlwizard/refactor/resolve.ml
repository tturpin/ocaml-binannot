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

open Path
open Types
open Env
open Util
open Env

type source_kind = [`ml | `mli]

type source_file = string * source_kind

type ident_context = [`pers of string | `source of source_file]

type global_ident = ident_context * Ident.t

let source2string (prefix, kind) =
  prefix ^ 
    match kind with
      | `ml -> ".ml"
      | `mli -> ".mli"

let context2string = function
  | `pers m -> m ^ "(cmi)"
  | `source s -> source2string s

let wrap_lookup to_string name lookup x e =
  try lookup x e
  with Not_found -> failwith ("unbound " ^ name ^ " " ^ to_string x)

let keep_first f lid env = fst (f lid env)

let parse parser s =
  let lexbuf = Lexing.from_string s in
  parser Lexer.token lexbuf

let kind2string = function
  | Value -> "value"
  | Type -> "type"
  | Annot -> "annot"
  | Constructor -> "constructor"
  | Label -> "label"
  | Module -> "module"
  | Modtype -> "module type"
  | Class -> "class"
  | Cltype -> "class type"

let lookup kind lid e =
  let lookup =
    match kind with
      | Value -> keep_first lookup_value
      | Type -> keep_first lookup_type
      | Module -> keep_first lookup_module
      | Constructor -> keep_first lookup_constructor
      | Label -> keep_first lookup_label
      | Modtype -> keep_first lookup_modtype
      | Class -> keep_first lookup_class
      | Cltype -> keep_first lookup_cltype
      | Annot -> assert false
  in
    wrap_lookup lid_to_str (kind2string kind) lookup lid e

(* Get the ident of a signature item, if it has one, and matches the kind. *)
let sig_item sort item =
  match sort, item with
    | Value, Sig_value (i, _)
    | Type, Sig_type (i, _, _)
    | Module, Sig_module (i, _, _)
    | Modtype, Sig_modtype (i, _)
    | Class, Sig_class (i, _, _)
    | Cltype, Sig_class_type (i, _, _)
    | Constructor, Sig_exception (i, _)
      -> Some i
    | _ -> None

(* Get the ident of a summary item, if it has one, and matches the kind. *)
let summary_item kind item =
  match kind, item with
    | Value, Env_value (_, i, _)
    | Type, Env_type (_, i, _)
    | Module, Env_module (_, i, _)
    | Modtype, Env_modtype (_, i, _)
    | Class, Env_class (_, i, _)
    | Cltype, Env_cltype (_, i, _)
    | Constructor, Env_exception (_, i, _)
      -> Some i
    | _ -> None

let parse_lid kind =
  match kind with
    | Value ->
	(function s ->
	   try parse Parser.val_longident s
	   with _ -> Longident.Lident (parse Parser.operator s))
    | Type -> parse Parser.type_longident
    | Module -> parse Parser.mod_ext_longident
    | Modtype -> parse Parser.mty_longident
    | Constructor -> parse Parser.constr_longident
    | Label -> parse Parser.label_longident
    | Class -> parse Parser.class_longident
    | Cltype -> parse Parser.clty_longident
    | Annot -> assert false

exception Abstract_modtype

(* Return the signature of a given (extended) module type path *)
let rec resolve_modtype0 env path =
  match wrap_lookup Path.name "module type" find_modtype path env with
  | Modtype_abstract -> raise Abstract_modtype
  | Modtype_manifest mt -> modtype0 env mt

and modtype0 env = function
  | Mty_ident p -> resolve_modtype0 env p
  | Mty_signature s -> `sign s
  | Mty_functor (id, t, t') -> `func (id, t, t')

let global modname path =
  let m = Path.head path in
  if Ident.persistent m then `pers (Ident.name m) else modname

(* Return the signature of a given (extended) module type path *)
let rec resolve_modtype modname env path =
  let modname = global modname path in
  match wrap_lookup Path.name "module type" find_modtype path env with
    | Modtype_abstract -> raise Abstract_modtype
    | Modtype_manifest mt -> modtype modname env mt

and modtype modname env = function
  | Mty_ident p -> resolve_modtype modname env p
  | Mty_signature s -> modname, `sign s
  | Mty_functor (id, t, t') -> modname, `func (id, t, t')

let modtype_signature0 env m =
  match modtype0 env m with
    | `sign s -> s
    | `func _ -> invalid_arg "modtype_signature"

let modtype_signature modname env m =
  match modtype modname env m with
    | source, `sign s -> source, s
    | _, `func _ -> invalid_arg "modtype_signature"

let modtype_functor modname env m =
  match modtype modname env m with
    | source, `func f -> source, f
    | _, `sign _ -> invalid_arg "modtype_signature"

(* Return the signature of a given (extended) module path *)
let resolve_module env path =
  modtype_signature0 env (wrap_lookup Path.name "module" find_module path env)

let resolve_module' modname env path =
  modtype_signature modname env (wrap_lookup Path.name "module" find_module path env)

(* unused *)
let resolve_module_lid env lid =
  modtype_signature0 env
    (snd (wrap_lookup lid_to_str "module" lookup_module lid env))

exception Found of int * Ident.t

(* We assume that the kind is correct *)
let first_of_in_id names id =
  let name = Ident.name id in
  Array.iteri
    (fun i n ->
      if name = n then
	raise (Found (i, id)))
    names

(* The type itself is excluded *)
let first_of_in_type_decl kind names tdecl =
  match kind, tdecl.type_kind with
    | Constructor, Type_variant constrs ->
      List.iter
	(function id, _ -> first_of_in_id names id)
	constrs
    | Label, Type_record (fields, _) ->
      List.iter
	(function id, _, _ -> first_of_in_id names id)
	fields
    | _ -> ()

let first_of_in_sig kind names sg =
  List.iter
    (function item ->
      (match sig_item kind item with
	| Some id ->
	  first_of_in_id names id
	| None -> ());
      (match item with
	| Sig_type (s, tdecl, _) ->
	  first_of_in_type_decl kind names tdecl
	| _ -> ()))
    (List.rev sg);
  raise Not_found

let rec first_of kind names env = function
  | Env_empty -> raise Not_found
  | Env_open (s, p) ->
    let sign = resolve_module env p in
    (try
       first_of_in_sig kind names sign
     with
	 Not_found ->
	   first_of kind names env s)
  | summary ->
    (match summary_item kind summary with
      | Some id ->
	  first_of_in_id names id
      | None -> ());
    (match summary with
      | Env_type (s, _, tdecl) ->
	first_of_in_type_decl kind names tdecl
      | _ -> ());
    match summary with
      | Env_value (s, _, _)
      | Env_type (s, _, _)
      | Env_exception (s, _, _)
      | Env_module (s, _, _)
      | Env_modtype (s, _, _)
      | Env_class (s, _, _)
      | Env_cltype (s, _, _)
	-> first_of kind names env s
      | Env_open _ | Env_empty _ -> assert false

let add_environments env sg =
  let _, sg =
    List.fold_left
      (fun (env, sg) item -> Env.add_item item env, (env, item) :: sg)
      (env, [])
      sg
  in
  List.rev sg

let lookup_in_signature kind name sg =
  if kind = Module || kind = Modtype then
    List.find
      (function _, item -> match sig_item kind item with
	| Some id -> Ident.name id = name
	| None -> false)
      sg
  else
    invalid_arg "lookup_in_signature"


let find_in_signature kind name sg =
  try
    first_of_in_sig kind [|name|] sg
  with
    | Found (0, id) -> id
    | Found _ -> assert false

(* True if p.name means id *)
let member_resolves_to kind env modname path name ids =
  try
    let modname, sg = resolve_module' modname env path in
    List.mem
      (modname, find_in_signature kind name sg)
      ids
  with
    | Not_found -> false

(* Test whether a p reffers to id in environment env. This indicates
   that the rightmost name in lid needs renaming. *)
let resolves_to kind env modname lid ids =
  match lookup kind lid env with
    | Pident id' as p -> List.mem (global modname p,  id') ids
    | Pdot (p, n, _) -> member_resolves_to kind env (global modname p) p n ids
    | Papply _ -> invalid_arg "resolves_to"

exception Masked_by of bool * global_ident

(* Check that the renaming of one of ids in name is not masked in the env. *)

let check_in ctx ~renamed first_of ~ids ~new_name =
  try
    let old_name = Ident.name (snd (List.hd ids)) in
    ignore (first_of [|old_name ; new_name|]);
    assert false
  with
      Found (i, id) ->
	match renamed, i with
	  | (true, 0 | false, 1) -> ()
	  | (true, 1 | false, 0) ->
	    (* for a non-renamed existing occurrence of new_name, we check that
	       the found definition of old_name actually points to ids. *)
	    if renamed || List.mem (ctx, id) ids then
	      raise (Masked_by (renamed, (ctx, id)))
	  | _ -> assert false

let check kind (ctx, env, summary) =
  try
    check_in ctx (function names -> first_of kind names env summary)
  with
      Not_found -> invalid_arg "ckeck_in_sig"

and check_in_sig kind (ctx, sg) =
  try
    check_in ctx (function names -> first_of_in_sig kind names sg)
  with
      Not_found -> invalid_arg "ckeck_in_sig"
