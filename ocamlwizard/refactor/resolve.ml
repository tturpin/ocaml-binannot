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

let wrap_lookup to_string name lookup x e =
  try lookup x e
  with Not_found -> failwith ("unbound " ^ name ^ " " ^ to_string x)

let keep_first f lid env = fst (f lid env)

let parse parser s =
  let lexbuf = Lexing.from_string s in
  parser Lexer.token lexbuf

let kind2str = function
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
    wrap_lookup lid_to_str (kind2str kind) lookup lid e

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
let rec resolve_modtype env path =
  match wrap_lookup Path.name "module type" find_modtype path env with
  | Modtype_abstract -> raise Abstract_modtype
  | Modtype_manifest mt -> modtype env mt

and modtype env = function
  | Mty_ident p -> resolve_modtype env p
  | Mty_signature s -> `sign s
  | Mty_functor (id, t, t') -> `func (id, t, t')

let modtype_signature env m =
  match modtype env m with
  | `sign s -> s
  | `func _ -> invalid_arg "modtype_signature"

let modtype_functor env m =
  match modtype env m with
  | `func f -> f
  | `sign _ -> invalid_arg "modtype_signature"

(* Return the signature of a given (extended) module path *)
let resolve_module env path =
  modtype_signature env (wrap_lookup Path.name "module" find_module path env)

(* unused *)
let resolve_module_lid env lid =
  modtype_signature env
    (snd (wrap_lookup lid_to_str "module" lookup_module lid env))


let is_one_of id = List.exists (Ident.same id)

exception FoundName of Ident.t
exception FoundIdent of Ident.t

let first_of_in_id ids name id =
  if is_one_of id ids then
    raise (FoundIdent id)
  else if Ident.name id = name then
    raise (FoundName id)

(* The type itself is excluded *)
let first_of_in_type_decl kind ids name tdecl =
  match kind, tdecl.type_kind with
    | Constructor, Type_variant constrs ->
      List.iter
	(function id, _ -> first_of_in_id ids name id)
	constrs
    | Label, Type_record (fields, _) ->
      List.iter
	(function id, _, _ -> first_of_in_id ids name id)
	fields
    | _ -> ()

let first_of_in_sig kind ids name sg =
  List.iter
    (function item ->
      (match sig_item kind item with
	| Some id ->
	  first_of_in_id ids name id
	| None -> ());
      (match item with
	| Sig_type (s, tdecl, _) ->
	  first_of_in_type_decl kind ids name tdecl
	| _ -> ()))
    (List.rev sg);
  raise Not_found

let rec first_of kind ids name env = function
  | Env_empty -> raise Not_found
  | Env_open (s, p) ->
    let sign = resolve_module env p in
    (try
       first_of_in_sig kind ids name sign
     with
	 Not_found ->
	   first_of kind ids name env s)
  | summary ->
    (match summary_item kind summary with
      | Some id ->
	  first_of_in_id ids name id
      | None -> ());
    (match summary with
      | Env_type (s, _, tdecl) ->
	first_of_in_type_decl kind ids name tdecl
      | _ -> ());
    match summary with
      | Env_value (s, _, _)
      | Env_type (s, _, _)
      | Env_exception (s, _, _)
      | Env_module (s, _, _)
      | Env_modtype (s, _, _)
      | Env_class (s, _, _)
      | Env_cltype (s, _, _)
	-> first_of kind ids name env s
      | Env_open _ | Env_empty _ -> assert false

(* Old implementation ; does not work for fields and constructors
let field_resolves_to kind env path name ids =
  name = Ident.name (List.hd ids) && (* only an optimisation *)
  try
    List.exists
      (function s ->
	match sig_item kind s with
	  | Some id -> Ident.name id = name && is_one_of id ids
	  | None -> false)
      (resolve_module env path)
  with
      Abstract_modtype -> assert false
*)

let lookup_in_signature kind name sg =
  if kind = Module || kind = Modtype then
    List.find
      (function item -> match sig_item kind item with
	| Some id -> Ident.name id = name
	| None -> false)
      sg
  else
    invalid_arg "lookup_in_signature"

let find_in_signature kind name sg =
  try
    first_of_in_sig kind [] name sg
  with
    | FoundName id -> id
    | FoundIdent _ -> assert false

(* True if p.name means id *)
let member_resolves_to kind env path name ids =
  try
    is_one_of
      (find_in_signature kind name (resolve_module env path))
      ids
  with
    | Not_found -> false

(* Test whether a p reffers to id in environment env. This indicates
   that the rightmost name in lid needs renaming. *)
let resolves_to kind env lid ids =
  match lookup kind lid env with
    | Pident id' -> is_one_of id' ids
    | Pdot (p, n, _) -> member_resolves_to kind env p n ids
    | Papply _ -> invalid_arg "resolves_to"

exception Masked_by of bool * Ident.t

(* Check that the renaming of one of ids in name is not masked in the env. *)

let check_in ~renamed first_of arg =
  try
    ignore (first_of arg);
    assert false
  with
      (FoundIdent _ | FoundName _) as e ->
	match renamed, e with
	  | (true, FoundIdent _ | false, FoundName _) -> ()
	  | (true, FoundName id | false, FoundIdent id) -> raise (Masked_by (renamed, id))
	  | _ -> assert false

let check kind id name env summary =
  try
    check_in (first_of kind id name env) summary
  with
      Not_found -> invalid_arg "ckeck_in_sig"

and check_in_sig kind id name sg =
  try
    check_in (first_of_in_sig kind id name) sg
  with
      Not_found -> invalid_arg "ckeck_in_sig"
