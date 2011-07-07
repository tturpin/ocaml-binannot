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

let keep_first name f lid env = fst (wrap_lookup lid_to_str name f lid env)

let keep_first' f lid env = fst (f lid env)

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
  | Modtype -> "modtype"
  | Class -> "class"
  | Cltype -> "cltype"

let lookup kind lid e =
  let lookup =
    match kind with
      | Value -> keep_first' lookup_value
      | Type -> keep_first' lookup_type
      | Module -> keep_first' lookup_module
      | Constructor -> keep_first' lookup_constructor
      | Label -> keep_first' lookup_label
      | Modtype -> keep_first' lookup_modtype
      | Class -> keep_first' lookup_class
      | Cltype -> keep_first' lookup_cltype
      | Annot -> assert false
  in
    wrap_lookup lid_to_str (kind2str kind) lookup lid e

let sig_item sort item =
  match sort, item with
    | Value, Sig_value (i, _)
    | Type, Sig_type (i, _, _)
    | Module, Sig_module (i, _, _)
    | Modtype, Sig_modtype (i, _) -> Some i
    (* To be completed *)
    | _ -> None

let summary_item kind item =
  match kind, item with
    | Value, Env_value (_, i, _)
    | Type, Env_type (_, i, _)
    | Module, Env_module (_, i, _)
    | Modtype, Env_modtype (_, i, _) -> Some i
    (* To be completed *)
    | _ -> None

let parse_lid kind =
  match kind with
    | Value ->
	(function s ->
	   try parse Parser.val_longident s
	   with _ -> Longident.Lident (parse Parser.operator s))
    | Type -> parse Parser.type_longident
    | Module -> parse Parser.mod_longident (* extended ? *)
    | Modtype -> parse Parser.mty_longident
    | _ -> assert false

(*
let sig_item_ops = function
  | Sig_value _ -> value_ops
  | Sig_module _ -> module_ops
  | Sig_type _ -> type_ops
  | Sig_exception _
  | Sig_modtype _
  | Sig_class _
  | Sig_class_type _ ->
    assert false
*)

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

(* True if p.name means id *)
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

(* Test whether a p reffers to id in environment env. This indicates
   that the rightmost name in lid needs renaming. *)
let resolves_to kind env lid ids =
  match lookup kind lid env with
    | Pident id' -> is_one_of id' ids
    | Pdot (p, n, _) -> field_resolves_to kind env p n ids
    | Papply _ -> invalid_arg "resolves_to"

let lookup_in_signature kind name =
  List.find
    (function item -> match sig_item kind item with
      | Some id -> Ident.name id = name
      | None -> false)

exception Name of Ident.t
exception Ident of Ident.t

let first_of_in_sig kind ids name sg =
  List.iter
    (function item ->
      (match sig_item kind item with
	| Some id ->
	  debugln "found %s" (Ident.name id);
	  if is_one_of id ids then
	    raise (Ident id)
	  else if Ident.name id = name then
	    raise (Name id)
	| None -> ()))
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
	if is_one_of id ids then
	  raise (Ident id)
	else if Ident.name id = name then
	  raise (Name id)
      | None -> ());
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

exception Masked_by of bool * Ident.t

(* Check that the renaming of one of ids in name is not masked in the env. *)

let check_in ~renamed first_of arg =
  try
    ignore (first_of arg);
    assert false
  with
      (Ident _ | Name _) as e ->
	match renamed, e with
	  | (true, Ident _ | false, Name _) -> ()
	  | (true, Name id | false, Ident id) -> raise (Masked_by (renamed, id))
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
