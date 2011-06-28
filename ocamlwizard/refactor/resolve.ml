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

type sort = [
  | `Module
  | `Modtype
  | `Value
]

type specifics = {
  sort : sort;
  lookup : Longident.t -> Env.t -> Path.t;
  sig_item : Types.signature_item -> Ident.t option;
  summary_item : Env.summary -> Ident.t option
}

let keep_first f lid env = fst (f lid env)

let value_ops = {
  sort = `Value;
  lookup = keep_first Env.lookup_value;
  sig_item = (function Sig_value (i, _) -> Some i | _ -> None);
  summary_item = function Env_value (_, i, _) -> Some i | _ -> None
}

let module_ops = {
  sort = `Module;
  lookup = keep_first Env.lookup_module;
  sig_item = (function Sig_module (i, _, _) -> Some i | _ -> None);
  summary_item = function Env_module (_, i, _) -> Some i | _ -> None
}

let modtype_ops = {
  sort = `Modtype;
  lookup = keep_first Env.lookup_modtype;
  sig_item = (function Sig_modtype (i, _) -> Some i | _ -> None);
  summary_item = function Env_modtype (_, i, _) -> Some i | _ -> None
}

let sig_item_ops = function
  | Sig_value _ -> value_ops
  | Sig_module _ -> module_ops
  | Sig_type _
  | Sig_exception _
  | Sig_modtype _
  | Sig_class _
  | Sig_class_type _ ->
    assert false

exception Abstract_modtype

(* Return the signature of a given (extended) module type path *)
let rec resolve_modtype env path =
  match Env.find_modtype path env with
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
  modtype_signature env (Env.find_module path env)

let is_one_of id = List.exists (Ident.same id)

(* True if p.name means id *)
let field_resolves_to kind env path name ids =
  name = Ident.name (List.hd ids) && (* only an optimisation *)
  try
    List.exists
      (function s ->
	match kind.sig_item s with
	  | Some id -> Ident.name id = name && is_one_of id ids
	  | None -> false)
      (resolve_module env path)
  with
      Abstract_modtype -> assert false

(* Test whether a p reffers to id in environment env. This indicates
   that the rightmost name in lid needs renaming. *)
let resolves_to kind env lid ids =
  match kind.lookup lid env with
    | Pident id' -> is_one_of id' ids
    | Pdot (p, n, _) -> field_resolves_to kind env p n ids
    | Papply _ -> invalid_arg "resolves_to"

let lookup_in_signature kind name =
  List.find
    (function item -> match kind.sig_item item with
      | Some id -> Ident.name id = name
      | None -> false)

exception Name of Ident.t
exception Ident of Ident.t

let first_of_in_sig kind ids name sg =
  List.iter
    (function item ->
      (match kind.sig_item item with
	| Some id ->
	  debugln "found %s" (Ident.name id);
	  if is_one_of id ids then
	    raise (Ident id)
	  else if Ident.name id = name then
	    raise (Name id)
	| None -> ()))
    (List.rev sg);
  invalid_arg "ckeck_in_sig"

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
    (match kind.summary_item summary with
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

exception Masked_by of Ident.t

(* Check that the renaming of one of ids in name is not masked in the env. *)

let check ~renamed first_of arg =
  try
    ignore (first_of arg);
    assert false
  with
      (Ident _ | Name _) as e ->
	match renamed, e with
	  | (true, Ident _ | false, Name _) -> ()
	  | (true, Name id | false, Ident id) -> raise (Masked_by id)
	  | _ -> assert false

let check kind id name env summary =
  check (first_of kind id name env) summary

and check_in_sig kind id name sg =
  check (first_of_in_sig kind id name) sg

(*
let check kind id name env summary =
  try
    ignore (first_of kind id name env summary);
    assert false
  with
    | Ident _ -> ()
    | Name id -> raise (Masked_by id)

let check_in_sig kind id name sg =
  try
    ignore (first_of_in_sig kind id name sg);
    assert false
  with
    | Ident _ -> ()
    | Name id -> raise (Masked_by id)

let check_other kind id name env summary =
  try
    ignore (first_of kind id name env summary);
    assert false
  with
    | Name _ -> ()
    | Ident id -> raise (Masked_by id)

let check_other_in_sig kind id name sg =
  try
    ignore (first_of_in_sig kind id name sg);
    assert false
  with
    | Name _ -> ()
    | Ident id -> raise (Masked_by id)
*)
