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
open Types
open Typedtree
open Resolve

let sig_item_id = function
  | Sig_value (i, _)
  | Sig_type (i, _, _)
  | Sig_exception (i, _)
  | Sig_module (i, _, _)
  | Sig_modtype (i, _)
  | Sig_class (i, _, _)
  | Sig_class_type (i, _, _)
    -> i

module ConstraintSet =
  Set.Make (struct
    type t = Types.signature * Types.signature
    let compare = compare
  end)

module IncludeSet =
  Set.Make (struct
    type t = Types.signature * Ident.t list
    let compare = compare
  end)

let rec constraint_modtype incs env t t' =
  (* We rely on the fact that
      "module type X
     does not match
       module type X = sig  end" *)
  try
    let sg' = modtype_signature env t' in
    let sg =
      try modtype_signature env t
      with Abstract_modtype -> assert false
    in
    constraint_signature incs env sg sg'
  with
      Abstract_modtype -> ()

and constraint_signature incs env sg sg' =
  incs := ConstraintSet.add (sg, sg') !incs;
  List.iter
    (function
      | Sig_module (id, t, _) ->
	(match
	    lookup_in_signature module_ops (Ident.name id) sg
	 with
	   | Sig_module (_, t', _) ->
	     constraint_modtype incs env t t'
	   | _ -> assert false)
      | Sig_modtype (id, Modtype_manifest t) ->
	(match
	    lookup_in_signature modtype_ops (Ident.name id) sg
	 with
	   | Sig_modtype (_, Modtype_manifest t') ->
	     constraint_modtype incs env t t'
	   | _ -> assert false)
      | _ -> ())
    sg'

(* Collect the set of signature inclusion constraints implied by a structure. *)
let collect_signature_inclusions s =
  let incs = ref ConstraintSet.empty
  and includes = ref IncludeSet.empty in
  let module Rename =
	MakeIterator
	  (struct
	    include DefaultIteratorArgument

	    let enter_module_expr m =
	      match m.mod_desc with

		(* TODO : fix environments here *)
		| Tmod_constraint (m, t, cs, co) ->
		  constraint_modtype incs m.mod_env m.mod_type t
		(* what about cs and co ? *)

		| Tmod_apply (f, m, co) ->
		  let (_, t, _) = modtype_functor f.mod_env f.mod_type in
		  constraint_modtype incs f.mod_env m.mod_type t
		(* what about co ? *)

		| Tmod_unpack _ -> assert false (* TODO *)

		| Tmod_ident _
		| Tmod_structure _
		| Tmod_functor _ -> ()

	    (* To handle include, we need the correspondency between
	       renamed idents which is currently lost. *)
	    let enter_structure_item s = match s.str_desc with
	      | Tstr_include (m, ids) ->

		(* We may have
  		     module G(X : sig module type T module X : T end) =
                       struct include X end *)

		(try
		   let sign = modtype_signature m.mod_env m.mod_type in
		     includes := IncludeSet.add (sign, ids) !includes
		 with Abstract_modtype -> ())

	      | Tstr_eval _
	      | Tstr_value _
	      | Tstr_primitive _
	      | Tstr_type _
	      | Tstr_exception _
	      | Tstr_exn_rebind _
	      | Tstr_module _
	      | Tstr_recmodule _
	      | Tstr_modtype _
	      | Tstr_open _
	      | Tstr_class _
	      | Tstr_class_type _ -> ()

	   end)
  in
  Rename.iter_structure s;
  !incs, !includes

(* An equivalence relation is represented by a mapping from elements
   to their (non-trivial) equivalence class. *)
type 'a equivalence = ('a, 'a list ref) Hashtbl.t

let add_rel eq x y =
  let open Hashtbl in
      match x, y, mem eq x, mem eq y with
	| _, _, false, false ->
	  let l = ref [x ; y] in
	  add eq x l;
	  add eq y l
	| _, _, true, true ->
	  let lx = find eq x and ly = find eq y in
	  if lx !=  ly then (
	    lx := List.rev_append !ly !lx;
	    List.iter
	      (fun y -> replace eq y lx)
	      !ly
	  )
	| x, y, true, false
	| y, x, false, true ->
	  let x = find eq x in
	  x := y :: !x;
	  add eq y x

(* Return the set of ids that would need to be renamed simultaneously
   with id, and the list of "implicit" references which cause this
   need (so that we can check them for masking). *)
let propagate_renamings kind id incs includes =
  let name = Ident.name id in
  let eq = Hashtbl.create 10 in
    Hashtbl.add eq id (ref [id]);
    let implicit_refs = ref []
    and ambiguous = ref [] in
    let copy flag sg id' =
      try 
	let item = lookup_in_signature kind name sg in
	let id = sig_item_id item in
	  implicit_refs := (flag, sg, id') :: !implicit_refs;
	  add_rel eq id id'
      with Not_found -> assert false
    in
      ConstraintSet.iter
	(function sg, sg' ->
	   try
	     let item' = lookup_in_signature kind name sg' in
	     let id' = sig_item_id item' in
	       copy `certain sg id'
	   with
	       Not_found -> ())
      incs;
      IncludeSet.iter
	(function sg, ids ->
	   match List.filter (function id -> Ident.name id = name) ids with
	     | [] -> ()
		 (* WARNING ! We should check if the name id define
		    with the right kind in sg ! *)
	     | [id] -> copy `maybe sg id
	     | ids -> (* correct choice would require access to the resulting
			 environment to check ids w.r.t. kind. *)
		 List.iter (copy `maybe sg) ids;
		 (* because we still need to check them for capture *)
		 ambiguous := (lookup_in_signature kind name sg) :: !ambiguous)
      includes;
      let ids = !(Hashtbl.find eq id) in
	List.iter
	  (function item ->
	     let id = sig_item_id item in
	       if is_one_of id ids then
		 failwith
		   "Cannot perform renaming because of an ambiguous include")
	  !ambiguous;
	ids, !implicit_refs

(* Check that the implicit ident references which are concerned by
   renaming will not be masked (i.e., that the bound signature items
   remain the same). *)
let check_renamed_implicit_references renamed_kind ids name' implicit_refs =
  List.iter
    (function flag, sg, id ->
      try
	if is_one_of id ids then
	  check_in_sig renamed_kind ids name' sg ~renamed:true
      with Not_found ->
	assert (flag = `maybe))
    implicit_refs


(* Check that the implicit ident references which are concerned by
   renaming will not be masked (i.e., that the bound signature items
   remain the same). *)
let check_other_implicit_references renamed_kind ids name' incs includes =
  ConstraintSet.iter
    (function sg, sg' ->
       try
	 let _ = lookup_in_signature renamed_kind name' sg' in
	   check_in_sig renamed_kind ids name' sg ~renamed:false
       with
	   Not_found -> ())
    incs;
  IncludeSet.iter
    (function sg, ids' ->
       match List.filter (function id -> Ident.name id = name') ids' with
	 | [] -> ()
	 | _ ->
	     try
	       check_in_sig renamed_kind ids name' sg ~renamed:false
	     with
	         Not_found -> ()
		   (* Because we don't know the sort od these ids *))
    includes
