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
    let t' = modtype env t' in
    let t =
      try modtype env t
      with Abstract_modtype -> assert false
    in
      match t, t' with
	| `sign sg, `sign sg' -> constraint_signature incs env sg sg'
	| `func (_, arg, res), `func (_, arg', res') ->
	    constraint_modtype incs env arg arg';
	    constraint_modtype incs env res res'
	| _ -> assert false
  with
      Abstract_modtype -> ()

and constraint_signature incs env sg sg' =
  incs := ConstraintSet.add (sg, sg') !incs;
  List.iter
    (function
      | Sig_module (id, t, _) ->
	(match
	    lookup_in_signature Env.Module (Ident.name id) sg
	 with
	   | Sig_module (_, t', _) ->
	     constraint_modtype incs env t t'
	   | _ -> assert false)
      | Sig_modtype (id, Modtype_manifest t) ->
	(match
	    lookup_in_signature Env.Modtype (Ident.name id) sg
	 with
	   | Sig_modtype (_, Modtype_manifest t') ->
	     constraint_modtype incs env t t'
	   | _ -> assert false)
      | _ -> ())
    sg'

let constraint_with_cmi env (typedtree : TypedtreeOps.typedtree) cmi =
  let `structure {str_type = sg} | `signature {sig_type = sg} = typedtree in
  let incs = ref ConstraintSet.empty in
  constraint_signature incs env sg cmi;
  !incs

(* Collect the set of signature inclusion constraints implied by a structure.

   signature constraints are missing ! *)
let collect_signature_inclusions s =
  let incs = ref ConstraintSet.empty
  and includes = ref IncludeSet.empty in
  let enter = function
    | `module_expr m  ->
      (match m.mod_desc with

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
	| Tmod_functor _ -> ())

		(* To handle include, we need the correspondency between
		   renamed idents which is currently lost. *)
    | `structure_item s ->
      (match s.str_desc with
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
	| Tstr_class_type _ -> ())
    | _ -> ()
  in
  TypedtreeOps.iterator ~enter ~leave:ignore s;
  !incs, !includes

let string_table_union t t' =
  let open Location.StringTbl in
      let t = copy t in
      iter
	(fun x l -> add t x l)
	t';
      t

module Eq : sig

  type 'a t = ('a, 'a list ref) Hashtbl.t

  val add : 'a t -> 'a -> 'a -> unit

  val find : 'a t -> 'a -> 'a list

  val map : ('a -> 'b) -> 'a t -> 'b t

  val union : 'a t -> 'a t -> 'a t

end = struct
(* An equivalence relation is represented by a mapping from elements
   to their (non-trivial) equivalence class. *)
  type 'a t = ('a, 'a list ref) Hashtbl.t

  let add eq x y =
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

  let map f eq =
    let eq' = Hashtbl.create 10 in
    Hashtbl.iter
      (fun k l -> Hashtbl.add eq' (f k) (ref (List.map f !l)))
      eq;
    eq'

  let union eq eq' =
    (* Copy is not enough *)
    let eq = map (function x -> x) eq in
    Hashtbl.iter
      (fun x l -> List.iter (add eq x) !l)
      eq';
    eq

  let find eq x =
    try !(Hashtbl.find eq x)
    with Not_found -> [x]

end

let propagate_constraints bind_id_to_member kind name incs =
  ConstraintSet.iter
    (function sg, sg' ->
      try
	let id' = find_in_signature kind name sg' in
	bind_id_to_member `certain sg id'
      with
	  Not_found -> ())
    incs

let propagate_includes bind_id_to_member tag ambiguous kind name includes =
  IncludeSet.iter
    (function sg, ids ->
      match List.filter (function id -> Ident.name id = name) ids with
	| [] -> ()
	  (* WARNING ! We should check if the name id define
	     with the right kind in sg ! *)
	| [id] -> bind_id_to_member `maybe sg id
	| ids -> (* correct choice would require access to the resulting
		    environment to check ids w.r.t. kind. *)
	  List.iter (bind_id_to_member `maybe sg) ids;
	    (* because we still need to check them for capture *)
	  ambiguous := tag (find_in_signature kind name sg) :: !ambiguous)
    includes

let propagate_one_file eq implicit_refs ambiguous env kind name tag cmi_tag s cmi =
  let bind_id_to_member tag tag' flag sg id' =
    try
      let id = find_in_signature kind name sg in
      let id = tag id and id' = tag' id' in
      implicit_refs := (flag, sg, id') :: !implicit_refs;
      Eq.add eq id id'
    with Not_found -> invalid_arg "bind_id_to_member"
  in
  let incs, includes = collect_signature_inclusions s in
  let cmi_incs = constraint_with_cmi env s cmi in
  propagate_constraints (bind_id_to_member tag tag) kind name incs;
  propagate_includes (bind_id_to_member tag tag) tag ambiguous kind name includes;
  propagate_constraints (bind_id_to_member tag cmi_tag) kind name cmi_incs

(*
let propagate_renamings kind id incs includes idents =
  let name = Ident.name id in
  let eq, ambiguous, implicit_refs =
    propagate_renamings kind name incs includes in
  Hashtbl.add eq id (ref [id]);
  let ids = Eq.find eq id in
  let locs = List.map
    (function id ->
      try
	Locate.ident_def idents id
      with Not_found ->
	fail_owz "Cannot perform renaming because a member of a persistent \
                      structure would be impacted")
    ids
  in
  List.iter
    (function id ->
      if is_one_of id ids then
	failwith
	  "Cannot perform renaming because of an ambiguous include")
    ambiguous;
  ids, locs, implicit_refs
*)

let propagate_all_files env loc kind id files =
  let eq = Hashtbl.create 10 in
  let implicit_refs = ref []
  and ambiguous = ref [] in
  let name = Ident.name id in
  let idents =
    List.fold_left
      (fun idents ((_, ids, _, _), _) -> string_table_union idents ids)
      (Location.StringTbl.create 10)
      files in
(*
  let tag file_kind id = file_kind, id in
*)
  let tag id =
    (try `source (Locate.ident_def idents id) with Not_found -> `none),
    id
  and cmi_tag id = `cmi, id in
  List.iter
    (function (source, _, _, _), cmi ->
      let propagate = propagate_one_file eq implicit_refs ambiguous env kind name in
      propagate tag cmi_tag source cmi.sig_type)
    files;
  let ids = Eq.find eq (`source loc, id) in
(*
  let locs = List.map
    (function file_kind, id ->
      let idents = match file_kind with `ml -> ml_ids | `mli -> mli_ids in
      try
	Locate.ident_def idents id
      with Not_found ->
	fail_owz "Cannot perform renaming because a member of a persistent \
                      structure would be impacted")
    ids
  in
*)
  (* Check if propagation reached an unlocalised id *)
  List.iter
    (function loc, id ->
      if loc = `none then	
	fail_owz "Cannot perform renaming because a member of a persistent \
                  structure would be impacted")
    ids;
  (* Check if ids intersect ambiguous *)
  List.iter
    (function id ->
      if List.mem id ids then
	failwith
	  "Cannot perform renaming because of an ambiguous include")
    !ambiguous;
  ids, (*locs,*) !implicit_refs

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
	 let _ = find_in_signature renamed_kind name' sg' in
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
