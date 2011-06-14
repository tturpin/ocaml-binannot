(**************************************************************************)
(*                                                                        *)
(*  Ocamlwizard                                                           *)
(*  David Baudet and Mohamed Iguernelala                                  *)
(*  Copyright 2008 INRIA Saclay - Ile-de-France                           *)
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

(** This module contains functions which are used to extract informations
    from a given module recursively. These functions build the module 
    arborescence and apply filters on each signature of the eventual 
    sub-modules to extract the desired informations.*)

open Outcometree
open Format
open Interface
open Cmireader
open Debug
open Util

   
(** This record is used to store the result of a filtration operated
    on a signature : 
    - path    : The path of the signature in the module arborescence.
    - content : The result of the selection 
    - level   : The level of the current signature in the module 
    arborescence. *)
type 'a filter_result = {
    path    : string ;
    content : 'a ;
    level   : int
  }
    
(** This function doesn't select anything from the signature content.
    We only need to build the modules arborescence.
    Therefore we don't need to have an accumulator. 
    (The given accumulator will not be used) *)
      
let select_none filter_acc  signa_item = filter_acc
  
(** This function select values from a signature.
    If the given signature item is an "Osig_value", it
    adds its content into the given accumulator *)
  
let select_values filter_acc signa_item = 
  match signa_item with
    | Osig_value (name,ty,lis) -> (name,ty,lis)::filter_acc
    | _                        -> filter_acc
	
(** This function select types from a signature.
    If the given signature item is an "Osig_type", it
    adds the type's name and the type's representation into 
    the given accumulator *)
	
let select_types filter_acc signa_item= 
  match signa_item with
    | Osig_type ( (ty_name,_,ty,_,_) , _ ) -> (ty_name,ty)::filter_acc
    | _ -> filter_acc
	
	
(** *)
let build_arborescence filter_fun out_mod_ty = 
  let get_all  = function
    | Omty_abstract -> []
    | Omty_ident out_id -> []
    | Omty_signature sg -> 
	List.rev (List.fold_left filter_fun [] sg)
    | Omty_functor ( s , omty1 , omty2 ) -> []
	
    in 
  let rec tree_aux lv path acc = function 
    | Omty_abstract -> acc
    | Omty_ident out_id -> acc (*It's a module type*)
    | Omty_signature sg -> 
	List.fold_left (out_type_tree (lv+1) path) acc sg
    | Omty_functor ( s , omty1 , omty2 ) -> acc
	
  and out_type_tree lv path acc p=
    match p with
      | Osig_class _ -> acc
      | Osig_class_type _ -> acc
      | Osig_exception _ -> acc
      | Osig_type(o_tp_d,_) -> acc
      | Osig_value (s, o_tp, _)  -> acc
      | Osig_modtype _ -> acc
      | Osig_module (mod_nm, out_mod_tp,_) -> 
	  let c_lis = get_all out_mod_tp in
	  let line = { path = path^mod_nm ; content = c_lis ; level = lv } in
	  let acc = line::acc in 
	  tree_aux lv (path^mod_nm^".") acc out_mod_tp
  in 
  let acc = (List.rev (tree_aux 0 "" [] out_mod_ty)) in
  let c_lis = get_all out_mod_ty in
  if c_lis <> [] then 
    { path = "" ; content = c_lis ; level = 0 } ::acc
  else acc
    
    
(**  *)
 let apply_filter filter_fun ci = 
(*
  let signa =   get_signature (ci.fb_name^".cmi") in
  let gst_mod = get_module "Ocamlwizard_Ghost_Module" signa in
*)
   let signa = Printtyp.tree_of_signature ci in
   let gst_mod = Omty_signature signa in
  build_arborescence filter_fun gst_mod
    
(**  *)
let module_tree = apply_filter select_none
  
(**  *)
let values_tree = apply_filter select_values
  
(**  *)
let types_tree  = apply_filter select_types
  
(*---------------------------------------------------------------------*)
  
(** This function extract modules tree from the Ocamlwizard_Ghost_Module
    and build a list of modules_info from it *)
let mk_modules  ce = 
  let modules_info mods_tree = 
    let mods_inf = 
      List.fold_left 
	(fun acc md ->
	  { m_name  = md.path ; 
	    m_miss  = "" ; 
	    m_level = md.level 
	  }::acc
	) [] mods_tree 
    in List.sort (fun a b -> compare a.m_name b.m_name) mods_inf in 
  modules_info (module_tree ce)


(** This function extract values tree from the Ocamlwizard_Ghost_Module
    and build a list of values_info from it
*)
  
let mk_values ce se = 
  let vals_info_in_mod  acc md_ctt =
    List.fold_left
      (fun ac (nm,ty,lis) ->
	let nm =if md_ctt.path ="" then nm else md_ctt.path ^ "." ^ nm in 
	{ vl_name     = nm;
	  vl_miss     = "";
	  vl_level    = md_ctt.level;
	  vl_type     = ty;
	  vl_affect   = Tnone;
	  vl_fpat     = true;
	  vl_ftype    = true;
	  vl_kind     = V_all
	}::ac
      ) acc md_ctt.content
  in 
  let values_info vals_tree = 
    let vals_inf = List.fold_left vals_info_in_mod [] vals_tree in
    List.sort (
	fun a b -> 
	  let c = compare a.vl_level b.vl_level in
	  if c <> 0 then c
	  else compare a.vl_name b.vl_name
      ) vals_inf
  in
  let values = values_info (values_tree ce) in
  debugln "Found %d values:" (List.length values);
  List.iter
    (function v ->
      debugln "  - %s" v.vl_name)
    values;
  values

(** *)
let mk_record_info lv acc (rc_name,lbls)=
  let mk_label_info = List.fold_left (
      fun acc (s,b,otl) ->
	{ l_name   = s;
	  l_mut    = b;
	  l_miss   = "";
	  l_type   = otl;
	  l_affect = Tnone;
	  l_fpat   = true;
	  l_ftype  = true;
	  l_kind   = V_all;
	}::acc
    ) [] 
  in 
  { r_name   = rc_name;
    r_labels = mk_label_info lbls;
    r_level  = lv;
  }::acc
    

(** *)
let mk_records ce se pi ty = function
  | Fdummy -> unreachable "Path_extraction" 1
  | _      ->
      let path_ty, typ = Cmireader.get_main_type ce se ([],ty) false in
      match typ with
	| Otyp_record fl -> mk_record_info 0 [] (path_ty,fl)
	| Otyp_var _     -> [] (* require scope analysis *)
	| _              ->  unreachable "Path_extraction" 2
