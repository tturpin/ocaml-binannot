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

open Outcometree
open Format 
open Asttypes
open Interface
open Util 
open Stdr_types
open Debug


exception Ty_not_found of int
exception Md_not_found

(** Returns Outcometree Signature of a given file *)
let get_signature path =
  let modname = 
    String.capitalize(Filename.basename(Filename.chop_extension path))
  in
  let signa = Env.read_signature modname path in 
  Printtyp.tree_of_signature signa

(** Searchs type by name in given signature
    @raise Ty_not_found if list is empty *)
let rec get_type elm_tp = function
  | [] -> raise (Ty_not_found 1)
  | (Osig_type ((id,_,out_t,_,_),rc))::_ when id = elm_tp -> out_t
  | (Osig_type ((id,_,out_t,_,_),rc))::_ when id = elm_tp -> out_t
  | _::r -> get_type elm_tp r
      
(** Searchs module by name in given signature
    @raise Md_not_found if list is empty *)
let rec get_module elm_md = function
  | [] -> raise Md_not_found
  | ((Osig_modtype (nm, sg)| Osig_module (nm, sg,_)))::r when nm = elm_md -> sg
  | _::r -> get_module elm_md r
      
(** Searchs internal reprsentation of type t (formatted by [Module1|Module2|...|ModuleX|typname])
    in tree signature.
    In first step, it searchs Module1 then Module2 in Module1's Signature,... and finally searchs typname in ModuleX' signature.
    @raise Ty_not_found if list is empty*)
let rec find_type_infos tree_sg =  function
  | [] -> raise (Ty_not_found 2)
  |[elm_tp] -> get_type elm_tp tree_sg 
  | md::t_lg -> 
      let sg = get_module md tree_sg in 
      begin
	match sg with 
	  | Omty_signature sg_l -> find_type_infos sg_l t_lg
	  | _ -> raise (Ty_not_found 3)
      end

(** Searchs type: t in visible module: md of directories: dirs
    @raise Ty_not_found if list is empty *)
let rec find_in_dirs md t dirs =
  match dirs with 
    |[] -> raise (Ty_not_found 4)
    | e::r -> 
	try  
	  let cmi_fl = (String.uncapitalize md)^".cmi" in
	  let modpath = Filename.concat e cmi_fl in
	  (t, find_type_infos (get_signature modpath) t)
	with 
	  | Ty_not_found _ | Md_not_found  as e -> raise e
	  | Sys_error _ -> find_in_dirs md t r
	
(** *)
let rec find_in_current_file sg ty sub_mds = 
  match sub_mds with 
    | []     ->  ty, find_type_infos sg ty
    | hd::tl ->
	try ty, find_type_infos sg ((List.rev sub_mds)@ty)
	with _ -> find_in_current_file sg ty tl
	
(** Searchs t in current file then in opened directories by path
    @raise Ty_not_found if type t is not visible*)
let values_of_type t ce se is_persistent =
  if is_persistent then
    begin
      match t with
	| [] | [_] -> 
	    if !Common_config.debug then 
	      Format.eprintf "Value-of-type 11 ->%s@." (Util.concat_string_list t);
	    unreachable "Cmireader" 11
	| md::t_lg -> find_in_dirs md t_lg ce.includ
    end
  else
    try 
      let curr_sg = get_signature (ce.fb_name ^ ".cmi") in
      find_in_current_file curr_sg t (List.rev se.mpath)
    with Ty_not_found _ | Md_not_found ->
      (* when we use type-checking in all cases this case should not
	 happen *)
      begin
	match t with
	  | [] | [_] -> 
	      if !Common_config.debug then 
		Format.eprintf "Value-of-type 12 ->%s@." (Util.concat_string_list t);
	      unreachable "Cmireader" 12
	  | md::t_lg -> find_in_dirs md t_lg ce.includ
      end

(** *)
let rec get_main_type ce se (ty_path,ty) is_persistent = 
  match ty with
    | Otyp_constr (oid,ot_l) -> 
	let ld = flatten oid in
	if List.length ld = 1 && is_basic (List.hd ld) then (ty_path,ty)
	else get_main_type ce se (values_of_type ld ce se is_persistent) is_persistent
	  
    | Otyp_manifest (t1,t2)  -> get_main_type ce se (ty_path,t1) is_persistent
    | _ -> (ty_path,ty)
	
