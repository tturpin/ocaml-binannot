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

(**  *)

open Path_extraction
open Match_extraction
open Interface
open Format
open Debug
open Util
open Longident
open Parsetree    
open Types


(** *)
let complete_path ce se pi ty_lis = 
  match pi.p_kd, ty_lis with
    | (Module, [])        -> C_module ( mk_modules ce ,pi.p_id )
    | (Value vk, [ty])    -> C_value  ( mk_values ce se, vk, pi.p_id )
    | (Record kd, ty::_)  -> C_record ( mk_records ce se pi ty kd, kd,pi.p_id)
    | _                   -> unreachable "Proposal_extraction" 2


(** *)
let is_basic_constr = function
  | Lident "[]" -> true
  | _ -> false

(* Idea: given prefix_lis the path containing the type, add this
   prefix progressively to the constructor until it is sufficiently
   qualified for the lookup to succeed and return the right type,
   according to its declaration location.

   We need a better way of doing that, based on a unique identification
   of all elements. *)
let best_qualif pat env prefix_lis owz_inf =
(*
  let rec qualif_lid lid = function
    | [] -> 
	begin
	  try 
	    let const = Env.lookup_constructor lid env  in
	    if const.cstr_loc = owz_inf then lid else raise Exit
	  with _ -> 
	    if !Common_config.debug then
	      Format.eprintf "| [] -> lookup_constr : %s@." (Util.lid_to_str lid);
	    
	    if List.length prefix_lis > 0 && "Unix" = List.hd prefix_lis then(
		if !Common_config.debug then
		  Format.eprintf "> Special case for Unix@.";
		lid
	      )else 
	      if Util.is_list lid then
		lid
	      else unreachable "Proposal_extraction" 3
	end
    | p :: r ->
	if !Common_config.debug then
	  Format.eprintf "| p :: r -> lookup_constr : %s@." 
	    (Util.lid_to_str lid);
	try 
	  let const = Env.lookup_constructor lid env in 
	  if const.cstr_loc = owz_inf then lid else raise Exit
	with _ -> 
	  let up_qualif = add_qualif (Lident p) lid in
	  qualif_lid up_qualif r
  in 
*)
  (* Temporary solution: full qualification *)
  let qualif_lid =
    List.fold_left
      (fun lid p -> add_qualif (Lident p) lid)
  in
  match pat.ppat_desc with
    | Ppat_construct (lid, e1, e2) when lid <> Lident "[]" -> 
	{ pat with 
	  ppat_desc = 
	    let best_qualif = qualif_lid lid prefix_lis in
	    Ppat_construct (best_qualif, e1, e2)
	}
    | _ -> pat


(** *)
let best_qualification pat env_opt prefix_lis owz_inf =
  match env_opt with
    | None -> unreachable "Proposal-extraction" 10
    | Some (env, main_ty ) -> best_qualif pat env owz_inf prefix_lis


let rec type_expr_path ty_exp = 
  match ty_exp.desc with
    | Tconstr (p, _, _) -> p
    | Tlink ty_exp -> type_expr_path ty_exp 
    | _ -> raise Exit
(*  
    | Tvar -> Format.eprintf "tvar@.";
    | Tarrow _ ->Format.eprintf "tarrow@.";
    | Ttuple _ -> Format.eprintf "ttuple@.";
    | Tobject _ -> Format.eprintf "tobject@.";
    | Tfield _ -> Format.eprintf "tfield@.";
    | Tnil ->  Format.eprintf "tnil@.";
    | Tsubst _ -> Format.eprintf "tsubst@.";
    | Tvariant _ -> Format.eprintf "tvariant@.";
    | Tunivar -> Format.eprintf "tunivar@.";
    | Tpoly _ -> Format.eprintf "tpoly@."
*)

(** @return a list of pattern to complete the current match *)
let complete_match ce se ty pm_comp ty_check = 
  let (gvn_pats, depth) = 
    match pm_comp with
      | AllCs  -> 
	  let depth  = !Common_config.match_depth in
	  let depth = if depth = 1 then 2 else depth in [], depth
      | MissCs given ->
	  let depth = max_patterns_depth given in given, depth
      | BranchCs (_,given)  -> 
	  let depth = max_patterns_depth given in given, (depth +1)
  in
  if !Common_config.debug then
    Format.eprintf "> Max depth of given patterns : %d@."depth;
  let can_qualif, (*owz_inf,*) final_ty = 
    match ty_check with
      | None -> 
	  let cases = Match_extraction.build_match_comp depth ce se [] false ty  in
	  false, (*Old_types.dummy_owz_info,*) cases
      | Some (env, ty_exp) ->
	  try
	    let ty_env = Printtyp.tree_of_typexp false ty_exp in
	    let path = type_expr_path ty_exp in
(*
	    let ty_loc = (Env.find_type path env).type_loc in
*)
	    let is_pers = Ident.persistent (Path.head path)in 
	    let cases = 
	      Match_extraction.build_match_comp depth ce se [] is_pers ty_env in
	    true, (*ty_loc ,*) cases
	  with _ -> 
	    let cases = Match_extraction.build_match_comp depth ce se [] false ty in
	    false, (*Old_types.dummy_owz_info,*) cases
  in
  let type_path = List.rev (type_path ty) in
  let f g p = g p ty_check ()(*owz_inf*) type_path in 
  let pat_cases = Match_extraction.extract_match_cases depth final_ty ty_check in
  
  let pm_info =
    if can_qualif then
      List.map (fun (cpt,lcases) -> 
	(cpt, List.map (f best_qualification) lcases)) pat_cases
    else 
      let g p a b d = p in
      List.map (fun (cpt,lcases) -> (cpt, List.map (f g) lcases)) pat_cases
  in C_match (ME pm_info,pm_comp)
  
let main ce se ty_lis ty_check = 
  match se.comp, ty_lis with
    | Match pm_comp, [ty] -> complete_match ce se ty pm_comp ty_check
    | (Path  pc, _)       -> complete_path ce se pc ty_lis
    | (Try pm,  [])       -> assert false
    | _                   -> unreachable "Proposal_extraction" 4
	
