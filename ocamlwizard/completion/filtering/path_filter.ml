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
open Util
open Stdr_types
open Interface
	
(** *)
let rec get_meta_type = function
  | Oide_apply(o1, o2) -> get_meta_type o1
  | Oide_dot(o1, s)    -> get_meta_type o1
  | Oide_ident "ref"   -> Tref
  | Oide_ident "unit"  -> Tvoid
  | Oide_ident _       -> Tnone

(** 
    ty_wanted : type read from .annot 
    ty_exp    : type of the current value *)    
      
let rec filter_by_type ty_wanted ty_exp =
  match ty_wanted with
    | Otyp_var _ -> 
	begin
	  match ty_exp with
	    | Otyp_constr (oid,_) -> true, (get_meta_type oid), V_all
	    | _                   -> true, Tnone, V_all
	end
    | Otyp_arrow (s,t1,t2)    -> true, Tnone, V_all 
    | _ ->
	begin
	  match ty_exp with
	    | Otyp_var _              -> true, Tnone, V_all
	    | Otyp_constr (oid,otl)  -> 
		(ty_wanted = ty_exp), (get_meta_type oid), V_all
		  
	    | Otyp_arrow (s,t1,t2) -> filter_by_type ty_wanted t2
	    | Otyp_record _        -> ty_wanted = ty_exp, Tnone, V_record
	    | _                    -> ty_wanted = ty_exp, Tnone, V_all
	end
	  
(** This function slices a string on two parts
    split_str (s,pos) = ( part1 , part2 )
    with part1 = s[0];s[1];..s[pos-1]
    and  part2 = s[pos];s[pos+1];..s[n] *)
	  
let split_str str pos = 
  let len = String.length str in
  try ( String.sub str 0 pos , String.sub str pos (len - pos) )
  with _ -> ("",str)
    
(** This function computes 
    (part1,part2) = split_str lbl_nm (length pat)
    and returns (part2, true) if and only if part1 = pat 
    (i.e. lbl_nm begins with pat),It returns ("",false) otherwise *)
    
let filter_by_pattern lbl_nm pat =
  debugln "filtering %s by pattern %s" lbl_nm pat;
  let ( hd_str , tl_str ) = split_str lbl_nm (String.length pat) in
  if hd_str = pat then (tl_str,true) else ("",false)
    
(** This function filter the modules by pattern *)
let filter_modules pat mds_lst =
  let lis = List.map 
    (fun md_i ->
      let miss,_ = filter_by_pattern md_i.m_name pat in
      { md_i with m_miss = miss ^ " " }
    )mds_lst
  in List.sort (fun a b -> compare a.m_name b.m_name) lis

(** vl_inf : value info 
    ty_wanted : type searched *)
let filter_values ty_wanted pat vls_lst v_kd =
  debugln "filtering %d values" (List.length vls_lst);
  let f_sort a b =
    let c = compare a.vl_level b.vl_level in
    if c <> 0 then c else compare a.vl_name b.vl_name in 
  let f_filter vl_inf =
    let miss, s_pat            = filter_by_pattern vl_inf.vl_name pat in
    let s_type, t_affect, kind = filter_by_type ty_wanted vl_inf.vl_type in
    debugln "fpat = %B, miss = %s, ftype = %B" (*", affect = %B, kind = %B"*)
      s_pat miss s_type (*t_affect kind*);
    { vl_inf with
      vl_miss     = miss ^ " ";
      vl_fpat     = s_pat;       (* filtering by pattern   *)
      vl_ftype    = s_type;      (* filtering by type      *)
      vl_affect   = t_affect;    (* filtering by meta type *)
      vl_kind     = kind         (* filtering by context   *)
    }
  in List.sort f_sort (List.map f_filter vls_lst)

(** *)
let label_is_given lbl_name = function
  | Fdummy -> Debug.unreachable "Path_filter" 1
  | Faccess _ -> false
  | Fdef gvn | Fpat gvn ->
      try
	List.iter (fun lid -> if lid_head lid = lbl_name then raise Exit) gvn;
	false
      with _ -> true

(** *)
let filter_labels pat r_kd lbl = 
  let (miss, success ) = filter_by_pattern lbl.l_name pat in
  
  if not success || label_is_given lbl.l_name r_kd then 
    { lbl with
      l_miss  = "";
      l_fpat  = false;
    }
  else
    { lbl with
      l_miss  = miss;
      l_fpat  = true;
    }
   
(** For now , we only filter by pattern *)
let filter_records ty_wanted pat r_inf r_kd = 
  let filter = filter_labels pat r_kd in
  List.map 
    (fun r_item -> 
      { r_item with 
	r_labels = List.map filter r_item.r_labels }
    )
    r_inf
  
