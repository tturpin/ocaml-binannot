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

(** *)
open Interface
open Parsetree
open Format

module L = List
module F = Format

exception Exist
exception ProgressMore

(* True if one if one is a suffix of the other. Since this is the same
   type, t works ! *)
let constr_include c1 c2 = 
  let user_lg = L.rev (Longident.flatten c1) in
  let best_lg = L.rev (Longident.flatten c2) in
  try
    L.iter2 (fun s1 s2 ->if s1 <> s2 then raise Exit) user_lg best_lg;
    true
  with  
      Exit               -> false 
    | Invalid_argument _ -> true
      
let given_is_include special_behv gvn my_p = 
  let rec given_is_include gvn my_p = 
    match gvn.ppat_desc,my_p.ppat_desc with 
	
      (* cas de recursion *)
      | Ppat_tuple gvl, Ppat_tuple mypl -> tuple gvl mypl
      | Ppat_construct (a, x ,_),Ppat_construct (b,y,_) -> construct a b x y
      | Ppat_record (lg, _), Ppat_record (lp, _) -> record lg lp
	  
      (* cas des constantes a generaliser *)
      | Ppat_constant _, (Ppat_any | Ppat_var _) -> false
      | Ppat_construct _ , (Ppat_var _ | Ppat_any) -> true
      | Ppat_record _, (Ppat_any | Ppat_var _) -> true
	  
	  
      (* pour Ppat_tuple la fonction a un comportement special *)
      | Ppat_tuple _, (Ppat_any | Ppat_var _) -> special_behv ()
 	  
      (* Ppat_alias et Ppat_or a deconstruire *)
      | Ppat_alias (gv_2, _),_ -> given_is_include gv_2 my_p
	  
      | Ppat_or (gv_2,gv_3), _ -> 
	  let res1 = given_is_include gv_2 my_p 
	  and res2 = given_is_include gv_3 my_p in res1 || res2 
						
      (* Ppat_any et Ppat_var couvrent tout *)
      | Ppat_any , _ -> true
      | Ppat_var _ , _ -> true
	  
      (* dans tout les autres cas, il n'y a pas inclusion *)
      | _ -> false 
	  
	  
  and tuple gvl mypl =
    let acc = L.fold_left2
      (fun acc gv myp -> (given_is_include gv myp) :: acc) [] gvl mypl
    in not (L.mem false acc)
    
  and construct c1 c2 arg1 arg2 = 
    let mem_cst = constr_include c1 c2 in
    match arg1, arg2 with
      | None, None       -> mem_cst
      | Some e1, Some e2 -> mem_cst && (given_is_include e1 e2)
      |  _               -> false
	   
  and record lg lp =
    if !Common_config.debug then begin
	F.eprintf "> Match-filter.record on List.fold_left2 - entry : ";
	F.eprintf "|given-lg| = %d and |extracted-lp| = %d@."
	  (L.length lg)(L.length lp)
      end;
    let lg = L.map (fun (nm,ty) -> (Util.lid_head nm,ty)) lg in
    let lp = L.map (fun (nm,ty) -> (Util.lid_head nm,ty)) lp in
    
    let f_cmp (nm1, _) (nm2, _) = compare nm1 nm2 in 
    let normalize lnorm = L.filter (fun (nm,_) -> L.mem_assoc nm lnorm) in
    let lp = L.sort f_cmp (normalize lg lp)in
    let lg = L.sort f_cmp (normalize lp lg) in
    if !Common_config.debug then begin
	F.eprintf "> Matchfilter.record on List.fold_left2 -normalized: ";
	F.eprintf "|given-lg| = %d and |extracted-lp| = %d@."
	  (L.length lg) (L.length lp)
      end;
 
    let acc = 
      L.fold_left2
	(fun acc (_,e1) (_,e2) -> given_is_include e1 e2 :: acc ) [] lg lp
    in 
    L.iter (F.eprintf "%b ;") acc;
    F.eprintf "@.";
    not (L.mem false acc)
  in
  given_is_include gvn my_p
	
let alike_is_given my_pat givens = 
  let beh_exit () = raise ProgressMore in
  try
    L.iter 
      (fun given ->
	if given_is_include beh_exit given my_pat then
	  raise Exist
      )givens;
    false
  with 
    | Exist -> true
    | ProgressMore  -> true
	
let alike_is_stored my_pat acc = 
  let beh_false () = false in
  try
    L.iter 
      (fun stored ->
	if given_is_include beh_false my_pat stored then 
	  raise Exist	
      ) acc;
    false
  with 
    | Exist -> true


(** *)
let build_missing_cases gvn_pl mi_lis = 
  let check_for_missing acc lcases gvn_pl = 
    L.fold_left
      (fun acc p ->
	if alike_is_given p gvn_pl  || alike_is_stored p acc then acc 
	else p :: acc	
      ) acc lcases 
  in
  L.fold_left 
    (fun acc (_, lcases) -> check_for_missing acc lcases gvn_pl ) [] mi_lis 

let mk_mpat bol p = 
  {ma_pattern  = p;
   ma_level    = 1;
   ma_selected = bol;
   ma_vars     = [] }


let rec make_projection acc gvn exp_p = 
  let s, e = !Common_config.expand_loc in
  let ls,le = Util.get_c_num exp_p.ppat_loc in
  if ls = s && le = e then gvn :: acc
  else
    match gvn.ppat_desc,exp_p.ppat_desc with 
      | Ppat_construct (lg1, ar1, b1), Ppat_construct (lg2, ar2, b2) ->
	  let cond = constr_include lg1 lg2 in begin
	      match ar1, ar2 with
		| Some gv, Some exp when cond -> make_projection acc gv exp
		|  _   -> acc
	    end
					    
      | Ppat_tuple l1, Ppat_tuple l2  | Ppat_array l1, Ppat_array l2 -> 
	  L.fold_left2 make_projection acc l1 l2
	  
      | Ppat_record (lg, _), Ppat_record (lp, _) -> 
	  let lg = L.map (fun (nm,ty) -> (Util.lid_head nm,ty)) lg in
	  let lp = L.map (fun (nm,ty) -> (Util.lid_head nm,ty)) lp in
	  let f_cmp (nm1, _) (nm2, _) = compare nm1 nm2 in 
	  let normalize lnrm = L.filter (fun (nm,_) -> L.mem_assoc nm lnrm) in
	  let lp = L.sort f_cmp (normalize lg lp)in
	  let lg = L.sort f_cmp (normalize lp lg) in
	  L.fold_left2 make_projection acc (L.map snd lp) (L.map snd lg)
	
      | Ppat_constraint (p1,_) , Ppat_constraint (p2,_) ->
	  make_projection acc p1 p2
    
      (* CHECK these two cases *)
      | Ppat_alias (pat1, s1), Ppat_alias (pat2, s2) -> assert false
      | Ppat_or (p11, p12), Ppat_or (p21, p22)-> assert false
      
      | Ppat_any, Ppat_any
      | Ppat_constant _, Ppat_constant _
      | Ppat_var _, Ppat_var _ 
      | Ppat_type _ , Ppat_type _ 
      | Ppat_variant _, Ppat_variant _ 
      |_ -> acc
          
	  
      (*
           
      | _ , Ppat_or (p1, p2) ->
	  let res1 = extract_expansion pat p1 in
	  if res1 <> None then res1
	  else extract_expansion pat p2
	    
      | _ , Ppat_alias (p ,_) -> extract_expansion pat p
      *)

let internal_patterns exp_p = 
  L.fold_left (fun acc gvn -> make_projection acc gvn exp_p) []
    
let map_internal_patterns exp_p mi_lis = 
  let depth_pat  =  Util.pattern_depth exp_p in
  let proj = L.map (fun (n,pl) -> n, internal_patterns exp_p pl) mi_lis in
  List.filter (fun e -> fst e > depth_pat) proj

(*++ begin : For debug ++*)

let iter_pl = 
  List.iter 
    (fun e ->
      F.eprintf "\t +[";
      Util.Lpp.print_pattern F.err_formatter e.ppat_desc;
      F.eprintf "]@." )

let out_pl msg lis = F.eprintf "> %s :@." msg; iter_pl lis

let out_pl2 msg lis = 
  F.eprintf "> %s :@." msg;
  List.iter
    (fun (lvl,pl) -> 
      F.eprintf "   - For the level : [%d] : @." lvl;
      iter_pl pl )lis
    
(*++ end For debug ++*)


let filter_redencency pl = 
  let pl = 
    L.fold_left(fun acc e -> if L.mem e acc then acc else e::acc ) [] pl
  in List.rev pl

(** *)  
let filter_match mi_lis pm_comp = 
  let mi_lis = 
    match mi_lis with 
      | ME mi_lis -> mi_lis
      | MF _-> Debug.unreachable "Match-filter" 56
  in 
  match pm_comp with
    | AllCs  -> 
	let res = 
	  let depth  = !Common_config.match_depth in
	  let assoc = L.assoc 1 mi_lis in 
	  assoc
(*
	  if depth = 1 && L.length assoc <> 1 then  assoc
	  else L.assoc 2 mi_lis
*)
	in 
	if !Common_config.debug then
	  begin
	    Format.eprintf "----------------------------@.";
	    out_pl2 "Extracted patterns" mi_lis ; 
	    Format.eprintf "----------------------------@.";
	  end;
	L.map (mk_mpat true) res

    | MissCs gvn_pl -> 
	let acc = build_missing_cases gvn_pl mi_lis in
	let r1 = L.map (mk_mpat true) acc in
	let r2 = L.map (mk_mpat false) gvn_pl in
	
	if !Common_config.debug then
	  begin
	    Format.eprintf "----------------------------@.";
	    out_pl "Given patterns" gvn_pl ; 
	    Format.eprintf "----------------------------@.";
	    out_pl2 "Extracted patterns" mi_lis ; 
	    Format.eprintf "----------------------------@.";
	  end;
	filter_redencency (r1 @ r2)

    | BranchCs (to_expand, gvn_pl) ->
      match mi_lis with
	| (_, ps) :: _ ->
	  L.map (mk_mpat true) ps
(*
	if !Common_config.expand_loc = (-1,-1) then
	  failwith "< -expand int-int > is not set"
	else
	  let new_gvl    = internal_patterns to_expand gvn_pl in
	  let new_mi_lis = map_internal_patterns to_expand mi_lis in
	  
	  
	  let acc = build_missing_cases new_gvl new_mi_lis in
	  let r1 = L.map (mk_mpat true) acc in
	  let r2 = L.map (mk_mpat false) new_gvl in
	  
	  if !Common_config.debug then
	    begin
	      Format.eprintf "----------------------------@.";
	      out_pl "Given patterns" gvn_pl ; 
	      Format.eprintf "----------------------------@.";
	      out_pl "Considered given patterns" new_gvl ; 
	      Format.eprintf "----------------------------@.";
	      out_pl2 "Extracted patterns" mi_lis ; 
	      Format.eprintf "----------------------------@.";
	      out_pl2 "Considered extracted patterns" new_mi_lis ; 
	      Format.eprintf "----------------------------@.";
	    end;
	  filter_redencency (r1 @ r2)
*)	    
