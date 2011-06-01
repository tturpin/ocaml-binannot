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
open Outcometree
open Format
open Interface
open Util
open Stdr_types
open Parsetree

(** Takes [[One;Two;...][A;B;...]] and realizes the cartesian product 
    to return [[One;A][One; B][Two;A][Two; B]] *)
  
let rec cartesian_set  = function 
  | []   -> []
  | [l]  -> List.map (fun elt -> [elt]) l
  | el::rll -> 
      (*We want to return [[One;A][One; B]]@[[Two;A][Two; B]]*)
      List.concat(List.map (fun e_acc ->
	List.map 
	  (fun elt -> elt::e_acc)el)(cartesian_set rll))
	
	
(** 
    Takes [[One;Two;...][A;B;...]] and realizes the cartesian product 
    to return [[One;A][One; B][Two;A][Two; B]] in the out_type ast
*)
let rec cartesian_product  = function 
  | []   -> []
  | [l]  -> List.map (fun elt -> [elt]) l
  | el::rll -> 
      (*We want to return [[One;A][One; B]]@[[Two;A][Two; B]]*)
      List.concat 
	(List.map (fun e_acc ->
	  List.map 
	    (fun elt -> elt::e_acc)el)(cartesian_product rll))

(**   *)
let rec map_ty max ce se p_ty is_pers l  = 
  List.map (build_match_comp max ce se p_ty is_pers) l

and build_match_comp max ce se p_ty is_pers ty =
  if !Common_config.debug then
    begin
      Format.eprintf "> Pattern matching's type : [";
      !Oprint.out_type Format.err_formatter ty;
      Format.eprintf "]@.";
    end;
  if max <= 0 then ty else
    let max = max - 1 in
    match ty with 
      | Otyp_sum lis ->
	  Otyp_sum ((List.map (fun (s,t_lis) ->
	    s, map_ty max ce se p_ty is_pers t_lis ) ) lis)
	      
      | Otyp_constr (oid,ot_l) ->
	  let max = max + 1 in
	  let ot_l = map_ty max ce se p_ty is_pers ot_l in
	  let ld = flatten oid in
	  begin
	    match ld with
	      | ["option"] -> mk_option_sum ot_l  
	      | ["list"]   -> mk_option_list ot_l  
	      | [ld2] when is_basic ld2 -> Otyp_constr(oid,ot_l)
	      | _ -> 
		  begin
		    try
		      try
			let _, ty2 = Cmireader.values_of_type (p_ty@ld) ce se is_pers in
			let p_ty2 = List.rev (List.tl (List.rev ld)) in
			build_match_comp max ce se (p_ty@p_ty2) is_pers ty2
		      with _ ->
			let _, ty2 = Cmireader.values_of_type ld ce se is_pers in
			let p_ty2 = List.rev (List.tl (List.rev ld)) in
			build_match_comp max ce se p_ty2 is_pers ty2
			  
		    with _ as e -> raise e
		  end
	  end
	    
      | Otyp_tuple out_l ->
	  Otyp_tuple (map_ty max ce se p_ty is_pers out_l)
	    
      | Otyp_record fl  -> 
	  Otyp_record(List.map (fun (s,b,ot) ->(s,b,build_match_comp max ce se p_ty is_pers ot))fl)
	
      | Otyp_manifest (t1, _) -> 
	  build_match_comp (max + 1) ce se p_ty is_pers t1

      | Otyp_var _ | Otyp_alias _  | Otyp_poly _ | Otyp_variant _
      | Otyp_stuff _ | Otyp_object _ | Otyp_class _ | Otyp_arrow _
      | Otyp_abstract -> ty

(** *)
let create_var ty = 
  let prefix = match ty with
    | Otyp_abstract   -> "abstr"
    | Otyp_constr (oid, otl)   -> 
	let ty_name = List.hd (List.rev (flatten oid)) in
	if is_basic ty_name then find_basic ty_name
	else (try String.sub ty_name 0 3 with _ -> ty_name)
    | Otyp_manifest _ -> "e"
    | Otyp_object _   -> "objt"
    | Otyp_record _   -> "rcd"
    | Otyp_stuff _    -> "stf"
    | Otyp_sum   _    -> "sum"
    | Otyp_tuple _    -> "tpl"
    | Otyp_var   _    -> "var"
    | Otyp_variant _  -> "vat"
    | Otyp_poly _     -> "ply"
    | Otyp_alias _    -> "alia"
    | Otyp_arrow _    -> "arr"
    | Otyp_class _    -> "cls"
  in Ppat_var prefix
 
  
(* renvoie un pattern *)
let mk_pattern desc = { ppat_desc=desc;ppat_loc=Location.none }
  
(* MOVING THIS OPEN MAY CAUSE A CAPTURE	*)
open Longident

let rec build_pattern_list max ty = 
  if max <= 0 then [ mk_pattern (create_var ty) ]
  else 
    let max = max - 1 in
    match ty with
      | Otyp_constr (oid,otl) -> 
	  if oid = Oide_ident "bool" then mk_bool_pats ()
	  else 
	    let var = create_var ty in
	    let qualid  = qualif_from_oid oid var in
	    [ mk_pattern qualid ] 
	      
      | Otyp_tuple otl -> 
	  let lp = List.map (build_pattern_list max) otl in
	  let cp = cartesian_product lp                  in
	  List.map (fun p -> mk_pattern (Ppat_tuple p)) cp
	    
      | Otyp_sum lis -> 
	  List.fold_right (
	      fun (s,otl) acc  ->
		match List.map (build_pattern_list max) otl with
		  | [] -> (mk_pattern (Ppat_construct((Lident s),None, false)))::acc
		  | pll ->
		      let pl = cartesian_product pll in
		      List.fold_right 
			(fun e ac -> 
			  
			  let tpl = 
			    match e with 
			      | [] -> Debug.unreachable "Match-extraction" 1
			      | [e]-> e
			      | _  -> mk_pattern (Ppat_tuple e) 
			  in
			  (mk_pattern (Ppat_construct((Lident s),Some tpl , false)))::ac
			) pl acc 
	    ) lis [] 
	    
      | Otyp_record l -> 
	  let l1 = List.map (fun (s,b,t) -> s) l in
	  let l2 = List.map (fun (s,b,t) -> t) l in
	  let tmp = 
	    List.map (
		fun p ->
		  build_pattern_list max p
	      ) l2
	  in
	  let l3 = cartesian_product tmp in
	  let pat_lis = 
	    List.fold_left (
		fun acc e3 ->
		  let cpl_lst = 
		    List.fold_left2 (
			fun ac p c->
			  (Lident p,c)::ac
		      ) [] l1 e3
		  in (mk_pattern (Ppat_record (List.rev cpl_lst, Asttypes.Closed))) ::acc
	      ) [] l3
	  in 
	  (List.rev pat_lis)
	    
      | Otyp_abstract -> [ mk_pattern (create_var ty) ]
      | Otyp_var (b, s) -> [ mk_pattern (create_var ty) ]
      | Otyp_manifest (sum, sum1) -> assert false
      | _ -> [ mk_pattern (create_var ty) ]
	  
	  
let index_variables fct tbl pat = 
  (* tbl binds each variable with its next free index *)
  let rec index_vars pat = 
    { pat with ppat_desc = 
	match pat.ppat_desc with
	  | Ppat_var v ->
	      begin
		try
		  let i = Hashtbl.find tbl v in
		  let i = fct v i            in
		  Hashtbl.replace tbl v (i + 1);
		  Ppat_var (sprintf "%s%d" v i)
		with Not_found ->
		  let i = fct v 1 in
		  Hashtbl.add tbl v (i + 1); 
		  if i = 1 then Ppat_var v 
		  else Ppat_var (sprintf "%s%d" v i)
	      end
	  | Ppat_alias (p ,v) -> Ppat_alias (index_vars p, v)
	  | Ppat_tuple pl ->  Ppat_tuple (List.map index_vars pl)
	      
	  | Ppat_record (lp, closed) ->	  
	      let lp = List.map (fun (nm,p) -> (nm, index_vars p)) lp in
	      Ppat_record (lp, closed)
		
	  | Ppat_or (p1, p2) ->
	      Ppat_or (index_vars p1 , index_vars p2)
		
	  | Ppat_array pl -> Ppat_array (List.map index_vars pl)
	      
	  | Ppat_construct (lg, Some p ,b) -> 
	      Ppat_construct (lg, Some (index_vars p) ,b)
		
	  | Ppat_constraint (p, ct) -> 
	      Ppat_constraint (index_vars p , ct)
		
	  | Ppat_variant (lbl, Some p) ->
	      Ppat_variant (lbl , Some (index_vars p))
					    
	  | Ppat_construct (_, None ,_)  
	  | Ppat_variant (_, None) 
	  | Ppat_type _ 
	  | Ppat_any 
	  | Ppat_constant _ -> pat.ppat_desc 
    }
  in index_vars pat  
      
let check_unicity (env, _) var i =  
  let rec next_free env var i = 
    try 
      let lid = if i = 1 then Lident var else Lident (sprintf "%s%d" var i) in
      let _ = Env.lookup_value lid env in
      next_free env var (i + 1)
    with Not_found -> i
  in next_free env var i
	
let extract_match_cases max ty ty_check = 
  if !Common_config.debug then
    Format.eprintf "> Extract match cases : %s @." "[With Env]";
 let rec f cpt acc = 
    if cpt = 0 then acc
    else 
      let pats = build_pattern_list cpt ty in
      let tbl = Hashtbl.create 13 in
      let res = List.map (
	  fun pat ->
	    Hashtbl.clear tbl;
	    index_variables (check_unicity ty_check) tbl pat
	) pats
      in f (cpt - 1) ((cpt,res) :: acc)
  in
  f max []
    
