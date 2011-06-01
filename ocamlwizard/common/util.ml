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

open Longident
open Outcometree
open Format
open Parsetree

let get_c_num loc = 
  loc.Location.loc_start.Lexing.pos_cnum,
  loc.Location.loc_end.Lexing.pos_cnum

(** *)
let is_capitalize s = let c = String.sub s 0 1 in c = String.uppercase c
      
(** *)
let module_name str_l =
  let rec f_aux acc = function
    | []   -> acc
    | [e]  -> if is_capitalize e then sprintf "%s.%s" acc e else acc
    | s1::s2::r -> f_aux (sprintf "%s.%s" acc s1) (s2::r)
  in 
  if str_l <> [] then f_aux (List.hd str_l)(List.tl str_l)
  else ""
    
(** *)
let concat_string_list str_l =
  List.fold_left 
    (fun ac s -> sprintf "%s.%s" ac s) (List.hd str_l)(List.tl str_l)
  
(** *)
let flatten oid = 
  let rec flat accu = function
    | Oide_ident s -> s :: accu
    | Oide_dot(lid, s) -> flat (s :: accu) lid
    | Oide_apply(l1, l2) -> assert false
  in flat [] oid

let rec add_qualif md = function
  | Lident s -> Ldot (md, s)
  | Ldot(lid, s) -> Ldot (add_qualif md lid, s)
  | Lapply(l1, l2) -> assert false

(** *)
let max_depth lis default = 
  if lis <> [] then List.fold_left max (List.hd lis)(List.tl lis)
  else default
  
(** *)
let pattern_depth pat =  (* THIS FUNCTION MUST BE CHECKED *)
  let rec p_depth d pat = 
    match pat.ppat_desc with
      | Ppat_any -> d
      | Ppat_var _ -> d
      | Ppat_constant _ -> d + 1
      | Ppat_construct (_, None ,_) -> d + 1
        
      | Ppat_construct (lid, Some p' ,_) -> 
	  if lid <> Lident "::" then p_depth (d + 1) p' 
	  else p_depth d  p'

      | Ppat_variant (_,None) ->  d + 1
      | Ppat_variant (_,Some p') ->  p_depth (d + 1) p' 
	
	  
      | Ppat_tuple p_lis -> 
	  let d = d + 1 in max_depth (List.map (p_depth d)p_lis) d
	  
      | Ppat_record (lis, _) -> 
	  let d = d + 1 in
	  max_depth (List.map (p_depth d) (List.map snd lis)) d
	
      | Ppat_or (p1,p2) -> max (p_depth d p1) (p_depth d p2) 
	
      | Ppat_array p_lis ->  
	  let d = d + 1 in
	  max_depth (List.map (p_depth d) p_lis) d
  
      (* TO CHECK *)
      | Ppat_alias (p, _) -> p_depth d p 
      
      | Ppat_constraint _  -> assert false
      | Ppat_type ty  ->  assert false
  
  in p_depth 0 pat 

(** *)
let max_patterns_depth lis_pat = 
  max_depth (List.map pattern_depth lis_pat) 1


(** *)
let rec lid_head = function
  | Lident id             -> id
  | Ldot (qualid_p , id)  -> id
  | Lapply (l1, l2)       -> lid_head l2



let type_path = function
  | Otyp_constr (oid,otl) -> 
      let lis = flatten oid in
      let path_rev = List.tl (List.rev lis) in
      List.rev path_rev
  | _ ->  []


let lid_to_str lid = 
  concat_string_list (Longident.flatten lid)


(*** ***)

let rec add_qualif_oid md = function
  | Oide_ident s -> Oide_dot (md, s)
  | Oide_dot(lid, s) -> Oide_dot (add_qualif_oid md lid, s)
  | Oide_apply(l1, l2) -> assert false
      
(** *)
let flatten oid = 
  let rec flat accu = function
    | Oide_ident s -> s :: accu
    | Oide_dot(lid, s) -> flat (s :: accu) lid
    | Oide_apply(l1, l2) -> assert false
  in flat [] oid


let qualif_from_oid oid  = function
  | Ppat_var s -> 
      begin
	match (flatten oid) with
	    [] -> Ppat_var s
	  | e ->
	      let md_name = List.rev (List.tl (List.rev e)) in
	      if md_name <> [] then
		let md_name = module_name md_name in 
		Ppat_var (sprintf "%s.%s" md_name s) 
	      else Ppat_var s
      end
  | p_desc -> p_desc
      
let is_list (lid:Longident.t) = match lid with
  | Lident ("[]"|"(hd :: tl)") -> true
  | _ -> false


(** Module Lpp (Light patterns printer) *)
module Lpp = struct
  open Format
  open Asttypes
    
  let print_constant fmt = function
    | Const_int (i) -> fprintf fmt "%d" i
    | Const_char (c) -> fprintf fmt "%02x" (Char.code c)
    | Const_string (s) -> fprintf fmt "%S" s
    | Const_float (s) -> fprintf fmt "%s" s
    | Const_int32 (i) -> fprintf fmt "%ld" i
    | Const_int64 (i) -> fprintf fmt "%Ld" i
    | Const_nativeint (i) -> fprintf fmt "%nd" i
	
  let rec print_lid fmt = function
    | Lident s    -> fprintf fmt "%s" s
    | Ldot (t, s)   -> fprintf fmt "%a.%s" print_lid t s 
    | Lapply (t, t2) -> fprintf fmt "%a %a" print_lid t print_lid t2
	
  let rec print_record fmt lis =
    let (hd_lid,hd_pat),tl = List.hd lis, List.tl lis in
    fprintf fmt "%a = %a" print_lid hd_lid print_pattern hd_pat.ppat_desc;
    List.iter (
	fun (lid,pat) -> fprintf fmt "; %a = %a" print_lid lid print_pattern pat.ppat_desc
      ) tl
      
  and print_array fmt lis = 
    List.iter (fun pat -> fprintf fmt "%a;" print_pattern pat.ppat_desc ) lis
      
  and print_pattern fmt = function
    | Ppat_any                         -> fprintf fmt "_"
    | Ppat_var s                       -> fprintf fmt "%s" s
    | Ppat_alias (pat, s)              -> fprintf fmt "%a = %s" print_pattern pat.ppat_desc s 
    | Ppat_constant c                  -> fprintf fmt "%a" print_constant c
    | Ppat_tuple pat_lis               -> 
	begin
	  match pat_lis with
	    | [] | [_]-> Debug.unreachable "Lpp" 1
	    | p :: r ->
		fprintf fmt "(%a" print_pattern p.ppat_desc;
		List.iter (fun p -> fprintf fmt ", %a" print_pattern p.ppat_desc ) r;
		fprintf fmt ")" 
	end
    | Ppat_construct (lid, pat_opt, b) -> 
	begin
	  match pat_opt with
	    | None -> fprintf fmt "%a" print_lid lid
	    | Some p -> 
		fprintf fmt "%a %a" print_lid lid print_pattern p.ppat_desc
	end
	  
    | Ppat_variant (lbl, pat_opt)    ->
	begin
	  match pat_opt with
	    | None -> fprintf fmt "`%s" lbl
	    | Some p -> 
		fprintf fmt "`%s %a" lbl print_pattern p.ppat_desc
	end
    | Ppat_record (c_lis, _)           -> fprintf fmt "{ %a }" print_record c_lis
    | Ppat_array pat_lis               -> fprintf fmt "[%a]" print_array pat_lis
    | Ppat_or (pat1, pat2)             -> 
	fprintf fmt "%a | %a" print_pattern pat1.ppat_desc print_pattern pat2.ppat_desc
    | Ppat_constraint (pat, co)        -> assert false
    | Ppat_type lid                    ->fprintf fmt "%a" print_lid lid
	
end
  
