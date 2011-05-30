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
open Interface
open Asttypes
open Longident
open Parsetree
open Debug

let type_to_string ty = 
  !Oprint.out_type str_formatter ty;
  (flush_str_formatter())
  
let print_lbl_completion fmt = 
  List.iter (
    fun lbl ->
      fprintf fmt 
	"@\n<label name=\"%s\" type=\"%s\" selected=\"%b\" miss=\"%s\" mut=\"%b\" %s"
	lbl.l_name
	(type_to_string lbl.l_type)
        lbl.l_fpat
	lbl.l_miss
	lbl.l_mut
	"></label>";
    )
  
let print_rec_completion fmt = 
  List.iter (
      fun rc ->
	let lis = List.rev rc.r_name in
	let nm_ty, path_ty = List.hd lis, (List.rev (List.tl lis)) in
	let path = if(path_ty = []) then "" else (Util.module_name path_ty) in
	fprintf fmt "@\n@[<hov 1><record path=\"%s\" name=\"%s\" level=\"%d\">%a@]@\n</record>"
	  path
	  nm_ty
	  rc.r_level
	  print_lbl_completion rc.r_labels;
    )

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
	  | [] | [_]-> unreachable "xml_printer" 1
	  | p :: r ->
	      fprintf fmt "(%a" print_pattern p.ppat_desc;
	      List.iter (fun p -> fprintf fmt ", %a" print_pattern p.ppat_desc ) r;
	      fprintf fmt ")" 
      end
  | Ppat_construct (lid, pat_opt, b) -> 
      begin
	match pat_opt with
	  | None -> fprintf fmt "%a" print_lid lid
	  | Some p -> fprintf fmt "%a %a" print_lid lid print_pattern p.ppat_desc
      end

  | Ppat_variant (la, pat_option)    -> assert false
  | Ppat_record (c_lis, _)           -> fprintf fmt "{ %a }" print_record c_lis
  | Ppat_array pat_lis               -> fprintf fmt "[%a]" print_array pat_lis
  | Ppat_or (pat1, pat2)             -> 
      fprintf fmt "%a | %a" print_pattern pat1.ppat_desc print_pattern pat2.ppat_desc
  | Ppat_constraint (pat, co)        -> assert false
  | Ppat_type lid                    ->fprintf fmt "%a" print_lid lid

  
let print_match_completion fmt =
  List.iter (
    fun cs ->
      fprintf fmt "@\n<case pattern=\"%a\" level=\"%d\" selected=\"%b\"></case>"
	print_pattern cs.ma_pattern.ppat_desc
	cs.ma_level
	cs.ma_selected;
    )


(******************************************************************************
*
******************************************************************************)
let print_mod_completion fmt = 
  List.iter (
      fun md ->
	fprintf fmt "@\n<module name=\"%s\" miss=\"%s\" level=\"%d\"></module>"
	  md.m_name
	  md.m_miss
	  md.m_level   
    )

(******************************************************************************
*
******************************************************************************)
let print_taffect = function 
  | Tnone -> "other"
  | Tvoid -> "unit"
  | Tref  -> "ref"
  | Tmutable  -> "mutable"


(** Output format is : 
    "<value name=\"%s\" miss=\"%s\" affect=\"%s\" pattern_filter=\"%b\" 
    type_filter=\"%b\" is_record=\"%b\" level=\"%d\" type=\"%s\"></value>@\n"
*)
let print_value_completion fmt = List.iter (
    fun value ->
      fprintf fmt "@\n<value name=\"%s\" miss=\"%s\" affect=\"%s\" "
	value.vl_name value.vl_miss (print_taffect value.vl_affect);
      
      fprintf fmt "pattern_filter=\"%b\" type_filter=\"%b\" is_record=\"%b\" "
	value.vl_fpat value.vl_ftype (value.vl_kind = V_record);
      
      fprintf fmt "level=\"%d\" type=\"%s\"></value>"
	value.vl_level (type_to_string value.vl_type)
  )

let recup_type = function
  | AllCs -> "AllCs"
  | MissCs _ -> "MissCs"
  | BranchCs _ -> "BranchCs"
(******************************************************************************
*
******************************************************************************)
let print_expr fmt = function

  | (C_match (p,t),[ty_match]) -> 
      let p = 
	match p with 
	  | MF p -> p 
	  | ME _ -> Debug.unreachable "xml-printer" 99
      in 
      let t = recup_type t in
      fprintf fmt "@[<hov 1><match expr_type=\"%s\"  type=\"%s\">%a@.</match>@." 
	(type_to_string ty_match) t print_match_completion p 
	
  | (C_module (l,_),_ ) -> 
      fprintf fmt "@[<hov 1><modules>%a@.</modules>@." print_mod_completion l
	
  | (C_value (l,_,_),[ty_val]) -> 
      fprintf fmt "@[<hov 1><values expr_type=\"%s\" >%a@.</values>@." 
	(type_to_string ty_val) print_value_completion l


  | (C_record (l,rk,pat),[ _ ; ty_lbl]) -> 
      fprintf fmt "@[<hov 1><records expr_type=\"%s\">%a@.</records>@." 
	(type_to_string ty_lbl) print_rec_completion l

  | (C_record (l,rk,pat),[_]) -> 
      fprintf fmt "@[<hov 1><records>%a@.</records>@."
	print_rec_completion l


  | (C_error ex,_) -> raise ex
  | (C_other,_) -> fprintf fmt "No completion @?"
  | _           -> Debug.unreachable "Xml_printer" 2

let print fmt ty_lis p = 
  fprintf fmt "<?xml version=\"1.0\" encoding=\"utf-8\"?>@\n%a" print_expr (p,ty_lis)
