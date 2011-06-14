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

open Format
open Interface
open Longident
open Parsetree
open Util
module L = List

let type_to_string ty = 
  !Oprint.out_type str_formatter ty;
  (flush_str_formatter())
    
let print_lbl_completion path fmt = 
  L.iter (fun lbl ->
    if lbl.l_fpat then
      if path <> "" then fprintf fmt "%s.%s@." path lbl.l_name
      else fprintf fmt "%s@." lbl.l_name)
    
let print_rec_completion fmt = 
  L.iter (fun rc ->
    let lis = L.rev rc.r_name in
    let nm_ty, path_ty = L.hd lis, (L.rev (L.tl lis)) in
    fprintf fmt "%a" 
      (print_lbl_completion (Util.module_name path_ty))  rc.r_labels)
    

let opt_bar b_ref = 
  if !b_ref then (
      b_ref := false;
      " "
    )else "|"

let print_cases_completion bol fmt cases =
 let bol = ref bol in
 L.iter 
    (fun cs ->
      if cs.ma_selected then
	fprintf fmt "@\n %s %a -> assert false" 
	  (opt_bar bol)
	  Util.Lpp.print_pattern cs.ma_pattern.ppat_desc
    )cases
    
let print_dispatch_completion bol fmt cases =
  debugln "print_dispatch (%d cases)" (List.length cases);
  let bol = ref bol in
  L.iter
    (fun cs ->
      if cs.ma_selected then
	fprintf fmt "%s %a " 
	  (opt_bar bol)
	  Util.Lpp.print_pattern cs.ma_pattern.ppat_desc
    ) cases
    
let print_mod_completion fmt = L.iter (fun md ->fprintf fmt "@\n%s" md.m_name)

let print_taffect = function 
  | Tnone -> "other"
  | Tvoid -> "unit"
  | Tref  -> "ref"
  | Tmutable  -> "mutable"
      
(** Output format is : 
    "<value name=\"%s\" miss=\"%s\" affect=\"%s\" pattern_filter=\"%b\" 
    type_filter=\"%b\" is_record=\"%b\" level=\"%d\" type=\"%s\"></value>@\n"
*)
let print_value_completion fmt = 
  L.iter (fun v -> if(v.vl_fpat) then fprintf fmt "%s@." v.vl_miss)

let print_expr fmt = function
  | C_match (p,pm_c) -> 
      let p = match p with 
	| MF p -> p 
	| ME _ -> Debug.unreachable "ocaml-printer" 99
      in 
      (match pm_c with
	| BranchCs _ ->
	    fprintf fmt "%a@." (print_dispatch_completion true) p
	| MissCs _ -> 
	    fprintf fmt "%a@." (print_cases_completion false) p
	| AllCs -> 
	    fprintf fmt "%a@." (print_cases_completion true) p
      )
	
  | C_module (l,_) -> 
      fprintf fmt "%a@." print_mod_completion l
  | C_value (l,_,_) -> 
      fprintf fmt "%a@." print_value_completion l
  | C_record (l,rk,pat) -> 
      fprintf fmt "%a@." print_rec_completion l
  | C_error ex -> raise ex
  | C_other -> Debug.unreachable "ocaml-printer" 98

let print fmt p = fprintf fmt "%a" print_expr p
