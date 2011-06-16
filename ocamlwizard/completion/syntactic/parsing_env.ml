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
open Tags
open Util

(** *)
let dummy_range = { b = -1 ; e = -1 } 

(** *)
let parser_state = {
    closing      = [];
    c_sort       = Other;
    match_exp  = None;
    c_cut_pos    = -1;
    rec_inited   = false;
    eof_pos         = - 1;
    ends_with_space = false;
    prog            = "";
    mods_path       = []}
    
    
(** *)
let no_space () = 
  (not parser_state.ends_with_space) || 
    (match parser_state.c_sort with
      | Match _ -> true | _ -> false)

(** *)
let locate_completion md =  
  parser_state.mods_path <- md :: parser_state.mods_path

(** *)
let end_of_file pos = 
  (1 + pos >= parser_state.eof_pos)
  && (no_space() || pos >= parser_state.eof_pos )
    (* <=>
       if parser_state.no_space
       then 1 + pos >= parser_state.eof_pos 
       else pos >= parser_state.eof_pos *)
    
let can_up_record = function 
    Fdummy , _  -> true | _ -> false
      
(** *)
let can_up_value = function 
    V_all , _ ->  true  | _ -> false
      
let set_rec_inited () = parser_state.rec_inited <- true
  
let rec_is_not_inited () =  not parser_state.rec_inited

let can_up_comp_sort comp_s pos =
  end_of_file pos && 
    begin
      match parser_state.c_sort, comp_s with
	| Other, _  -> true
	| (Path pt, Path pt_comp) ->
	    begin
	      match pt.p_kd, pt_comp.p_kd with
		| Module, _ -> true
		| Record _, Module -> false
		| Record r1, Record r2 -> can_up_record (r1, r2)
		| Record _, _ -> true
		| Value _, Record (Faccess _)  -> true
		| Value _, (Record _ | Module) -> false
		| Value v1, Value v2 -> can_up_value (v1, v2)
	    end
	| _ -> false
	    
      end
    
(** *)
let update_comp_sort comp_s pos = 
  if can_up_comp_sort comp_s pos then 
    begin
      Util.debugln "SETTING THE COMPLETION SORT";
      parser_state.c_sort <- comp_s;
      true
    end
  else false
    
(** *)
let init_completion_env str =
  parser_state.mods_path   <- [];
  parser_state.closing     <- [];
  parser_state.c_sort      <- Other;
  parser_state.c_cut_pos   <- - 1;
  parser_state.rec_inited  <- false;
  (* *)
  
  let len = String.length str in
  parser_state.prog     <- str;
  parser_state.eof_pos  <- len ;
  parser_state.ends_with_space <- String.sub str  (len - 2) 1 = " "    
    
(** *)
let print s = 
  ()
    
(** *)
let update_cut_pos pos = parser_state.c_cut_pos <- pos
  
(** *)
let get_expr b_exp e_exp =
  Util.debugln "get_exp does nothing anymore !";
  ""
(*
  try
    String.sub  parser_state.prog b_exp (e_exp - b_exp) 
  with _ -> failwith ( sprintf "\nde : %d to %d \n"  b_exp e_exp)
*)
    
(** *)
let add_closing op cl p_st p_en =
  let exp = get_expr p_en parser_state.c_cut_pos in
  update_cut_pos p_st;
  parser_state.closing <- cl :: parser_state.closing
    
(** *)
let patterns cases = List.map fst cases
  
(** *)
let mod_and_ident () = 
  match parser_state.c_sort with
    | Path p -> p.p_md , p.p_id
    | _ -> failwith "Completion_env-2 : Completion sorte is not a path"
	    
(** *)
  
(** *)
let update_pattern is_match exp given p_end closing =
  let pm_info = given  in
  let pm_comp = if is_match then Match pm_info else Try pm_info in
  let cond =  update_comp_sort pm_comp p_end in
  if cond then update_cut_pos p_end;
    print closing;
    cond
      
(** *)
let update_match exp given p_end closing =
  debugln "UPDATE MATCH";
  parser_state.match_exp <- Some exp;
  update_pattern true  exp given p_end closing
    
(** *)
let update_try exp given p_end closing  = 
  let _ = update_pattern false exp given p_end closing
  in ()
  
(** *)
let update_module md uid p_st p_end = 
  debugln "UPDATE MODULE";
  if no_space () then
    let cp_s = { p_kd = Module ; p_md = md ; p_id = uid } in
    if update_comp_sort (Path cp_s) p_end then 
      begin
	debugln "comp sort <- Path Module";
	update_cut_pos p_st;
      end

(** *)
let update_value exp md value p_st p_end kd =
  if no_space () then
    let cp_s = { p_kd = Value kd ; p_md = md ; p_id = value } in
    if update_comp_sort (Path cp_s) p_end then (
      parser_state.match_exp <- Some exp;
      update_cut_pos p_st
    )

(** Called when reaching EOF in a label (field name). *)
let update_lbl_longid md id p_st p_end = 
  if no_space () then 
    let cp_s = { p_kd = Record Fdummy; p_md = md; p_id = id } in
    if update_comp_sort (Path cp_s) p_end then update_cut_pos (p_st)
	
(** Called after update_lbl_longident, if the context is e.f *)
let update_expr_longid exp md id exp_st exp_end p_end =
  if no_space () then 
    let cp_s = { p_kd = Record (Faccess exp); p_md = md; p_id = id } in
    if update_comp_sort (Path cp_s) p_end then 
	let exp = get_expr exp_st exp_end in
	update_cut_pos exp_st
	  
(** *)
let update_not_inited_redef md lbl given exp_st exp_en p_en = 
  let cp_s = { p_kd = Record (Fdef given) ; p_md = md ; p_id = lbl } in
    if update_comp_sort (Path cp_s) p_en then
      let exp = get_expr exp_st exp_en in
      ()
	
(** *)
let update_value_kind new_kd = 
  match parser_state.c_sort with
      | Path p ->
	  let p_kd2 = 
	    match p.p_kd with
	      | Value v            -> Value new_kd 
	      | Record _ as r -> r
	      | _ as x -> x 
	  in parser_state.c_sort <- Path { p with p_kd = p_kd2 }
      | _ -> ()

(** *)
let update_value_in_redef exp_st exp_en p_end =
  if no_space () then
    let exp = get_expr exp_st exp_en in
    ()

(** *)
let update_not_inited_def md lbl given exp_st exp_en p_en = 
  let cp_s = { p_kd = Record (Fdef given) ; p_md = md ; p_id = lbl } in
  if update_comp_sort (Path cp_s) p_en then
    let lbl_exp = get_expr exp_st exp_en in
    ()

(** *)
let update_value_in_def exp_st exp_en p_en = 
  if no_space () then
      let lbl_exp = get_expr exp_st exp_en in
      ()
	
(** *)
let update_left_imbr () = ()
      
(** *)
let update_pattern md lbl gv p_en =
  debugln "UPDATE PATTERN";
  let cp_s = { p_kd = Record (Fpat gv) ; p_md = md ; p_id = lbl } in
    if update_comp_sort (Path cp_s) p_en then (
      debugln "comp_sort <- path"
    )
	
(** *)
let untagg_in () = ()
    
(** *)
  let rewrite_function p_st =
    update_cut_pos p_st
