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
    
(** *)
let dummy_range = { b = -1 ; e = -1 } 

(** *)
let parser_state = {
    subs_code    = "";
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
      parser_state.c_sort <- comp_s;
      true
    end
  else false
    
(** *)
let init_completion_env str =
  parser_state.mods_path   <- [];
  parser_state.subs_code   <- "";
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
  parser_state.subs_code <- sprintf " %s %s @?" parser_state.subs_code s
    
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
  parser_state.subs_code <- 
    sprintf " %s %s %s %s " op exp parser_state.subs_code cl;
  parser_state.closing <- cl :: parser_state.closing
    
(** *)
let patterns cases = List.map fst cases
  
(** *)
let mod_and_ident () = 
  match parser_state.c_sort with
    | Path p -> p.p_md , p.p_id
    | _ -> failwith "Completion_env-2 : Completion sorte is not a path"
	    
(** *)
let set_subs_code str = parser_state.subs_code <-  str
  
(** *)
let get_subs_code () = parser_state.subs_code 
  
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
  parser_state.match_exp <- Some exp;
  update_pattern true  exp given p_end closing
    
(** *)
let update_try exp given p_end closing  = 
  let _ = update_pattern false exp given p_end closing
  in ()
  
(** *)
let update_module md uid p_st p_end = 
  if no_space () then
    let cp_s = { p_kd = Module ; p_md = md ; p_id = uid } in
    if update_comp_sort (Path cp_s) p_end then 
      begin
	update_cut_pos p_st;
	set_subs_code "Pervasives"
      end

(** *)
let update_value md value p_st p_end kd =
  if no_space () then
    let cp_s = { p_kd = Value kd ; p_md = md ; p_id = value } in
    if update_comp_sort (Path cp_s) p_end then
      let subs =  sprintf "(let %s = assert false in %s)" ogv tagged_ogv in
      set_subs_code subs;
      update_cut_pos p_st
	  
(** *)
let update_lbl_longid md id p_st p_end = 
  if no_space () then 
    let cp_s = { p_kd = Record Fdummy; p_md = md; p_id = id } in
    if update_comp_sort (Path cp_s) p_end then update_cut_pos (p_st)
	
(** *)
let update_expr_longid md id exp_st exp_end p_end =
  if no_space () then 
    let cp_s = { p_kd = Record (Faccess V_all); p_md = md; p_id = id } in
    if update_comp_sort (Path cp_s) p_end then 
	let exp = get_expr exp_st exp_end in
	let subs = sprintf "(let %s = %s in %s)" tagged_any exp tagged_asf in
	set_subs_code subs;
	update_cut_pos exp_st
	  
(** *)
let update_not_inited_redef md lbl given exp_st exp_en p_en = 
  let cp_s = { p_kd = Record (Fdef given) ; p_md = md ; p_id = lbl } in
    if update_comp_sort (Path cp_s) p_en then
      let exp = get_expr exp_st exp_en in
      let subs = sprintf "(let %s = %s in %s)" ogv exp tagged_ogv in
      set_subs_code subs
	
(** *)
let update_value_kind new_kd = 
  match parser_state.c_sort with
      | Path p ->
	  let p_kd2 = 
	    match p.p_kd with
	      | Value v            -> Value new_kd 
	      | Record (Faccess v) -> Record (Faccess new_kd)
	      | _ as x -> x 
	  in parser_state.c_sort <- Path { p with p_kd = p_kd2 }
      | _ -> ()

(** *)
let update_value_in_redef exp_st exp_en p_end =
  if no_space () then
    let exp = get_expr exp_st exp_en in
    let subs = parser_state.subs_code in
    let new_rec = sprintf "(let %s = {%s%s} in %s)" any exp subs asf in
    set_subs_code new_rec

(** *)
let update_not_inited_def md lbl given exp_st exp_en p_en = 
  let cp_s = { p_kd = Record (Fdef given) ; p_md = md ; p_id = lbl } in
  if update_comp_sort (Path cp_s) p_en then
    let lbl_exp = get_expr exp_st exp_en in
    let exp = sprintf "{ (assert false) with %s }" lbl_exp in
    let subs = sprintf "(let %s = %s in %s)" ogv exp tagged_ogv in
    set_subs_code subs
      
(** *)
let update_value_in_def exp_st exp_en p_en = 
  if no_space () then
      let lbl_exp = get_expr exp_st exp_en in
      let subs1 =  parser_state.subs_code in
      let exp = sprintf "{ (assert false) with %s%s }" lbl_exp subs1 in
      let subs2 = sprintf "(let %s = %s in %s)" ogv exp tagged_ogv in
      set_subs_code subs2
	
(** *)
let update_left_imbr () =
    let subs1 = parser_state.subs_code in
    let subs2 = sprintf "(let %s = %s in %s)" any subs1 asf in
    set_subs_code subs2
      
(** *)
let update_pattern md lbl gv p_en = 
  let cp_s = { p_kd = Record (Fpat gv) ; p_md = md ; p_id = lbl } in
    if update_comp_sort (Path cp_s) p_en then
      let subs = sprintf "%s as %s2" tagged_ogv ogv in
      set_subs_code subs
	
(** *)
let untagg_in () =
  parser_state.subs_code <- untag_letin parser_state.subs_code
    
(** *)
  let rewrite_function p_st =
    let c_pos = parser_state.c_cut_pos in
    let new_f = sprintf "%s %s"(get_expr p_st c_pos) parser_state.subs_code in
    parser_state.subs_code <- sprintf "(let %s = %s in %s)" any new_f asf;
    update_cut_pos p_st
