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

(** The main module for completion : It coordinates the differentes 
    steps : 
    - syntactic completion 
    - expression typing 
    - proposal extraction
    - proposal filering
    - proposal printing*)

open Format
open Interface

exception Compilation_failed

(* Copied from driver/compile (to avoid many dependencies) *)
let initial_env () =
  Ident.reinit();
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    Misc.fatal_error "cannot open pervasives.cmi"

let compile_file ast c_env =
  let prefix = Filename.chop_extension c_env.fb_name in
  (* Maybe we should avoid using ".". *)
  let outputprefix = prefix ^ ".ocamlwizard" in
  (* We change the module name accordingly because it is checked against
     the file name when reading .cmi files. *)
  let modulename = String.capitalize (Filename.basename outputprefix) in
  Clflags.include_dirs := (*! Clflags.include_dirs @*) c_env.includ;
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) !Clflags.include_dirs in
  Config.load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
  Clflags.compile_only := true;
  let env = initial_env () in
  (* This is probably not needed *)
  Typecore.reset_delayed_checks ();
  let str, _, _ =
    let ppf = err_formatter in
    try
      Typemod.type_structure env ast Location.none
    with e ->
      (match e with
	| Typecore.Error(loc, err) ->
	  Location.print_error ppf loc; Typecore.report_error ppf err
	| Typetexp.Error(loc, err) ->
	  Location.print_error ppf loc; Typetexp.report_error ppf err
	| Typedecl.Error(loc, err) ->
	  Location.print_error ppf loc; Typedecl.report_error ppf err
	| Typeclass.Error(loc, err) ->
	  Location.print_error ppf loc; Typeclass.report_error ppf err
	| Includemod.Error err ->
	  Location.print_error_cur_file ppf;
	  Includemod.report_error ppf err
	| Typemod.Error(loc, err) ->
	  Location.print_error ppf loc; Typemod.report_error ppf err
	| _ -> raise e);
      failwith "Error while typing"
  in
  str, {c_env with fb_name = outputprefix}

(** *)
let mk_list_rg se = 
  let lis = 
    if se.asf_rg = Parsing_env.dummy_range then [] 
    else [se.asf_rg] 
  in 
  if se.exp_rg = Parsing_env.dummy_range then lis 
  else se.exp_rg::lis

(** *)
let step msg = 
  Util.debugln "\n <+> Step : %s \n-----------------" msg

(*
let out_types_from_annot ty_lis = 
  if !Common_config.debug then (
      Format.eprintf "> Types from .annot : [";
      match ty_lis with
	| []   -> 
	    Format.eprintf "empty]@."; 
	| [a]  ->        
	    !Oprint.out_type Format.err_formatter a;
	    Format.eprintf "]@."
	| [a;b]->      
	    !Oprint.out_type Format.err_formatter a;
	    Format.eprintf "]@."
	| _   -> 
	    Debug.unreachable "Comletion" 7
    )
*)

(** *)
let main ce = 
  (* 1 - parsing the file *)
  step "Parsing with sytactic completion";
  let se, ce  = Syntax_completion.main ce in
  if !Common_config.debug then Debug.print_c_sort se.comp;

  (* Avoids an error when type inference tries to locate a warning *)
  Location.input_name := "";

  (* Typing the completed file *)
  step "Typing the the completed parsetree";
  let structure, ce = compile_file se.ast ce in
  
  (* Exiting with the error code (for auto-test) *)
  if !Common_config.compile_only then
    Debug.exit_with_code (!Common_config.dot_test) se.comp;
  
  let pattern_env, pattern_type =
    match se.comp with
      | Match (AllCs | MissCs _) -> (* We should rather look at the pattern. *)
	step "Getting the type of the matched expression";
	let match_exp =
	  match Parsing_env.parser_state.match_exp with
	    | Some e ->
	      Expression_typing.type_of_exp structure e.Parsetree.pexp_loc
	    | None -> assert false
	in
	match_exp.Typedtree.exp_env, match_exp.Typedtree.exp_type
      | Match (BranchCs (p, l)) ->
	let match_pat =
	  Expression_typing.type_of_pat structure
	    ! Common_config.expand_loc
(*
 p.Parsetree.ppat_loc
*)
	in
	match_pat.Typedtree.pat_env, match_pat.Typedtree.pat_type
      | Try _ -> assert false
      | Path _ -> assert false
      | Other -> assert false
      | Error _ -> assert false
  in

  let ty_lis = [
    Printtyp.tree_of_typexp false pattern_type
  ] in
  
  let ty_check = pattern_env, pattern_type in
  
  (* 3 - Extracting propositions *)
  step "Proposal extraction";
  let c_res = Proposal_extraction.main ce se ty_lis ty_check in

  (* 4 - Filtering propositions *)
  step "Proposal filtering";
  let c_res  = Proposal_filtering.main c_res ty_lis in

  (* 5 - Printing the result in a formatter *)
  step "Proposal printing";
  Proposal_printing.main std_formatter ty_lis c_res ce.c_printer;
  
  (* Exiting with the error code (for auto-test) *)
  Debug.exit_with_code (!Common_config.dot_test) se.comp
    
