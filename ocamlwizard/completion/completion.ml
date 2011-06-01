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

let compile_file s_env c_env =
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
  (* The source file is used to look for a .mli, thus we give the name
     of the (non-existing) temp .ml file. *)
  Typemod.type_implementation
    (outputprefix ^ ".ml") outputprefix modulename env s_env.ast,
  {c_env with fb_name = outputprefix}

(** *)
let write_to_file ce se = 
  let ce = { ce with fb_name = ce.fb_name ^ "ocamlwizard_tmp_file" } in
  let out_ch = open_out (ce.fb_name ^ ".ml") in
  output_string out_ch se.cprog;
  flush out_ch ; 
  close_out out_ch;
  ce

(** *)
let mk_list_rg se = 
  let lis = 
    if se.asf_rg = Parsing_env.dummy_range then [] 
    else [se.asf_rg] 
  in 
  if se.exp_rg = Parsing_env.dummy_range then lis 
  else se.exp_rg::lis

(** *)
let rec find_qualif env lid = function
  |[] -> Env.lookup_constructor lid env
  | p::r -> 
      begin
	try Env.lookup_constructor lid env
	with _ -> find_qualif env (Longident.Ldot (lid,p)) r
      end

(** *)
let step msg = 
  if !Common_config.debug then
    Format.eprintf "\n <+> Step : %s \n-----------------@." msg


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
  
(** *)
let main ce = 
  (* 1 - parsing the file *)
  step "Sytactic completion";
  let se, ce  = Syntax_completion.main ce in
  if !Common_config.debug then Debug.print_c_sort se.comp;

(* 
  (* + writing the completed file *)
  let ce = write_to_file ce se in
*)

  (* + compiling the completed file *)
  step "Compiling the completed file";
  let (structure, coercion), ce = compile_file se ce in
  
  (* Exiting with the error code (for auto-test) *)
  if !Common_config.compile_only then
    Debug.exit_with_code (!Common_config.dot_test) se.comp;
    
  step "Expression typing";
  let match_exp =
    match Parsing_env.parser_state.match_exp with
      | Some e ->
	Expression_typing.type_of_exp structure e.Parsetree.pexp_loc
      | None -> assert false
  in

  let pattern_type = match_exp.Typedtree.exp_type in

  let ty_lis = [
    Printtyp.tree_of_typexp false pattern_type
  ] in
  out_types_from_annot ty_lis ;
  
  (* 2.2 - Type_checking*)
  step "Additionnal step : type-checking the file [ for pattern matching]";
  let ty_check = match_exp.Typedtree.exp_env, pattern_type in
  
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
    
