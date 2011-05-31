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

(** *)
let compile_file ce = 
  let f_aux e ac = sprintf "-I %s %s" e ac in
  let dirs = List.fold_right f_aux  ce.includ "" in
  let cmd = 
    sprintf "%s %s %s.ml " Common_config.ocaml_compiler dirs ce.fb_name 
  in
  let cmd_res = Unix.system cmd in
  match cmd_res with 
    | Unix.WEXITED 0 ->
	if !Common_config.debug then
	  Format.eprintf "compilation succeeded@."
    | _ -> 
	if !Common_config.debug then
	  Format.eprintf "compilation failed@."

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
  let modulename =
    String.capitalize (Filename.basename (Filename.chop_extension c_env.fb_name))
  in
  Clflags.include_dirs := (*! Clflags.include_dirs @*) c_env.includ;
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) !Clflags.include_dirs in
  Config.load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
  Clflags.compile_only := true;
  let env = initial_env () in
  (* The source file is used to look for a .mli, thus we give the name
     of the temp file. *)
  let res =
    Typemod.type_implementation
      c_env.fb_name c_env.fb_name modulename env s_env.ast
  in
  Stypes.dump (c_env.fb_name ^ ".annot");
  res

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
 
  (* + writing the completed file *)
  let ce = write_to_file ce se in
 
  (* + compiling the completed file *)
  step "Compiling the completed file";
  let structure, coercion = compile_file se ce in
  
  (* Exiting with the error code (for auto-test) *)
  if !Common_config.compile_only then
    Debug.exit_with_code (!Common_config.dot_test) se.comp;
    
  (* 2.1 - Reading the type from .annot *)
  step "Expression typing";
  let ty_lis = 
(*
    Expression_typing.main (ce.fb_name^".annot")(mk_list_rg se) 
*)
    let range = match Parsing_env.parser_state.match_exp with
      | Some e -> e.Parsetree.pexp_loc
      | None -> assert false in
    [Expression_typing.type_of_pat structure range]
  in
  out_types_from_annot ty_lis ;
  
  (* 2.2 - Type_checking*)
  step "Additionnal step : type-checking the file [ for pattern matching]";
  let ty_check = 
    None
(*
    if !Common_config.match_annot then None
    else 
      match se.comp with
	| Match _ ->
	    begin
	      try
		Common_config.loc := (se.exp_rg.b, se.exp_rg.e) ;
		List.iter (fun s -> 
		  Clflags.include_dirs := s :: !Clflags.include_dirs)
		  (List.rev ce.includ);
		
		Main.main [| "ocamlwizard"; ce.fb_name ^ ".ml" |];
		Debug.unreachable "Completion" 1
	      with 
		| Sc_completion.Partial_env (env,ty_pat) ->
		    Printtyp.type_expr Format.err_formatter ty_pat;
		    Some (env, ty_pat)
	    end
	| _  -> None
*)
  in
  
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
    
