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
open Util

exception Compilation_failed

let compile_file ast c_env =
  Clflags.include_dirs := (*! Clflags.include_dirs @*) c_env.includ;
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) !Clflags.include_dirs in
  Config.load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
  Clflags.compile_only := true;
  let env = initial_env () in
  (* This is probably not needed *)
  Typecore.reset_delayed_checks ();
  let str, sg, _ =
    let ppf = err_formatter in
    try
      Typemod.type_structure env ast Location.none
    with e ->
      (match e with
	| Typecore.Error(loc, err) -> debugln "1";
	  Location.print_error ppf loc; Typecore.report_error ppf err
	| Typetexp.Error(loc, err) -> debugln "2";
	  Location.print_error ppf loc; Typetexp.report_error ppf err
	| Typedecl.Error(loc, err) -> debugln "3";
	  Location.print_error ppf loc; Typedecl.report_error ppf err
	| Typeclass.Error(loc, err) -> debugln "5";
	  Location.print_error ppf loc; Typeclass.report_error ppf err
	| Includemod.Error err -> debugln "6";
	  Location.print_error_cur_file ppf;
	  Includemod.report_error ppf err
	| Typemod.Error(loc, err) -> debugln "7";
	  Location.print_error ppf loc; Typemod.report_error ppf err
	| _ -> debugln "8"; raise e);
      Format.pp_print_flush err_formatter ();
      failwith "Error while typing"
  in
  str, sg, c_env

(** *)
let step msg = 
  Util.debugln "\n <+> Step : %s \n-----------------" msg

(*
let out_types_from ty_lis = 
  if !Common_config.debug then (
      Format.eprintf "> Types: [";
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
  let structure, sg, ce = compile_file se.ast ce in
  Util.debugln "OK";
  
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
	      Expression_typing.locate_expression structure e.Parsetree.pexp_loc
	    | None -> assert false
	in
	match_exp.Typedtree.exp_env, match_exp.Typedtree.exp_type
      | Match (BranchCs (p, l)) | Try (BranchCs (p, l)) ->
	let place =
	  Expression_typing.locate_expansion_place structure
	    ! Common_config.expand_loc
(*
 p.Parsetree.ppat_loc
*)
	in
	let env, desc =
	  Expression_typing.expansion_type place in
	env,
	{Types.desc = desc;
	 level = 0; (* Meaningless ! *)
	 id = 0}
      | Try _ -> assert false
      | Path {p_kd = Record (Faccess e)} ->
	let match_exp =
	  Expression_typing.locate_expression structure e.Parsetree.pexp_loc
	in
	match_exp.Typedtree.exp_env, match_exp.Typedtree.exp_type
      | Path {p_kd = Module _} ->
	(* Que c'est moche ! *)
	(initial_env ()), {Types.desc = Types.Tvar ; level = 0; id = 0}
      | Path _ ->
	let match_exp =
	  match Parsing_env.parser_state.match_exp with
	    | Some e ->
	      Expression_typing.locate_expression structure e.Parsetree.pexp_loc
	    | None -> assert false
	in
	match_exp.Typedtree.exp_env, match_exp.Typedtree.exp_type
      | Other -> assert false
      | Error _ -> assert false
  in

  Format.eprintf "pattern_type = %a\n%!" Printtyp.type_expr pattern_type;
  let ty = Printtyp.tree_of_typexp false pattern_type in
  let ty_lis = [ty] in
  
  let ty_check = pattern_env, pattern_type in
  
  (* 3 - Extracting propositions *)
  step "Proposal extraction";
  let c_res = Proposal_extraction.main sg ce se ty_check in

  (* 4 - Filtering propositions *)
  step "Proposal filtering";
  let c_res  = Proposal_filtering.main c_res ty_lis in

  (* 5 - Printing the result in a formatter *)
  step "Proposal printing";
  Proposal_printing.main ~miss:Parsing_env.parser_state.miss std_formatter ty_lis c_res ce.c_printer;
  
  (* Exiting with the error code (for auto-test) *)
  Debug.exit_with_code (!Common_config.dot_test) se.comp
    
