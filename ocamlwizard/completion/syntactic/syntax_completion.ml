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
open Parsing_env
open Tags
open Format
open Lexing
open Interface

(** Transform a flux/stream in string and close flux/stream *)
let close_flux_to_str flux_in = 
  let buf = Buffer.create 1024 in
  begin
    try 
      while true do 
	Buffer.add_char buf (input_char flux_in)
      done;
    with
	End_of_file -> close_in flux_in
  end;
  Buffer.contents buf 
  
(** *)
let sub_code = String.sub 
  
(** *)
let completed_record ce se prg = function
    Fdummy    -> failwith "Completefile-2 : This case should not happen : Fdummy record"  
  | Faccess _ -> 
      { se with exp_rg = locate_tag prg T_any; 
	asf_rg = locate_tag prg T_asf;
	cprog = prg},ce
  | Fdef _ | Fpat _  -> 
      { se with exp_rg = locate_tag prg T_ogv;cprog = prg },ce
      

(** *)
let completed_path ce se prg pc = 
  match pc.p_kd with
    | Module | Value _ when pc.p_md = [] ->
	(*scope analysis todo*)
	failwith "Completefile-1 : This case is not supported yet ! pc.p_md = [] in completed path"
    | Module     -> { se with cprog = prg ^ (mk_ghost_module pc.p_md)},ce
    | Record rcd -> completed_record ce se prg rcd
    | Value _    -> 
	{se with exp_rg =locate_tag prg T_ogv; 
	  cprog = (prg ^(mk_ghost_module pc.p_md))},ce
	
(** *)
let completed_file ce se state prog  = 
  let prg = (String.sub prog 0 state.c_cut_pos) ^ (state.subs_code) in
  match state.c_sort, ce.c_kind with
    | Match _,("match"|"") ->  
	{se with cprog = prg;exp_rg = locate_tag prg T_any }, ce
    | Path pc,("path"|"") -> completed_path ce se prg pc
    | Try _,("try"|"")  -> se,ce
    | Other,_   -> se,ce
    | Error _,_ -> se,ce
    | _ ->  
	Format.eprintf "Completion non geree:\n%s"
	  "type de completion specifie differe de celui calcule par le parser.";
	exit 2
	  
let default_parser c_env s = 
  let pos = c_env.c_rg.e in
  let s_sz = String.length s in
  
  if s_sz < pos then (
      Format.eprintf " Error : The (-pos) is greater than the file's size\n@?";
      exit 1
    );
  let str = if pos < 0 then s else String.sub s 0 pos in
  
  (** Initialize environnements and complete syntaxically code with parser *)
  let buf = from_string str in
  init_completion_env str;
  let s_env = {
      ast      = [];
      cprog    = "";
      mpath    = [];
      comp     = Other;
      closures = [];
      exp_rg   = dummy_range;
      asf_rg   = dummy_range}
  in 
  try  
    let caml_ast = Owz_parser.implementation Owz_lexer.token buf in 
    let s_env = { 
	s_env with 
	  ast      = caml_ast ; 
	  mpath    = parser_state.mods_path;
	  closures = parser_state.closing ;
	  comp     = parser_state.c_sort;
      }
    in completed_file c_env s_env parser_state str
  with e ->  { s_env with comp = Error e }, c_env
		
let main c_env = 
  let s = close_flux_to_str (open_in c_env.fb_name) in
  match c_env.c_parser with
    | Default_parser -> default_parser c_env s

