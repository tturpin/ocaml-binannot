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
open Util

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
  
(* We don't want to backtrack on the last chunk. *)
let rec enforce_last_modification = function
  | [] | [Diff.Same _] as m -> m
  | [Diff.Changed (_, s)] -> [Diff.Same s]
  | t :: q -> t :: enforce_last_modification q

let default_parser c_env s f = 
  let pos = c_env.c_rg.e in
  let s_sz = String.length s in
  if s_sz < pos then (
      Format.eprintf " Error : The (-pos) is greater than the file's size\n@?";
      exit 1
    );
  let str = if pos < 0 then s else String.sub s 0 pos in
  debug "Cutting at position %d..." pos;
  let f =
    enforce_last_modification
      (if pos < 0 then f else Diff.cut_new pos f)
  in
  debugln " OK";
  if !Common_config.debug then (
    Diff.print_modified stderr f ; prerr_endline "EOF"
  );
  
  (** Initialize environnements and complete syntaxically code with parser *)
  let buf = from_string str in
  init_completion_env str;
  let s_env = {
      ast      = [];
      cprog    = "";
      mpath    = [];
      comp     = Other;
      closures = [];
      exp_rg   = dummy_range}
  in
  try  
(*
    let caml_ast = Owz_parser.implementation Owz_lexer.token buf in 
*)
   debug "Parsing...";
   let caml_ast = IncParser.implementation f in
   debugln " OK";
   if !Common_config.debug then
     Printast.implementation Format.err_formatter caml_ast;
    let s_env = { 
	s_env with 
	  ast      = caml_ast ; 
	  mpath    = parser_state.mods_path;
	  closures = parser_state.closing ;
	  comp     = parser_state.c_sort;
      }
    in s_env, c_env
  with e ->
    prerr_endline (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    { s_env with comp = Error e }, c_env

let main c_env =
  debug "Reading modified file...";
  let f = Diff.read_modified_file
    (c_env.fb_name ^ ".last_compiled") c_env.fb_name in
  debugln " OK";
  let s = close_flux_to_str (open_in c_env.fb_name) in
  match c_env.c_parser with
    | Default_parser -> default_parser c_env s f

