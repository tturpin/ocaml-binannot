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

(** This module provides main operations on annots files such as parsing
    an existing annot file or looking for a type corresponding with 
    a given range *)

open Interface
open Lexing
open Outcometree
open Annotast
open Debug

(** This exception is raised by the function get_type if there is no
    type in the given range *)
exception  Annot_not_found

(** This exception is raised by the function parse_annot below if the
    parsing failed *)
exception  Annot_parse_error
   

(** This function parses the given annot file and returns an
    abstract syntax tree (defined in Annotast) *)
let parse_annot annot_name =
  let stream = open_in annot_name in
  let lexbuf = from_channel stream in
  let ast =
    try Annotparser.annot_parse Annotlexer.nexttoken lexbuf 
    with 
      | Fail lis -> fail "Expression-typing" 1 lis
      | e ->
	Printf.eprintf "Character %d\n%!" lexbuf.lex_curr_p.pos_cnum;
	raise e (*Annot_parse_error*)
  in close_in stream;
  ast

(** This function looks for the type in the given abstract syntax tree
    having the range rg.
    It raises @Annot_not_found if there is no type in the ast
    corresponding with the given range *)
let rec get_type ast rg = 
  let f_aux ty = function
    | A_type t -> t = ty
    | _        -> true
  in
  match ast with
    | []                  -> raise Annot_not_found
    | (hder, contt)::ast' ->
	if (fst hder).pos_cnum = rg.b && 
	  (snd hder).pos_cnum = rg.e then
	  begin
	    match contt with
	      | [] -> unreachable "Annotreader" 2
	      | (A_type ty )::r | (A_call _ )::(A_type ty )::r ->
		  if List.for_all (f_aux ty) r then ty
		  else failwith "Annotreader-3 : Which type to choose ?"
	      | (A_ident _)::_ -> unreachable "Annotreader" 4
	      | (A_call _ )::_ -> unreachable "Annotreader" 5
	  end
	else get_type ast' rg
	
(** This function gets the ast of the given annot file and, for each
    range in range_list, Its looks for its corresponding type in the
    ast. The order of the types in the result is the same as
    the order of the ranges *)
let main annot_name range_list =
  let ast = parse_annot annot_name 
  in List.map (get_type ast) range_list
  
