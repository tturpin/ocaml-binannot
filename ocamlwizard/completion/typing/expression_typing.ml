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
open Debug
open Util


exception Found of Typedtree.expression

type expansion_place =
  | Pat of Typedtree.pattern
  (* expand this wildcard or variable *)
  | Args of Typedtree.pattern * Path.t * Types.constructor_description
  (* expand the arguments of this constructor pattern *)

open Typedtree

let expansion_type = function
  | Pat p -> p.Typedtree.pat_env, p.Typedtree.pat_type.Types.desc
  | Args (p, path, desc) ->
    let env = p.Typedtree.pat_env in
    let t = Env.find_type (Path_extraction.type_path p.pat_type) env in
    let c = match path with
      | Path.Pdot (_, c, _) -> c
      | Path.Pident i -> Ident.name i
      | _ -> invalid_arg "expansion_type"
    in
    match t.Types.type_kind with
      | Types.Type_variant cs ->
	env,
	Types.Ttuple (List.assoc c cs)
      | _ -> invalid_arg "expansion_type"

exception Found_pat of expansion_place

open Location

let type_of_exp structure loc =
  debugln "looking for expression at loc:";
  if !Common_config.debug then
    print Format.err_formatter loc;
  debug "";
  let module Type_of_pat =
	Typedtree.MakeIterator (struct
	  include Typedtree.DefaultIteratorArgument
	  let enter_expression e =
	    if e.Typedtree.exp_loc = loc then
	      raise (Found e)
	end)
  in
  try
    Type_of_pat.iter_structure structure;
    raise Not_found
  with
      Found t ->
	debugln "found !";
	t

let type_of_pat structure loc =
  debugln "looking for pattern at loc:";
(*
  if !Common_config.debug then
    print Format.err_formatter loc;
  debugln "";
*)
  let module Type_of_pat =
	Typedtree.MakeIterator (struct
	  include Typedtree.DefaultIteratorArgument
	  let enter_pattern p =
	    match p.Typedtree.pat_desc with
	      (* The pattern Cons _ is parsed as Cons (_, _) with
		 identical locations, so we need a special case. *)
	      | Typedtree.Tpat_construct
		  (c, d, ({pat_loc = l ; pat_desc = Tpat_any} as p' :: ps)) ->
		if (l.loc_start.pos_cnum, l.loc_end.pos_cnum) = loc &&
		  ps <> [] &&
		  List.for_all (function p'' -> p''.pat_loc = l) ps then
		  raise (Found_pat (Args (p, c, d)))
	      | _ ->
	    (*
	      if !Common_config.debug then
	      print Format.err_formatter p.Typedtree.pat_loc;
	    *)
	    let l = p.Typedtree.pat_loc in
	    if (l.loc_start.pos_cnum, l.loc_end.pos_cnum) = loc then
	      raise (Found_pat (Pat p))
	end)
  in
  try
    Type_of_pat.iter_structure structure;
    raise Not_found
  with
      Found_pat t ->
	debugln "found !";
	t
