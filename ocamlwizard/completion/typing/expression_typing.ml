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

open Lexing
open Debug
open Util
open Typedtree
open TypedtreeOps
open Location


type expansion_place =
  | Pat of Typedtree.pattern
  (* expand this wildcard or variable *)
  | Args of Typedtree.pattern * Path.t * Types.constructor_description
  (* expand the arguments of this constructor pattern *)

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
    match desc.Types.cstr_tag, t.Types.type_kind with
      | Types.Cstr_exception pe, _ ->
	  env,
	  Types.Ttuple desc.Types.cstr_args
      | _, Types.Type_variant cs ->
	let cs = List.map (function id, x -> Ident.name id, x) cs in
	  env,
	  Types.Ttuple (List.assoc c cs)
      | _ -> invalid_arg "expansion_type"

let locate_expression s loc =
  let expression e =
    if e.Typedtree.exp_loc = loc then
      Some e
    else
      None
  in
  find_expression `outermost expression (`structure s)

(* Maybe risky, because different sorts of nodes sometimes have the
   same location. *)
let locate_expression s loc =
  match TypedtreeOps.locate `innermost (Util.get_c_num loc) (`structure s) with
    | `expression e -> e
    | _ -> raise Not_found

let locate_expansion_place s (b, e as loc) =
  let pattern p =
    debugln "looking for pattern at loc: [%d, %d[" b e;
    debugln "visiting pattern at loc: [%d, %d["
      p.Typedtree.pat_loc.loc_start.pos_cnum p.Typedtree.pat_loc.loc_end.pos_cnum;
    match p.Typedtree.pat_desc with
      (* The pattern Cons _ is parsed as Cons (_, _) with
	 identical locations, so we need a special case. *)
      | Typedtree.Tpat_construct
	  (c, d, ({pat_loc = l ; pat_desc = Tpat_any} :: ps)) ->
	if (l.loc_start.pos_cnum, l.loc_end.pos_cnum) = loc &&
	  ps <> [] &&
	  List.for_all (function p'' -> p''.pat_loc = l) ps then (
	    debugln "found constructor arguments";
	    Some (Args (p, c, d))
	  ) else
	  None
      | _ ->
	    (*
	      if !Common_config.debug then
	      print Format.err_formatter p.Typedtree.pat_loc;
	    *)
	let l = p.Typedtree.pat_loc in
	if (l.loc_start.pos_cnum, l.loc_end.pos_cnum) = loc then (
	  debugln "found pattern";
	  Some (Pat p)
	) else
	  None
  in
  find_pattern `outermost pattern (`structure s)

