(**************************************************************************)
(*                                                                        *)
(*  Ocamlwizard-Binannot                                                  *)
(*  Tiphaine Turpin                                                       *)
(*  Copyright 2011 INRIA Saclay - Ile-de-France                           *)
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

open Util
open Location
open Types
open Typedtree
open TypedtreeOps
open Resolve

type occurrence_kind = [
  `exp_ident
| `mod_ident
| `exp_open
| `str_open
| `sig_open
| `mty_with
| `mty_ident
]

(* Is that meaningful ? *)
let rec path2loc idents = function
  | Path.Pident s -> Location.StringTbl.find idents (Ident.name s)
  | Path.Pdot (p, s, _) ->
    let l = path2loc idents p
    and l' = Location.StringTbl.find idents s in
    {l with Location.loc_end = l'.Location.loc_end}
  | Path.Papply (p, p') -> failwith "not implemented"

let rec env_of_node father_table n =
  let up () =
    match NodeTbl.find_all father_table n with
      | [father] -> env_of_node father_table father
      | [] -> failwith "root"
      | _ -> failwith "ambiguous"
  in
  match n with
    | `core_type _
    | `pattern _ -> up ()
    | `structure_item { str_desc = Tstr_type _ } -> failwith "env"
    | _ -> failwith ("env_of_node: unsupported case " ^ node_kind n)

(* This should only be complete w.r.t. values and module paths ! But
   we cannot have safe renaming for modules until we are complete
   w.r.t. paths of all sorts. *)
let find_all_occurrences idents tree =
  let father_table = reverse tree in
  let found loc env occ = Some (loc, (env, occ))
  and loc = path2loc idents
  and env = env_of_node father_table in
  find_all_map
    (function n ->
      match n with

(*
      | `pattern p ->
	(match p.pat_desc with
	  | Tpat_alias (_, TPat_type p) ->
	    found (path2loc p) (assert false) `pat_alias_type)
*)
(*
      | `core_type t ->
	(match t.ctyp_desc with
	  | Ttyp_constr (p, _) -> found (loc p) (env n) `core_type_type
	  | _ -> None)
*)

      | `expression e ->
	(match e.exp_desc with
	  | Texp_ident _ -> found e.exp_loc e.exp_env `exp_ident
	  (* If the renamed ident is not a module or modtype,
	     then we could filter according to the right_most
	     ident. Otherwise, there is no way to know if we
	     need renaming until we get the longident. *)

	  (* needed for modules
	     | Texp_open _ -> found e.exp_loc e.exp_env `exp_open
	  *)

	  (* No instance variables for now *)
	  | Texp_instvar (_self, var)
	  | Texp_setinstvar (_self, var, _) -> None
	  | Texp_override (_self, modifs) -> None
	  | _ -> None)

      | `module_expr m ->
	(match m.mod_desc with
	  | Tmod_ident _ -> found m.mod_loc m.mod_env `mod_ident
	  | _ -> None)

      | `module_type t ->
	(match t.mty_desc with
	  (*
	    | Tmty_ident p -> found t.mty_loc (assert false) `mty_ident
	  *)
(*
	  | Tmty_with (_, cs) ->
	    List.iter
	      (function p, c -> match c with
		| Twith_type _ -> ()
		| _ -> ())
	      cs
*)
	  | _ -> None)

      | `with_constraint _ -> None
	  (*
	    | Twith_module _
	    | Twith_modsubst _ ->
	    found (assert false) (assert false) (assert false)
	  *)

      | `structure_item i ->
	(match i.str_desc with
	  | Tstr_open _ -> found i.str_loc i.str_env `str_open
	  | _ -> None)

	  (* needed for modules
	     | `module_type t =
	     match t.mty_desc with
	     | Tmty_with _ ->
	  *)
	  (*

	    | `signature_item i =
	    match i.sig_desc with
	    | Tsig_open _ -> found i.sig_loc (assert false) `sig_open

	    | _ -> ()
	  *)
      | _ -> None)
    tree

let get_occurrences idents s =
  List.sort
    (fun (loc, _) (loc', _) ->
      let open Lexing in
      compare loc.loc_start.pos_cnum loc.loc_end.pos_cnum)
    (find_all_occurrences idents s)

let extract_longident (loc, s, (env, occ)) =
  let parse parser s =
    let lexbuf = Lexing.from_string s in
      parser Lexer.token lexbuf
  in
  let parser, kind = match occ with
    | `exp_ident ->
	(function s ->
	   try parse Parser.val_longident s
	   with _ -> Longident.Lident (parse Parser.operator s)),
	value_ops
    | `mod_ident -> parse Parser.mod_longident, module_ops
    | _ -> failwith "not implemented"
  in
  let ast =
    try
      parser s
    with _ ->
      failwith ("error parsing the following ident: " ^ s)
  in
    (loc, ast, (env, kind))

let get_lids file idents ast =
  List.map
    extract_longident
    (source_locations file (get_occurrences idents ast))

let ident_of_subtree = function
  | `pattern {pat_desc = Tpat_var id}
  | `expression {exp_desc = Texp_for (id, _, _, _, _)}
  | `signature_item {sig_desc = Tsig_value (id, _)}
    -> value_ops, id
  | `structure_item {str_desc = Tstr_module (id, _)}
    -> module_ops, id
  | `structure_item {str_desc = Tstr_modtype (id, _)}
    -> modtype_ops, id
  | `structure_item {str_desc = Tstr_type types}
    -> (match types with
      | [id, _] -> type_ops, id
      | _ -> failwith "multiple type definitions are not yes supported")
  | _ -> raise Not_found

(* Should be almost complete for expressions, but this is not a safety
   requirement anyway. *)
let locate_renamed_id s loc =
  try
    let kind, id = ident_of_subtree (locate `innermost loc s) in kind, id
  with Not_found ->
    invalid_arg "rename"

let find_id_def table id =
  StringTbl.find table (Ident.name id)
