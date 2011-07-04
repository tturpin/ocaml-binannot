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
open Lexing
open Location
open Longident
open Path
open Types
open Typedtree
open TypedtreeOps
open Env
open Resolve
open FindName
open RenameLid
open RenamePropagation

(*
(* The type of renaming conotexts, i.e., modules or module types of
   which the renamed element is a member. *)
type renaming_context =
  | Persistent of Path.t (* a flat path starting with a persistent Id *)
  | Local of Ident.t (* A "local" ident *)
*)

type toplevel_item =
  | Ml of structure
  | Mli of signature

(* Identifiers for a set of compilation units. *)
type absolute_id = toplevel_item * Ident.t (* non-persistent Id *)
type absolute_path = toplevel_item * Path.t (* maybe persistent root Id *)

(*
let rec path2lident = function
  | Pident i -> Lident (Ident.name i)
  | Pdot (p, n, _) -> Ldot (path2lident p, n)
  | Papply (p, p') -> Lapply (path2lident p, path2lident p')
*)

(*
type exp_exp =
  | Rexp_let_left of rec_flag * (pattern * expression) list * int * expression
  | Rexp_let_right of rec_flag * (pattern * expression) list
(* TODO *)
(*
  | Texp_function of label * (pattern * expression) list * partial
  | Texp_apply of expression * (label * expression option * optional) list
  | Texp_match of expression * (pattern * expression) list * partial
  | Texp_try of expression * (pattern * expression) list
  | Texp_tuple of expression list
  | Texp_construct of Path.t * constructor_description * expression list
  | Texp_variant of label * expression option
  | Texp_record of (Path.t * label_description * expression) list * expression option
  | Texp_field of expression * Path.t * label_description
  | Texp_setfield of expression * Path.t * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * expression * expression * direction_flag * expression
  | Texp_constraint of expression * core_type option * core_type option
  | Texp_when of expression * expression
  | Texp_send of expression * meth * expression option
  | Texp_setinstvar of Path.t * Path.t * expression
  | Texp_override of Path.t * (Path.t * expression) list
  | Texp_letmodule of Ident.t * module_expr * expression
  | Texp_assert of expression
  | Texp_lazy of expression
  | Texp_poly of expression * core_type option
  | Texp_newtype of string * expression
  | Texp_open of Path.t * expression
*)

and exp_str =
  | Rstr_eval
  | Rstr_value of rec_flag * (pattern * expression) list * int

type ident_ctx =
  | Rpat_var of pattern * pattern_ctx
  | Rstr_module of structure_item * structure_item_ctx

and pattern_ctx =
  | Rexp_let of expression * expression_ctx
  | Rexp_function of expression * expression_ctx
  | Rexp_match of expression * expression_ctx
  | Rexp_try of expression * expression_ctx
  | Rpat_alias of pattern_desc * alias_kind

and expression_ctx =
  | Rexp of expression * exp_exp
  | Rstr of structure_item * exp_str

and expression = {
  exp : Parsetree.expression
  exp_ctx : expression_ctx
}

and structure_item_ctx

let expression ctx exprs e = match e.exp_desc with
  | Texp_let (_, bindings, exp) ->
    exprs exp (Rexp_let_right (e, ctx))
  | Texp_ident _
  | Texp_constant _
  | Texp_function _
  | Texp_apply _
  | Texp_match _
  | Texp_try _
  | Texp_tuple _
  | Texp_construct _
  | Texp_variant _
  | Texp_record _
  | Texp_field _
  | Texp_setfield _
  | Texp_array _
  | Texp_ifthenelse _
  | Texp_sequence _
  | Texp_while _
  | Texp_for _
  | Texp_constraint _
  | Texp_when _
  | Texp_send _
  | Texp_new _
  | Texp_instvar _
  | Texp_setinstvar _
  | Texp_override _
  | Texp_letmodule _
  | Texp_assert _
  | Texp_assertfalse
  | Texp_lazy _
  | Texp_poly _
  | Texp_object _
  | Texp_newtype _
  | Texp_pack _
  | Texp_open _ ->  ()
*)
(*
let resolve item = function
    | Pident i -> item, i
    | Pdot (p, n, _) ->
      let item, i = resolve p in
    | Papply (p, p') -> assert false
*)


let check_lids renamed_kind id name' lids =
  List.iter
    (function _, lid, (env, kind) ->
      check_lid renamed_kind id name' env kind lid)
    lids

let rename_lids renamed_kind id name' lids =
  List.fold_left (* this is a filter_map *)
    (fun l (loc, lid, (env, kind)) ->
      match rename_in_lid renamed_kind id name' env kind lid with
	| Some lid ->
	  (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum, Util.lid_to_str lid)
	  :: l
	| None -> l)
    []
    (List.rev lids)

let read_cmt file =
  if Filename.check_suffix file ".cmt" then
    let c = open_in file in
    let data = input_value c in
    close_in c;
    match data.(0) with
      | Saved_implementation str ->
	  (try
	     match data.(1) with
	       | Saved_ident_locations (Some loc) ->
		   str, loc
	       | Saved_ident_locations None ->
		   failwith "ident location table is empty"
	       | _ -> raise Not_found
	   with
	       _ -> failwith "ident location table not found in cmt")
      | _ -> failwith "error reading cmt file"
  else
    invalid_arg "read_cmt"

let sort_replaces =
  List.sort
    (fun (x, _, _) (y, _, _) -> compare  x y)

let find_id_defs ids name s =
  List.fold_right
    (fun id acc ->
      try
	let loc = find_id_def s id in
	(loc.loc_start.pos_cnum, loc.loc_end.pos_cnum, name) :: acc
      with
	  Not_found -> acc)
    ids
    []
(*
 [fst loc, snd loc, name']
*)

(* TODO *)
let valid_ident kind name = true

(* Temporary : we rename only in one file *)
let rename loc name name' file =
  let s, idents = read_cmt (Filename.chop_suffix file ".ml" ^ ".cmt") in

  (* Get the "initial" id to rename and its sort *)
  let renamed_kind, id = locate_renamed_id (`structure s) loc in

  (* Collect constraints requiring simultaneous renaming *)
  let incs, includes = collect_signature_inclusions s in

  (* Deduce the minimal set of ids to rename *)
  let ids, implicit_refs = propagate_renamings renamed_kind id incs includes in

  List.iter
    (function id -> debugln "rename %s\n%!" (Ident.unique_name id))
    ids;

  (* Compute the replacements for the *definitions* of the rename ids *)
(*
  let def_replaces = find_id_defs ids (`structure s) in (* obviously incomplete ! *)
*)
  let def_replaces = find_id_defs ids name' idents in (* obviously incomplete ! *)

  (* Check that our new name will not capture useful signature members *)
  check_other_implicit_references renamed_kind ids name' incs includes;

  (* Check that useful renamed signature members are not masked. *)
  check_renamed_implicit_references renamed_kind ids name' implicit_refs;

  (* Collect all lids *)
  let lids = get_lids file s in

  (* Check that our new name will not capture other occurrences *)
  check_lids renamed_kind ids name' lids;

  (* Compute renamed lids, checking that they are not captured *)
  let occ_replaces = rename_lids renamed_kind ids name' lids in

  (* We will need to sort them again ! *)
  let replaces = sort_replaces (def_replaces @ occ_replaces) in

  List.iter
    (function b, e, s -> debugln "replace %d--%d by %s\n%!" b e s)
    replaces;

  (* Replace lids in the source file *)
  Edit.edit replaces file;

  exit 0
