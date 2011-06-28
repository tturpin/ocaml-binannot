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


(* Check that the implicit ident references which are concerned by
   renaming will not be masked (i.e., that the bound signature items
   remain the same). *)
let check_signature_inclusions renamed_kind ids name' occs =
  List.iter
    (function sg, _ -> check_in_sig renamed_kind ids name' sg ~renamed:true)
    occs


let get_occurrences s =
  let l = ref [] in
  let module Rename =
	MakeIterator
	  (struct
	    include DefaultIteratorArgument

	    let enter_expression e =
	      match e.exp_desc with
		| Texp_ident _ ->
		  (* If the renamed ident is not a module or modtype,
		     then we could filter according to the right_most
		     ident. Otherwise, there is no way to know if we
		     need renaming until we get the longident. *)
		  l := (e.exp_loc, e) :: !l
		| _ -> ()
	 end)
  in
  Rename.iter_structure s;
  !l

(*
let get_occurrences s =
  let l = ref [] in
  let module Rename =
	MakeIterator
	  (struct
	    include DefaultIteratorArgument

	    let leave_pattern p =
	    let enter_expression e =
	      match e.exp_desc with
		| Texp_ident _ ->
		  (* If the renamed ident is not a module or modtype,
		     then we could filter according to the right_most
		     ident. Otherwise, there is no way to know if we
		     need renaming until we get the longident. *)
		  l := (e.exp_loc, e) :: !l
		| _ -> ()
	   end)
  in
  Rename.iter_structure s;
  !l
*)

(* Traverse a source file to get a list of locations *)
let source_locations f file locs acc =
  let c = open_in file in
  let acc =
    List.fold_left
      (fun acc (loc, x) ->
	let len = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
	let s = String.create len in
	seek_in c loc.loc_start.pos_cnum;
	really_input c s 0 len;
	f loc x s acc)
      acc
      locs
  in
  close_in c;
  acc

let get_asts parser file locs =
  let f loc x s asts =
    let lexbuf = Lexing.from_string s in
    let ast = parser Lexer.token lexbuf in
    (ast, x) :: asts
  in
  source_locations f file locs []

let get_lids file ast =
  get_asts Parser.val_longident file (get_occurrences ast)

let check_lids id name' lids =
  List.iter
    (function lid, e ->
      check_lid value_ops id name' e.exp_env value_ops lid)
    lids

let rename_lids id name' lids =
  List.fold_left
    (fun l (lid, e) ->
      match rename_in_lid value_ops id name' e.exp_env value_ops lid with
	| Some lid ->
	  (e.Typedtree.exp_loc, lid) :: l
	| None -> l)
    []
    lids

let locate_pattern s loc =
  let pattern p =
    if Util.get_c_num p.pat_loc = loc then
      Some p
    else
      None
  in
  (find_pattern pattern).structure s

let read_cmt file =
  if Filename.check_suffix file ".cmt" then
    let c = open_in file in
    match (input_value c).(0) with
      | Saved_implementation str -> str
      | _ -> failwith "error reading cmt file"
  else
    invalid_arg "read_cmt"

(* Temporary : we rename only in one file *)
let rename loc name name' file =
  let s = read_cmt (Filename.chop_suffix file ".ml" ^ ".cmt") in
  let id =
    match (locate_pattern s loc).pat_desc with
      | Tpat_var id -> id
      | _ -> invalid_arg "rename"
  in
  let incs = collect_signature_inclusions s in
  let ids, occs = propagate_renamings value_ops id incs in
  check_signature_inclusions value_ops ids name' occs;
  List.iter
    (function id ->
      Printf.eprintf "rename %s\n%!" (Ident.unique_name id))
    ids;
  let lids = get_lids file s in
  check_lids ids name' lids;
  let r = rename_lids ids name' lids in
  List.iter
    (function loc, s ->
      Printf.eprintf "replace %d--%d by %s\n%!"
	loc.loc_start.pos_cnum loc.loc_end.pos_cnum (Util.lid_to_str s))
    r;
  exit 0

(*
let pos fname cnum = {
  pos_fname = fname;
  pos_lnum = - 1;
  pos_bol = -1;
  pos_cnum = cnum
}

let _ =
  let f = Sys.argv.(1)
  and b = int_of_string Sys.argv.(2)
  and e = int_of_string Sys.argv.(3)
  and name' = Sys.argv.(4) in
  let c = open_in f in
  let lexbuf = Lexing.from_channel c in
  let ast = Parse.implementation lexbuf in
(*
  let env = Completion.initial_env () in
*)
  let env = Env.initial in
  let str, sg, _ = Typemod.type_structure env ast Location.none in
  let loc = {
    loc_start = pos f b; 
    loc_end = pos f e;
    loc_ghost = false
  } in
  let r = rename f str loc name' in
  List.iter
    (function loc, s ->
      Printf.eprintf "replace %d--%d by %s\n%!"
	loc.loc_start.pos_cnum loc.loc_end.pos_cnum (Util.lid_to_str s))
    r;
  exit 0
*)
