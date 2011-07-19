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
open Resolve
open FindName
open RenameLid
open RenamePropagation

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
    (function _, _, lid, env, kind ->
      check_lid renamed_kind id name' env kind lid)
    lids

let rename_lids renamed_kind id name' lids =
  filter_map
    (fun (source, loc, lid, env, kind) ->
      match rename_in_lid renamed_kind id name' env kind source lid with
	| Some lid ->
	  Some (
	    loc,
(*
	    source,
	    loc.loc_start.pos_cnum,
	    loc.loc_end.pos_cnum,
*)
	    Util.lid_to_str lid
	  )
	| None -> None)
    (List.rev lids)

let classify_source f =
  let open Filename in
    if
      check_suffix f ".ml" then `ml
    else if
      check_suffix f ".mli" then `mli
    else
      invalid_arg "not an OCaml source file"

let typedtree_file source_kind f =
  Filename.chop_extension f ^
    match source_kind with
      | `ml -> ".cmt"
      | `mli -> ".cmti"

let read_typedtree sort file =
  let c = open_in file in
  let data = input_value c in
    close_in c;
    let tree =
      match data.(0) with
	| Saved_implementation str -> `structure str
	| Saved_signature sg -> `signature sg
	| _ -> fail_owz "error reading cmt(i) file"
    in
      try
	match data.(1), data.(2), data.(3) with
	  | Saved_ident_locations (Some loc),
	    Saved_longident_locations (Some lloc),
	    Saved_path_environments (Some env) ->
	    (sort tree), loc, lloc, env
	  | _ -> raise Not_found
      with
	  _ -> fail_owz
	    "ident location or path environment table not found in cmt(i)"

let read_cmi file =
  (* Copied from env.ml *)
  let ic = open_in_bin file in
  let buffer = String.create (String.length Config.cmi_magic_number) in
  really_input ic buffer 0 (String.length Config.cmi_magic_number);
  if buffer <> Config.cmi_magic_number then begin
    close_in ic;
    fail_owz "error reading cmi file"
  end;
  let (_name, (sign : Types.signature)) = input_value ic in
  let _crcs = input_value ic in
  let _flags = input_value ic in
  close_in ic;
  sign

(* Read the cmt (if any), cmti (if any), and cmi. *)
let read_one_unit f =
  let prefix = Filename.chop_extension f in
  debugln "parsing %s" prefix;
  let cmi = read_cmi (prefix ^ ".cmi") in
  let sort x = x in
  let parse kind source parsetree l =
    let source = prefix ^ source
    and parsetree = prefix ^ parsetree in
    if Sys.file_exists source then (
      if not (Sys.file_exists parsetree) then
	fail_owz "error reading cmt(i) file for %s" f;
      (* We should also check the modification times *)
      let ast, loc, lloc, env = read_typedtree sort parsetree in
      ((prefix, kind), (ast, loc, lloc, env, cmi)) :: l
    ) else
      l
  in
  parse `ml ".ml" ".cmt" (parse `mli ".mli" ".cmti" [])

let read_all_files dirs =
  List.fold_left
    (fun files d ->
      let fs = Array.to_list (Sys.readdir d) in
      let mls =
	List.concat
	  (filter_map
	     (function f ->
	       let open Filename in
		   let f = concat d f in
		   if (check_suffix f ".ml" &&
			 not (Sys.file_exists (f ^ "i"))) ||
		     check_suffix f ".mli" then
		     Some (read_one_unit f)
		   else
		     None)
	     fs)
      in
      files @ mls)
    []
    dirs

let sort_replaces =
  List.sort
    (* This comparison is total because def locations are either
       disjoint or identical *)
    (fun (x, _, _) (y, _, _) -> compare  x y)

let rec remove_duplicates = function
  | x :: (y :: _ as l) ->
    if x = y then
      remove_duplicates l
    else
      x :: remove_duplicates l
  | l -> l

let hashtbl_keys t =
  Hashtbl.fold
    (fun k _ l ->
      match l with
	| k' :: _ as l when k = k' -> l
	| l -> k :: l)
    t
    []

let sort_replaces files replaces =
  let t = Hashtbl.create 2 in
  List.iter
    (function loc, rep ->
      let basename = Filename.basename loc.loc_start.pos_fname in
      let (prefix, _), _ =
	List.find
	  (function (prefix, _ ), _ ->
	    Filename.basename prefix = Filename.chop_extension basename)
	  files
      in
      let fname = Filename.concat (Filename.dirname prefix) basename in
      Hashtbl.add t fname
	(loc.loc_start.pos_cnum, loc.loc_end.pos_cnum, rep))
    replaces;
  List.map
    (function f ->
      f, remove_duplicates (sort_replaces (Hashtbl.find_all t f)))
    (hashtbl_keys t)

let string_table_union t t' =
  let open Location.StringTbl in
      let t = copy t in
      iter
	(fun x l -> add t x l)
	t';
      t

let find_id_defs files ids name =
  filter_map
    (fun (loc, id) ->
      match loc with
	| `pers _ -> None
	| `source (fname, _ as f) ->
	  debugln "Rename %s in %s"(Ident.name id) fname;
	  let _, idents, _, _, _ = List.assoc f files in
	    try
	      let loc =
		Locate.ident_def idents id
	      in
	      Some (loc, name)
	    with Not_found ->
	      None
	(* Why three idents defs in renameProp ? *)
(*
	      fail "ident %s not found" (Ident.name id)
*)
	| _ -> invalid_arg "find_id_defs")
    ids

let get_lids files =
  List.concat
    (List.map
       (function source, (ast, loc, lloc, lid2env, cmi) ->
	 List.map (function loc, lid, (env, kind) -> source, loc, lid, env, kind) (get_lids lloc lid2env ast))
       files)

let fix_case kind =
  match kind with
    | Env.Module | Env.Modtype | Env.Constructor -> String.capitalize
    | _ -> String.uncapitalize

let backup file =
  let backup = file ^ ".backup_" ^ string_of_int (int_of_float (Unix.time ())) in
  if Sys.file_exists backup then
    failwith "bad luck"
  else
    Edit.cp file backup

(* Rename an ident in a list of source files. *)
let rename_in_files env renamed_kind id file new_name files =

  (* Collect constraints requiring simultaneous renaming and deduce
     the minimal set of ids to rename *)
  let constraints, includes = constraints_all_files env renamed_kind id files in
  let ids, implicit_refs =
    propagate file renamed_kind id files constraints includes in

  debugln "found %d idents to rename" (List.length ids);

  (* Compute the replacements for the *definitions* of the rename ids *)
  let def_replaces = find_id_defs files ids new_name in

  (* Check that our new name will not capture useful signature members *)
  check_other_implicit_references renamed_kind ids new_name constraints includes;

  (* Check that useful renamed signature members are not masked. *)
  check_renamed_implicit_references renamed_kind ids new_name implicit_refs;

  (* Collect all lids *)
  let lids = get_lids files in

  (* Check that our new name will not capture other occurrences *)
  check_lids renamed_kind (Ident.name id) new_name lids;

  (* Compute renamed lids, checking that they are not captured *)
  let occ_replaces = rename_lids renamed_kind ids new_name lids in

  def_replaces, occ_replaces

(* Renaming entry point: user interface... *)
let rename loc new_name file =

  backup file;

  (* Setup the environment *)
  let dirs, current = Common_config.project_dirs file in
  Config.load_path := "" :: List.rev_append dirs (Clflags.std_include_dir ());
  let env = initial_env () in (* Make sure that Pervasives is loaded *)
  debugln "load_path:"; List.iter (debugln "  %s") !Config.load_path;

  (* Check that everything is up-to-date *)
  if Common_config.has_auto_save file then
    fail_owz "buffer must be saved before renaming";
  let source_kind = classify_source file in
  let prefix = Filename.concat (Sys.getcwd ()) (Filename.chop_extension file) in
  debugln "prefix = %s" prefix;
  let typedtree_file = typedtree_file source_kind file in
  if not (Sys.file_exists typedtree_file) then
    fail_owz "no cmt(i) file for %s" file;
(*
  if Unix.((stat file).st_mtime > (stat typedtree_file).st_mtime) then
    fail_owz "cmt(i) file is older than source file";
*)

  (* Read the typedtrees *)
  let files = read_all_files dirs in
  let s, idents, lidents, paths, _ = List.assoc (prefix, source_kind) files in

  (* Get the "initial" id to rename and its sort and location *)
  let renamed_kind, id =
    try Locate.longident idents loc s
    with Not_found -> fail_owz "Cannot rename anything here"
  in
  let loc = Locate.ident_def idents id in

  let old_name = Ident.name id in

  let new_name = fix_case renamed_kind new_name in

  let defs, occs =
    try
      rename_in_files env renamed_kind id (prefix, source_kind) new_name files
    with
	Masked_by (renamed, id) ->
	  Location.print Format.std_formatter loc;
	  if renamed then
	    fail_owz
	      "This existing definition of %s would capture an occurrence of %s"
	      new_name old_name
	  else
	    fail_owz
	      "This definition of %s that you are trying to rename would \
                 capture an occurrence of an existing definition of %s"
	      old_name new_name
  in

  let replaces = sort_replaces files (defs @ occs) in

  try
    (* Replace lids in the source file *)
    List.iter
      (function file, replaces ->
	debugln "replace in %s" file;
	Edit.edit replaces file)
      replaces;
    Printf.printf "Renamed %d definition(s) and %d reference(s) in %d file(s)"
      (List.length defs) (List.length occs) (List.length replaces)
  with
      e ->
	Printf.printf "Renaming failed while editing the files !\n";
	raise e
