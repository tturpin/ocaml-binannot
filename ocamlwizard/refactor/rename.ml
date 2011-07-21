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

let check_lids renamed_kind ids name' lids =
  List.iter
    (function source, _, lid, env, kind ->
      check_lid renamed_kind ids name' env kind source lid)
    lids

let rename_lids renamed_kind id name' lids =
  filter_map
    (fun (source, loc, lid, env, kind) ->
      match rename_in_lid renamed_kind id name' env kind source lid with
	| Some lid ->
	  Some (
	    loc,
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

let read_typedtree sort = Profile.time_call "read_typedtree" (read_typedtree sort)

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

let read_cmi = Profile.time_call "read_cmi" read_cmi

(* Read the cmt (if any), cmti (if any), and cmi. *)
let read_unit f =
  let prefix = Filename.chop_extension f in
  debugln "parsing %s" prefix;
  let cmi = read_cmi (prefix ^ ".cmi") in
  let sort x = x in
  let parse kind source typedtree l =
    let source = prefix ^ source
    and typedtree = prefix ^ typedtree in
    if Sys.file_exists source then (

      (* Check that everything is up-to-date *)
      if Common_config.has_auto_save source then
	fail_owz "file %s must be saved before renaming" source;
      if not (Sys.file_exists typedtree) then
	fail_owz "no cmt(i) file for %s" source;
      if Unix.((stat source).st_mtime > (stat typedtree).st_mtime) then
	fail_owz "cmt(i) file is older than source file";

      (* We should also check the modification times *)
      let ast, loc, lloc, env = read_typedtree sort typedtree in
      ((prefix, kind), (ast, loc, lloc, env, cmi)) :: l
    ) else
      l
  in
  parse `ml ".ml" ".cmt" (parse `mli ".mli" ".cmti" [])

let read_one_unit = Profile.time_call "read_one_unit" read_unit

(* Given a set of directories, return the set of (qualified) source
   file names contained in those directories. *)
let project_files dirs =
  List.fold_left
    (fun files d ->
      let fs = Array.to_list (Sys.readdir d) in
      let mls =
	filter_map
	  (function f ->
	    let open Filename in
		let f = concat d f in
		if check_suffix f ".ml" || check_suffix f ".mli" then
		  Some f
		else
		  None)
	  fs
      in
      files @ mls)
    []
    dirs

(* Read a list of (qualified) compilation unit names. *)
let read_units files =
  List.concat
    (filter_map
       (function f ->
	 let open Filename in
	     if check_suffix f ".ml" && not (Sys.file_exists (f ^ "i")) ||
	       (* do not duplicate units which have both a .ml and .mli *)
	       check_suffix f ".mli" then
	       Some (read_one_unit f)
	     else
	       None)
       files)

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

let source_of_loc files loc =
  let fname = loc.loc_start.pos_fname in
  let basename = Filename.basename fname in
  fst
    (List.find
       (function (prefix, source_kind), _ ->
	 Filename.basename prefix = Filename.chop_extension basename &&
	   classify_source fname = source_kind)
       files)

(** Sort a list of locations by file and order. *)
let sort_locations files locs =
  let t = Hashtbl.create 2 in
  List.iter
    (function loc ->
      let basename = Filename.basename loc.loc_start.pos_fname in
      let (prefix, _) = source_of_loc files loc in
      let fname = Filename.concat (Filename.dirname prefix) basename in
      Hashtbl.add t fname loc)
    locs;
  List.map
    (function f ->
      f,
      remove_duplicates
	(List.sort
	   (fun l l' -> compare l.loc_start.pos_cnum l'.loc_start.pos_cnum)
	   (* This comparison is total because def locations are
	      either disjoint or identical. *)
	   (Hashtbl.find_all t f)))
    (hashtbl_keys t)

let sort_replaces files replaces =
  let locs = List.map fst replaces in
  List.map
    (function f, locs ->
      f,
      List.map
	(function loc ->
	  let rep = List.assoc loc replaces in
	  loc.loc_start.pos_cnum, loc.loc_end.pos_cnum, rep)
	locs)
    (sort_locations files locs)

let find_id_defs files ids name =
  filter_map
    (fun (loc, id) ->
      match loc with
	| `pers _ -> None
	| `source (fname, _ as f) ->
	  debug "Rename %s in %s"(Ident.name id) fname;
	  let _, idents, _, _, _ = List.assoc f files in
	  try
	    let loc =
	      Locate.ident_def idents id
	    in
	    debug " at " ; Location.print Format.err_formatter loc;
	    Some (loc, name)
	  with Not_found ->
	    debugln " implicit";
	    fail "ident %s not found" (Ident.name id)
	| _ -> invalid_arg "find_id_defs")
    ids

let get_lids filter files =
  List.concat
    (List.map
       (function source, (ast, loc, lloc, lid2env, cmi) ->
	 List.map
	   (function loc, lid, (env, kind) ->
	     source, loc, lid, env, kind) (get_lids lloc lid2env filter ast))
       files)

let rec filter cond = function
  | Longident.Lident x -> cond x
  | Longident.Ldot (lid, x) -> filter cond lid || cond x
  | Longident.Lapply (lid, lid') -> filter cond lid || filter cond lid'

let filter old_name new_name lid =
  let cond n = n = old_name || n = new_name in
  filter cond lid &&
    not (List.mem (lid_to_str lid) ["false" ; "()"])

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

let make_absolute file =
  if Filename.is_relative file then
    Filename.concat (Sys.getcwd ()) file
  else
    file

let read_program file =

  let dirs, current, project_path = Common_config.project_dirs file in
  let file =
    Common_config.prefix_by (project_path @ current) (Filename.basename file) in

  (* Setup the load path *)
  Config.load_path := "" :: List.rev_append dirs (Clflags.std_include_dir ());
  debugln "load_path:"; List.iter (debugln "  %s") !Config.load_path;

  let source_kind = classify_source file in
  let prefix = Filename.chop_extension file in
  debugln "current = %s" (Common_config.list2path current);
  debugln "rel = %s" (Common_config.list2path project_path);
  debugln "prefix = %s" prefix;

  (* Read the typedtrees *)
  let files =
    match dirs with
      | [] -> [file]
      | _ -> project_files dirs
  in
  let files = read_units files in

  let env = initial_env () in (* Make sure that Pervasives is loaded *)

  (prefix, source_kind), files, env, current, project_path

(* Rename an ident in a list of source files. *)
let rename_in_files env renamed_kind id file new_name files =

  Profile.time_push "constraints";
  (* Collect constraints requiring simultaneous renaming and deduce
     the minimal set of ids to rename *)
  let constraints, includes = constraints_all_files env renamed_kind id files in

  Profile.time_switch_to "propagate";
  let ids, implicit_refs =
    propagate file renamed_kind id files constraints includes in

  debugln "found %d idents to rename" (List.length ids);

  Profile.time_switch_to "find id defs";
  (* Compute the replacements for the *definitions* of the rename ids *)
  let def_replaces = find_id_defs files ids new_name in

  Profile.time_switch_to "check other implicit";
  (* Check that our new name will not capture useful signature members *)
  check_other_implicit_references renamed_kind ids new_name constraints includes;

  Profile.time_switch_to "check renamed implicit";
  (* Check that useful renamed signature members are not masked. *)
  check_renamed_implicit_references renamed_kind ids new_name implicit_refs;

  Profile.time_switch_to "collect lids";
  (* Collect all lids *)
  let lids = get_lids (filter (Ident.name id) new_name) files in

  Profile.time_switch_to "check other lids";
  (* Check that our new name will not capture other occurrences *)
  check_lids renamed_kind ids new_name lids;

  Profile.time_switch_to "rename lids";
  (* Compute renamed lids, checking that they are not captured *)
  let occ_replaces = rename_lids renamed_kind ids new_name lids in

  Profile.time_pop ();
  def_replaces, occ_replaces

(* Collect an ident in a list of source files. *)
let grep_in_files env kind id file files =
  let constraints, includes = constraints_all_files env kind id files in
  let new_name = "invalid name" in
  let ids, _ = propagate file kind id files constraints includes in
  let defs = find_id_defs files ids new_name in
  let lids = get_lids (filter (Ident.name id) new_name) files in
  let occs = rename_lids kind ids new_name lids in
  List.map fst defs, List.map fst occs

let source_name = function
  | s, `ml -> s ^ ".ml"
  | s, `mli -> s ^ ".mli"

(* Renaming entry point: user interface... *)
let rename_point loc new_name file =

  (* Read the program *)
  Profile.time_push "read program";
  let source, files, env, _, _ = read_program file in
  let s, idents, lidents, paths, _ = List.assoc source files in

  (* Get the "initial" id to rename and its sort and location *)
  Profile.time_switch_to "locate longident";
  let renamed_kind, id =
    try Locate.longident idents loc s
    with Not_found -> fail_owz "Cannot rename anything here"
  in

  let old_name = Ident.name id in

  let new_name = fix_case renamed_kind new_name in

  Profile.time_switch_to "rename in files";
  let defs, occs =
    try
      rename_in_files env renamed_kind id source new_name files
    with
	Masked_by (renamed, (ctx, id)) ->
	  let loc =
	    match ctx with
	      | `source source ->
		(try
		   let _, idents, _, _, _ = List.assoc source files in
		   Locate.ident_def idents id
		 with Not_found -> Location.none)
	      | `pers _ -> assert false
	  in
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

  Profile.time_switch_to "sort_replace";
  let replaces = sort_replaces files (defs @ occs) in

  Profile.time_switch_to "apply renamings";
  (* Replace lids in the source file *)
  try
    List.iter
      (function file, replaces ->
	debugln "replace in %s" file;
	Profile.time_call "backup" backup file;
	Edit.edit replaces file)
      replaces;
    Printf.printf "Renamed %d definition(s) and %d reference(s) in %d file(s)"
      (List.length defs) (List.length occs) (List.length replaces);
    Profile.time_pop ();
    Profile.print_time_stats ()
  with
      e ->
	Printf.printf "Renaming failed while editing the files !\n";
	raise e

(* Grep entry point: user interface... *)
let grep_point loc file =

  let source, files, env, _, rel = read_program file in
  let s, idents, lidents, paths, _ = List.assoc source files in

  let kind, id =
    try Locate.longident idents loc s
    with Not_found -> fail_owz "No ident found here"
  in
  let name = Ident.name id in

  let defs, occs = grep_in_files env kind id source files in
  let show occs =
    let occs = sort_locations files occs in
    List.iter
      (function file, locs ->
	let lines = Array.of_list (lines_of file) in
	List.iter
	  (function loc ->
	    let source = source_of_loc files loc in
	    let pos_fname = source_name source in
(*
	    let loc = { loc with loc_start = {loc.loc_start with pos_fname}} in
	    Location.print Format.std_formatter loc;
*)
	    Printf.printf "%s:%d:" pos_fname loc.loc_start.pos_lnum;
	    Printf.printf "%s\n" lines.(loc.loc_start.pos_lnum-1))
	  locs)
      occs
  in
  Printf.printf "OCamlWizard grep:\n\n";
  Printf.printf "Definitions of %s %s:\n\n" (kind2string kind) name;
  show defs;
  Printf.printf "\nUses of %s %s:\n\n" (kind2string kind) name;
  show occs;
  Printf.printf "\nFound %d definition(s) and %d reference(s)\n"
    (List.length defs) (List.length occs)
