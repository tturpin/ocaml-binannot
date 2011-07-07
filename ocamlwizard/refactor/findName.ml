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

(* An alternative technique for collecting occurrences. *)
(*
(* Is that meaningful ? *)
let rec path2loc idents = function
  | Path.Pident s -> Location.StringTbl.find idents (Ident.name s)
  | Path.Pdot (p, s, _) ->
    let l = path2loc idents p
    and l' = Location.StringTbl.find idents s in
    {l with Location.loc_end = l'.Location.loc_end}
  | Path.Papply (p, p') -> failwith "not implemented"

(* We need to be complete for all the supported sorts, otherwise we
   have regression on value renaming, for example. *)
let rec env_of_node root_env father_table n =
  let up () =
    debug "father of %s is " (node_kind n);
    match NodeTbl.find_all father_table n with
      | [father] ->
	debugln "%s" (node_kind father);
	env_of_node root_env father_table father
      | [] -> debugln "root" ; root_env
      | _ -> failwith ("node " ^ node_kind n ^ " has multiple fathers")
  in
  match n with
    | `core_type _
    | `pattern _ -> up ()
    | `type_declaration _ -> up () (* faux a cause de la recursion ! *)
    | `structure_item { str_desc = Tstr_type _ ; str_env = env } ->
      debugln "found env in structure_item" ; env
    | `expression { exp_desc = (*Texp_constraint*) _ ; exp_env = env } -> env
    | `binding ({ pat_desc = Tpat_alias _}, _) -> up ()
    | `bindings _ -> assert false
    | _ -> failwith ("env_of_node: unsupported case " ^ node_kind n)

let rec check_same = function
  | [x] -> x
  | (_, lid, env) :: ((_, lid', env') :: _ as l) ->
    if lid != lid' then failwith "different longidents";
    if env != env' then failwith "different environments";
    check_same l
  | [] -> invalid_arg "check_same"

let lookup_path path_table p =
  match Env.PathTbl.find_all path_table p with
    | [] -> failwith ("path " ^ Path.name p ^ " not found")
    | l -> debugln "checking path %s" (Path.name p) ; check_same l

let shift n loc =
  let start = loc.Location.loc_start in
  { loc with Location.loc_start =
      { start with Lexing.pos_cnum = start.Lexing.pos_cnum + n }
  }

(* This should only be complete w.r.t. values and module paths ! But
   we cannot have safe renaming for modules until we are complete
   w.r.t. paths of all sorts. *)
let find_all_occurrences env idents paths tree =
  let father_table = reverse tree in
  let found loc env occ = Some (loc, (env, occ))
(*
  and loc n =
    let loc = path2loc idents n in
    let b, e = get_c_num loc in
    debugln "loc = [%d, %d[" b e;
    loc
*)
(*
  and env = env_of_node env father_table in
*)
  and env p =
    let sort, lid, env = lookup_path paths p in
(*
    let loc = lid2loc
*)
    env
  in
  find_all_map
    (function n ->
      match n with

      | `pattern p ->
	(match p.pat_desc with
	  | Tpat_alias (_, TPat_type path) ->
	    found (shift 1 p.pat_loc) (env path) Env.Type
	  | _ -> None)

      | `core_type t ->
	(match t.ctyp_desc with
	  | Ttyp_constr (p, _) ->
	    found t.ctyp_loc (env p) Env.Type
	  | _ -> None)

      | `expression e ->
	(match e.exp_desc with
	  | Texp_ident _ -> found e.exp_loc e.exp_env Env.Value
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
	  | Tmod_ident _ -> found m.mod_loc m.mod_env Env.Module
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
	  | Tstr_open _ -> found (shift 4 i.str_loc) i.str_env Env.Module
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

let get_occurrences env idents lidents paths s =
  List.sort
    (fun (loc, _) (loc', _) ->
      let open Lexing in
      compare loc.loc_start.pos_cnum loc.loc_end.pos_cnum)
    (find_all_occurrences env idents paths s)
*)

let reverse t =
  let r = Longident.LongidentTbl.create 1000 in
  Env.PathTbl.iter
    (fun p (sort, lid, env) ->
      if sort <> Env.Annot then
	Longident.LongidentTbl.add r lid (sort, p, env))
    t;
  r
    
let kind2kind = function
  | Env.Value -> value_ops
  | Env.Type -> type_ops
  | Env.Annot
  | Env.Constructor
  | Env.Label
  | Env.Module
  | Env.Modtype
  | Env.Class
  | Env.Cltype
    -> raise Not_found

let kind2str = function
  | Env.Value -> "value"
  | Env.Type -> "type"
  | Env.Annot -> "annot"
  | Env.Constructor -> "constructor"
  | Env.Label -> "label"
  | Env.Module -> "module"
  | Env.Modtype -> "modtype"
  | Env.Class -> "class"
  | Env.Cltype -> "cltype"

let rec check_same = function
  | [x] -> x
  | (kind, p, env) :: ((kind', p', env') :: _ as l) ->
    if kind <> kind' then
      failwith (kind2str kind ^ " <> " ^ kind2str kind');
    if p != p' then failwith "different paths";
    if env != env' then failwith "different environments";
    check_same l
  | [] -> invalid_arg "check_same"

let get_occurrences env idents lidents paths s =
  let lid2path = reverse paths in
  (* We should check that keys are bound only once *)
  Longident.LongidentTbl.fold
    (fun lid loc acc ->
      match Longident.LongidentTbl.find_all lid2path lid with
	| [] ->
	  debugln "lident %s has no environment" (lid_to_str lid);
	  acc
	| l ->
	  debugln "testing %s" (lid_to_str lid);
	  let kind, p, env = check_same l in
	  if kind = Env.Value || kind = Env.Type then
	    (loc, (env, kind)) :: acc
	  else
	    acc)
    lidents
    []

let extract_longident (loc, s, (env, occ)) =
  let kind = match occ with
    | Env.Value -> value_ops
    | Env.Module -> module_ops
    | Env.Type -> type_ops
    | _ -> failwith "not implemented"
  in
  let ast =
    try
      kind.parse_lid s
    with _ ->
      failwith ("error parsing the following ident: " ^ s)
  in
    (loc, ast, (env, kind))

let get_lids env file idents lidents paths ast =
  List.map
    extract_longident
    (source_locations file (get_occurrences env idents lidents paths ast))

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
