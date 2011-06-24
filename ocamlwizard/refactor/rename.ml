open Lexing
open Location
open Longident
open Path
open Types
open Typedtree
open TypedtreeOps
open Env



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

let rec path2lident = function
  | Pident i -> Lident (Ident.name i)
  | Pdot (p, n, _) -> Ldot (path2lident p, n)
  | Papply (p, p') -> Lapply (path2lident p, path2lident p')

(*
type ident_ctx =
  | Rpat_var of pattern * pattern_context
  | Rstr_module of structure_item * structure_item_ctx

and pattern_ctx =
  | Rexp_let of expression * expression_ctx
  | Rexp_function of expression * expression_ctx
  | Rexp_match of expression * expression_ctx
  | Rexp_try of expression * expression_ctx

and expression_ctx =
  | Rexp_let_left of int * expression * expression_ctx
  | Rexp_let_right of expression * expression_ctx
  | Rstr_eval of expression * expression_ctx
  | Rstr_value of int * expression * expression_ctx
*)

(*
let resolve item = function
    | Pident i -> item, i
    | Pdot (p, n, _) ->
      let item, i = resolve p in
    | Papply (p, p') -> assert false
*)

type sort = [
  | `Module
  | `Modtype
  | `Value
]

type specifics = {
  sort : sort;
  lookup : Longident.t -> Env.t -> Path.t;
  sig_item : Types.signature_item -> Ident.t option;
  summary_item : Env.summary -> Ident.t option
}

let keep_first f lid env = fst (f lid env)

let value_ops = {
  sort = `Value;
  lookup = keep_first Env.lookup_value;
  sig_item = (function Sig_value (i, _) -> Some i | _ -> None);
  summary_item = function Env_value (_, i, _) -> Some i | _ -> None
}

let module_ops = {
  sort = `Module;
  lookup = keep_first Env.lookup_module;
  sig_item = (function Sig_module (i, _, _) -> Some i | _ -> None);
  summary_item = function Env_module (_, i, _) -> Some i | _ -> None
}

(* Return the signature of a given (extended) module type path *)
let rec resolve_modtype env path =
  match Env.find_modtype path env with
  | Modtype_abstract -> invalid_arg "resolve_mod_type"
  | Modtype_manifest mt -> modtype_signature env mt

and modtype_signature env = function
  | Mty_ident p -> resolve_modtype env p
  | Mty_signature s -> s
  | Mty_functor _ -> assert false

(* Return the signature of a given (extended) module path *)
let resolve_module env path =
  modtype_signature env (Env.find_module path env)

(* True if p.name means id *)
let field_resolves_to kind env path name id =
  name = Ident.name id && (* only an optimisation *)
  List.exists
    (function s -> kind.sig_item s = Some id)
    (resolve_module env path)

(* Test whether a p reffers to id in environment env. This indicates
   that the rightmost name in lid needs renaming. *)
let resolves_to kind env lid id =
  match kind.lookup lid env with
    | Pident id' -> Ident.same id id'
    | Pdot (p, n, _) -> field_resolves_to kind env p n id
    | Papply _ -> invalid_arg "resolves_to"

exception Not_masked
exception Masked_by of Ident.t


(* Check that the renaming of id in name is not masked in the env. *)
let check_in_sig kind id name env =
  List.iter
    (function item ->
      (match kind.sig_item item with
	| Some id' ->
	  if Ident.same id' id then
	    raise Not_masked
	  else if Ident.name id' = name then
	    raise (Masked_by id')
	| None -> ()))

(* Check that the renaming of id in name is not masked in the env. *)
let rec check kind id name env = function
  | Env_empty -> raise Not_found
  | Env_open (s, p) ->
    let sign = resolve_module env p in
    check_in_sig kind id name env sign;
    check kind id name env s
  | summary ->
    (match kind.summary_item summary with
      | Some id' ->
	if Ident.same id' id then
	  raise Not_masked
	else if Ident.name id' = name then
	  raise (Masked_by id')
      | None -> ());
    match summary with
      | Env_value (s, _, _)
      | Env_type (s, _, _)
      | Env_exception (s, _, _)
      | Env_module (s, _, _)
      | Env_modtype (s, _, _)
      | Env_class (s, _, _)
      | Env_cltype (s, _, _)
	-> check kind id name env s
      | Env_open _ | Env_empty _ -> assert false

let check kind id name env summary =
  try
    ignore (check kind id name env summary);
    assert false
  with
      Not_masked -> ()

(* The following it an attempt to solve the renaming in two steps,
   (for module paths, then for arbitrary paths) but it does not seem
   to simplify the second step, so we do all cases at the same time. *)

(* Rename a module name in an extended module path. *)
let rec rename_in_ext_mod_path
    (env : Env.t)
    (id : Ident.t)
    (name' : string)
    (lid : Longident.t) =
  let rename = rename_in_ext_mod_path env id name' in
  match lid with
    | Lident i ->
      if resolves_to module_ops env lid id then (
	check module_ops id name' env (Env.summary env);
	Some (Lident name')
      ) else
	None
    | Ldot (lid', n) ->
      let n' =
	if resolves_to module_ops env lid id then
	  Some name'
	else
	  None
      and lid' = rename lid' in
      (match lid', n' with
	| None, None -> None
	| None, Some n -> Some (Ldot(lid, n))
	| Some lid, None -> Some (Ldot(lid, n))
	| Some lid, Some n -> Some (Ldot(lid, n)))
    | Lapply (lid, lid') ->
      (match rename lid, rename lid' with
	| None, None -> None
	| Some lid, None -> Some (Lapply (lid, lid'))
	| None, Some lid' -> Some (Lapply (lid, lid'))
	| Some lid, Some lid' -> Some (Lapply (lid, lid')))

(*
let rec rename_in_lid
    renamed_kind
    (env : Env.t)
    (id : Ident.t)
    (name' : string)
    kind
    (lid : Longident.t) =
  match renamed_kind.sort with
    | `Module -> rename_in_ext_mod_path env id name'
    | _ ->
      match lid with
	| Lident i ->
      let p, _ = renamed_kind.lookup lid env in
      if kind.sort = renamed_kind.sort &&
	resolves_to renamed_kind env id p then (
	  check_value id name' env (Env.summary env);
	  Some (Lident name')
	) else
  	  None
    | _, Ldot (lid, n) ->
      let p, _ = renamed_kind.lookup lid env in
      if kind.sort = renamed_kind.sort && field_resolves_to kind env p n id then
	Some (Ldot(lid, name'))
      else
	None
    | _, Lapply _ -> invalid_arg "rename_in_lid"
*)

(* Rename the ident id of type renamed_kind in the longident lid of kind sort *)
let rec rename_in_lid
    renamed_kind
    (env : Env.t)
    (id : Ident.t)
    (name' : string)
    kind
    (lid : Longident.t) =
  let rename = rename_in_lid renamed_kind env id name' module_ops in
  match renamed_kind.sort, lid with
    | _, Lident i ->
      if kind.sort = renamed_kind.sort && resolves_to kind env lid id then (
	check kind id name' env (Env.summary env);
	Some (Lident name')
      ) else
	None
    | _, Ldot (pref, n) ->
      let n' =
	if kind.sort = renamed_kind.sort && resolves_to kind env lid id then
	  Some name'
	else
	  None
      and pref' = rename pref in
      (match pref', n' with
	| None, None -> None
	| None, Some n -> Some (Ldot(pref, n))
	| Some pref, None -> Some (Ldot(pref, n))
	| Some pref, Some n -> Some (Ldot(pref, n)))
    | `Module, Lapply (lid, lid') ->
      (match rename lid, rename lid' with
	| None, None -> None
	| Some lid, None -> Some (Lapply (lid, lid'))
	| None, Some lid' -> Some (Lapply (lid, lid'))
	| Some lid, Some lid' -> Some (Lapply (lid, lid')))
    | _, Lapply _ -> None

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
	 (*
	   let enter_pattern p =
	   let enter_module_expr e =
	   val enter_structure : structure -> unit
	   val enter_value_description : value_description -> unit
	   val enter_type_declaration : type_declaration -> unit
	   val enter_exception_declaration :
	   exception_declaration -> unit
	   val enter_pattern : pattern -> unit
	   val enter_package_type : package_type -> unit
	   val enter_signature : signature -> unit
	   val enter_signature_item : signature_item -> unit
	   val enter_modtype_declaration : modtype_declaration -> unit
	   val enter_module_type : module_type -> unit
	   val enter_module_expr : module_expr -> unit
	   val enter_with_constraint : with_constraint -> unit
	   val enter_class_expr : class_expr -> unit
	   val enter_class_signature : class_signature -> unit
	   val enter_class_description : class_description -> unit
	   val enter_class_type_declaration :
	   class_type_declaration -> unit
	   val enter_class_infos : 'a class_infos -> unit
	   val enter_class_type : class_type -> unit
	   val enter_class_type_field : class_type_field -> unit
	   val enter_core_type : core_type -> unit
	   val enter_core_field_type : core_field_type -> unit
	   val enter_class_structure : class_structure -> unit
	   val enter_class_field : class_field -> unit
	   val enter_structure_item : structure_item -> unit

	   val enter_bindings : rec_flag -> unit      
	   val enter_binding : pattern -> expression -> unit
	 *)
	 end)
  in
  Rename.iter_structure s;
  !l

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

let rename_lids id name' lids =
  List.fold_left
    (fun l (lid, e) ->
      match rename_in_lid value_ops e.exp_env id name' value_ops lid with
	| Some lid ->
	  (e.Typedtree.exp_loc, lid) :: l
	| None -> l)
    []
    lids

let same_loc l l' =
  Util.get_c_num l = Util.get_c_num l'

let locate_pattern s loc =
  let pattern p =
    if same_loc p.pat_loc loc then
      Some p
    else
      None
  in
  (find_pattern pattern).structure s

let rename file s loc name' =
  let id =
    match (locate_pattern s loc).pat_desc with
      | Tpat_var id -> id
      | _ -> invalid_arg "rename"
  in
  rename_lids id name' (get_lids file s)

let pos fname cnum = {
  pos_fname = fname;
  pos_lnum = - 1;
  pos_bol = -1;
  pos_cnum = cnum
}

(*
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
