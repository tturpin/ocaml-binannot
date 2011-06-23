open Longident
open Path
open Typedtree
open Types

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

let rec resolve_modtype env path =
  match Env.find_modtype path env with
  | Modtype_abstract -> invalid_arg "resolve_mod_type"
  | Modtype_manifest mt -> signature env mt

and signature env = function
  | Mty_ident p -> resolve_modtype env p
  | Mty_signature s -> s
  | Mty_functor _ -> assert false

let resolve_module env path =
  signature env (Env.find_module path env)

let mem find proj env path i =
  List.exists
    (function s -> proj s = Some i)
    (find env path)

let mem_value = mem resolve_module
  (function Sig_value (i, _) -> Some i | _ -> None)

let rec rename_in_path
    (env : Env.t)
(*
    (ctx : absolute_id option)
*)
    (id : Ident.t)
    (name : string)
    (name' : string)
(*
    (item : toplevel_item)
*)
    (path : Path.t) =
  let rename = rename_in_path env id name name' in
  match path with
    | Pident i ->
      if i =  id then
	Some (Lident name')
      else
	None
    | Pdot (p, n, _) -> (* what about this pos *)
      let n' =
	if n = name && mem_value env p id then
	  Some name'
	else
	  None
      and p' = rename p in
      (match p', n' with
	| None, None -> None
	| None, Some n' -> Some (Ldot(path2lident p, n))
	| Some p', None -> Some (Ldot(p', n))
	| Some p', Some n' -> Some (Ldot(p', n')))
    | Papply(p, p') ->
      match rename p, rename p' with
	| None, None -> None
	| Some p, None -> Some (Lapply (p, path2lident p'))
	| None, Some p' -> Some (Lapply (path2lident p, p'))
	| Some p, Some p' -> Some (Lapply (p, p'))

let rename_in_structure id name name' s =
  let l = ref [] in
  let module Rename =
	MakeIterator
	  (struct
	    include DefaultIteratorArgument

	    let enter_expression e =
	      match e.exp_desc with
		| Texp_ident (p, _) ->
		  (match rename_in_path e.exp_env id name name' p with
		    | Some lid ->
		      l := (e.Typedtree.exp_loc, lid) :: !l
		    | None -> ())
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

open TypedtreeOps

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

let rename s loc name' =
  let id =
    match (locate_pattern s loc).pat_desc with
      | Tpat_var id -> id
      | _ -> invalid_arg "rename"
  in
  let name = Ident.name id in
  rename_in_structure id name name' s

open Lexing
open Location

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
  let r = rename str loc name' in
  List.iter
    (function loc, s ->
      Printf.eprintf "replace %d--%d by %s"
	loc.loc_start.pos_cnum loc.loc_end.pos_cnum (Util.lid_to_str s))
    r
