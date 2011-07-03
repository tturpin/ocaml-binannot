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
]

(* This should only be complete w.r.t. values and module paths ! But
   we cannot have safe renaming for modules until we are complete
   w.r.t. paths of all sorts. *)
module Occurrence =
  Find
    (struct
      type t = Location.t * (Env.t * occurrence_kind)
      module IteratorArgument(Action : sig val found : t -> unit end) = struct
	include DefaultIteratorArgument
	open Path

	let found loc env occ = Action.found (loc, (env, occ))

	let enter_expression e =
	  match e.exp_desc with
	    | Texp_ident _ -> found e.exp_loc e.exp_env `exp_ident
	    (* If the renamed ident is not a module or modtype,
	       then we could filter according to the right_most
	       ident. Otherwise, there is no way to know if we
	       need renaming until we get the longident. *)

	    | Texp_open _ -> found e.exp_loc e.exp_env `exp_open

	    (* No instance variables for now *)
	    | Texp_instvar (_self, var)
	    | Texp_setinstvar (_self, var, _) -> ()
	    | Texp_override (_self, modifs) -> ()

	    | _ -> ()

	let enter_module_expr m =
	  match m.mod_desc with
	    | Tmod_ident _ -> found m.mod_loc m.mod_env `mod_ident

	    | _ -> ()

	let enter_structure_item i =
	  match i.str_desc with
	    | Tstr_open m -> found m.loc (assert false) `str_open

	    | _ -> ()

(*
	let enter_module_type t =
	  match t.mty_desc with
	    | Tmty_with _ ->
*)
	let enter_with_constraint = function
	  | Twith_module _
	  | Twith_modsubst _ ->
	    found (assert false) (assert false) (assert false)

	  | _ -> ()

	let enter_signature_item i =
	  match i.sig_desc with
	    | Tsig_open _ -> found i.sig_loc (assert false) `sig_open

	    | _ -> ()

      end

     end)

let get_occurrences s =
  List.sort
    (fun (loc, _) (loc', _) ->
      let open Lexing in
      compare loc.loc_start.pos_cnum loc.loc_end.pos_cnum)
    (Occurrence.find_all (`structure s))

let get_longident (loc, s, (env, occ)) =
  let lexbuf = Lexing.from_string s in
  let parser, kind = match occ with
    | `exp_ident -> Parser.val_longident, value_ops
    | `mod_ident -> Parser.mod_longident, module_ops
    | _ -> failwith "not implemented"
  in
  let ast = (parser Lexer.token lexbuf).Longident.lid in
  (loc, ast, (env, kind))

let get_lids file ast =
  List.map
    get_longident
    (source_locations file (get_occurrences ast))


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

let ident_of_subtree = function
  | `pattern {pat_desc = Tpat_var id ; pat_loc = loc}
  | `expression {exp_desc = Texp_for (id, _, _, _, _) ; exp_loc = loc}
  | `signature_item {sig_desc = Tsig_value (id, _) ; sig_loc = loc}
    -> value_ops, id, loc
  | `structure_item {str_desc = Tstr_module (id, _) ; str_loc = loc}
    -> module_ops, id, loc
  | _ -> raise Not_found

(* Should be almost complete for expressions, but this is not a safety
   requirement anyway. *)
let locate_renamed_id s loc =
  try
    let kind, id, _ = ident_of_subtree (locate_innermost s loc) in kind, id
  with Not_found ->
    invalid_arg "rename"

let find_id_def s id =
  find_map_innermost s
    (function t ->
      try
	let _, id', loc = ident_of_subtree t in
	if id' = id then Some loc else None
      with Not_found -> None)
