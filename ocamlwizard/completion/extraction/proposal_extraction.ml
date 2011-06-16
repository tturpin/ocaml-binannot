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

(**  *)

open Path_extraction
open Interface
open Format
open Debug
open Util
open Longident
open Parsetree    
open Types
open Path


(** *)
let complete_path sg ce se pi ty_check = 
  match pi.p_kd with
    | Module        -> C_module ( mk_modules sg ,pi.p_id )
    | Value vk    ->
      let pat = List.fold_right (fun m id -> m ^ "." ^ id) pi.p_md pi.p_id in
      C_value (
	mk_values sg se ty_check pat,
	vk,
	pat
      )
    | Record kd  -> C_record ( mk_records ce se pi ty_check kd, kd,pi.p_id)
    | _                   -> unreachable "Proposal_extraction" 2


(** *)
let is_basic_constr = function
  | Lident "[]" -> true
  | _ -> false

(* Idea: given prefix_lis the path containing the type, add this
   prefix progressively to the constructor until it is sufficiently
   qualified for the lookup to succeed and return the right type,
   according to its declaration location.

   We need a better way of doing that, based on a unique identification
   of all elements. *)
let best_qualif pat env prefix_lis typ =

  let rec qualif_lid lid = function
    | [] -> 
	begin
	  try 
	    let _, const = Env.lookup_constructor lid env  in
(*
	    if const.cstr_loc = owz_inf then lid else raise Exit
*)
	    Format.eprintf "%a = %a -> %B\n%!"
	      Printtyp.type_expr const.cstr_res
	      Printtyp.type_expr typ
	      (Types.TypeOps.equal const.cstr_res typ);
	    if const.cstr_res = typ then lid else raise Exit
	  with _ -> 
	    if !Common_config.debug then
	      Format.eprintf "| [] -> lookup_constr : %s@." (Util.lid_to_str lid);
	    
	    if List.length prefix_lis > 0 && "Unix" = List.hd prefix_lis then(
		if !Common_config.debug then
		  Format.eprintf "> Special case for Unix@.";
		lid
	      )else 
	      if Util.is_list lid then
		lid
	      else unreachable "Proposal_extraction" 3
	end
    | p :: r ->
	if !Common_config.debug then
	  Format.eprintf "| p :: r -> lookup_constr : %s@." 
	    (Util.lid_to_str lid);
	try 
	  let _, const = Env.lookup_constructor lid env in 
	  if const.cstr_res = typ then lid else raise Exit
	with _ -> 
	  let up_qualif = add_qualif (Lident p) lid in
	  qualif_lid up_qualif r
  in 
(*
  (* Temporary solution: full qualification *)
  let qualif_lid =
    List.fold_left
      (fun lid p -> add_qualif (Lident p) lid)
  in
*)
  match pat.ppat_desc with
    | Ppat_construct (lid, e1, e2) when lid <> Lident "[]" -> 
	{ pat with 
	  ppat_desc = 
	    let best_qualif = qualif_lid lid prefix_lis in
	    Ppat_construct (best_qualif, e1, e2)
	}
    | _ -> pat


(** *)
let best_qualification pat (env, main_ty ) prefix_lis owz_inf =
  best_qualif pat env owz_inf main_ty (*prefix_lis*)


let rec type_expr_path ty_exp = 
  match ty_exp.desc with
    | Tconstr (p, _, _) -> p
    | Tlink ty_exp -> type_expr_path ty_exp 
    | _ -> raise Exit
(*  
    | Tvar -> Format.eprintf "tvar@.";
    | Tarrow _ ->Format.eprintf "tarrow@.";
    | Ttuple _ -> Format.eprintf "ttuple@.";
    | Tobject _ -> Format.eprintf "tobject@.";
    | Tfield _ -> Format.eprintf "tfield@.";
    | Tnil ->  Format.eprintf "tnil@.";
    | Tsubst _ -> Format.eprintf "tsubst@.";
    | Tvariant _ -> Format.eprintf "tvariant@.";
    | Tunivar -> Format.eprintf "tunivar@.";
    | Tpoly _ -> Format.eprintf "tpoly@."
*)
  
(* Copied from Match_extraction *)
let mk_pattern desc = { ppat_desc=desc;ppat_loc=Location.none }

(* Return a longident which, in environment env, denotes the
   constructor c of type t. *)
let rev_lookup_cstr env tcstr tdecl cstr = Lident cstr

(* shortest_path cond p returns (cond p') for the shortest sub-path of
   p (inclusive) such that (cond p') does not raise Not_found. *)
let rec shortest_path cond = function
  | Lident _ as lid -> cond lid
  | Ldot (p, id) -> (
    try shortest_path cond (Lident id)
    with Not_found ->
      shortest_path
	(fun p -> cond (Ldot (p, id)))
	p
  )
  | Lapply (p, p') ->
    shortest_path
      (function p ->
	shortest_path
	  (function p' -> cond (Lapply (p, p')))
	  p')
      p

(* Provisoire, en attendant une solution definitive dans Untypeast *)
(* reverse lookup of a constructor of field name *)
let rev_lookup_member lookup res tcstr cstr =
  let p =
    match tcstr with
      | Pdot (p, _, _) -> Ldot (Untypeast.lident_of_path p, cstr)
      | Pident _ -> Lident cstr
      | _ -> invalid_arg "rev_lookup_cstr"
  in
  shortest_path
    (function p ->
      let _, cstr_desc = lookup p in
      match (res cstr_desc).Types.desc with
	| Tconstr (tcstr', _, _) ->
	  if tcstr' = tcstr then
	    p
	  else
	    raise Not_found
	| _ -> assert false
    )
    p

let rev_lookup_cstr env tcstr cstr =
  rev_lookup_member
    (function p -> Env.lookup_constructor p env)
    (function cstr_desc -> cstr_desc.cstr_res)
    tcstr
    cstr

let rev_lookup_field env tcstr field =
  rev_lookup_member
    (function p -> Env.lookup_label p env)
    (function field_desc -> field_desc.lbl_res)
    tcstr
    field

let infix = function
  | "::" -> true
  | _ -> false

let tuple_pattern ts =
  mk_pattern
    (Ppat_tuple
       (List.map (function _ -> mk_pattern Ppat_any) ts))

let variant_patterns env tcstr cstrs =
  List.map
    (function cstr, ts ->
      let arg =
	match ts with
	  | [] -> None
	  | _ ->
	    let arg =
	      if infix cstr
	      then
		tuple_pattern ts
	      else
		mk_pattern Ppat_any
	    in Some arg
      in
      mk_pattern
	(Ppat_construct
	   (rev_lookup_cstr env tcstr cstr, arg, false)))
    cstrs

let record_pattern env tcstr fields =
  mk_pattern
    (Ppat_record
       (List.map
	  (function field, _, t ->
	    (rev_lookup_field env tcstr field,
	     mk_pattern Ppat_any))
	  fields,
	Asttypes.Closed))

let polymorphic_variant_patterns r_desc =
  (* We should look at row_more too *)
  List.map
    (function lbl, field ->
      let arg = match field with
	| Rpresent (Some _) -> Some (mk_pattern Ppat_any)
	| Rpresent None -> None
	| _ -> assert false
      in
      mk_pattern (Ppat_variant (lbl, arg)))
    r_desc.row_fields

(* Simple version. *)
let rec patterns env t =
  match t.Types.desc with
    | Tconstr (tcstr, [_], _) when Path.name tcstr = "array" ->
      [mk_pattern (Ppat_array [])]
    | Tconstr (tcstr, [_], _) when Path.name tcstr = "lazy_t" ->
      [mk_pattern (Ppat_lazy (mk_pattern Ppat_any))]
    | Tconstr (tcstr, args, _) -> (
      let tdecl = Env.find_type tcstr env in
      debugln "%s" (Path.name tcstr);
      match tdecl.Types.type_kind with
	| Type_abstract -> [mk_pattern Ppat_any]
	| Type_variant cstrs -> variant_patterns env tcstr cstrs
	| Type_record (fields, _) -> [record_pattern env tcstr fields]
    )
    | Tvariant r_desc -> polymorphic_variant_patterns r_desc
    | Ttuple ts -> [tuple_pattern ts]
    | Tlink t | Tsubst t -> patterns env t
    | Tfield _ -> failwith "field"
    | Tvar _ -> failwith "var"
    | Tarrow (_, _, t, _) -> patterns env t
    | Tpackage (_, _, _)
    | Tpoly (_, _)
    | Tobject (_, _)
    | Tunivar
    | Tnil ->
      [mk_pattern Ppat_any]

(** @return a list of pattern to complete the current match *)
let complete_match ce pm_comp (env, t) =
  let ps = patterns env t in
  C_match (ME [1, ps], pm_comp)

let main sg ce se ty_check = 
  match se.comp with
    | Match pm_comp -> complete_match ce pm_comp ty_check
    | Path  pc -> complete_path sg ce se pc ty_check
    | Try pm -> assert false
    | Other -> assert false
    | Error e -> raise e
