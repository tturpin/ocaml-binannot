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

(** This module is used to type-check when the user request for
    a pattern matching completion with type-checking *)

open Typedtree

exception Partial_env of Env.t * Types.type_expr

let rec iter_pat_var loc f (p: pattern) = 
  if Util.get_c_num p.pat_loc = loc then f p else
  let iter = iter_pat_var loc f in
  match p.pat_desc with 
    | Tpat_any
    | Tpat_var _ 
    | Tpat_constant _ -> ()
    | Tpat_alias (p, _) -> iter p
    | Tpat_tuple pl 
    | Tpat_array pl 
    | Tpat_construct (_, _, pl) -> List.iter iter pl
    | Tpat_variant (_, Some p, _) -> iter p
    | Tpat_variant _ -> ()
    | Tpat_record (l, _) -> List.iter (fun (_, _, p) -> iter p) l
    | Tpat_or (p1, p2, _) -> iter p1; iter p2

let check_for_pat_var cases =
  if !Common_config.command = Common_config.Completion then
    let display p = raise (Partial_env (p.pat_env, p.pat_type)) in
    List.iter (fun (p,_) -> 
      iter_pat_var !Common_config.loc display p) cases

