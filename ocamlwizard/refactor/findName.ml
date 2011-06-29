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

(* This is extremely incomplete ! We cannot have safe renaming for
   modules until we are complete w.r.t. paths of all sorts. *)
module Occurrence =
  Find
    (struct
      type t = Location.t * (Env.t * specifics)
      module IteratorArgument(Action : sig val found : t -> unit end) = struct
	include DefaultIteratorArgument
	open Action

	let enter_expression e =
	  match e.exp_desc with
	    | Texp_ident _ ->
		  (* If the renamed ident is not a module or modtype,
		     then we could filter according to the right_most
		     ident. Otherwise, there is no way to know if we
		     need renaming until we get the longident. *)
	      found (e.exp_loc, (e.exp_env, value_ops))

	    | Texp_open _ -> (* We don't have the location. *)
	      ()

	    | _ -> ()

	let enter_module_expr m =
	  match m.mod_desc with
	    | Tmod_ident _ -> 
(* We can't parse them yet
	      found (m.mod_loc, (m.mod_env, module_ops))
*)
	      ()

	    | _ -> ()

	let enter_structure_item i =
	  match i.str_desc with
	    | Tstr_open _ -> (* We don't have the location. *)
	      ()

	    | _ -> ()

	let enter_module_type t =
	  match t.mty_desc with
	    | Tmty_with _ -> (* We don't have the location. *)
	      ()

	    | _ -> ()

	let enter_signature_item i =
	  match i.sig_desc with
	    | Tsig_open _ -> (* We don't have the location. *)
	      ()

	    | _ -> ()

      end

     end)

let get_occurrences s = Occurrence.find_all (`structure s)

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

let contains loc (b', e') =
  let b, e = Util.get_c_num loc in
  b <= b' && e' <= e

let locate_name s loc =
  let module M = Find (struct
    type t = [`pattern of pattern | `structure_item of structure_item]
    module IteratorArgument(Action : sig val found : t -> unit end) = struct
      include DefaultIteratorArgument
      open Action

      let leave_pattern p =
	if Util.get_c_num p.pat_loc = loc then
	  found (`pattern p)

      let leave_structure_item i =
	if contains i.str_loc loc then
	  found (`structure_item i)

    end
  end) in
  M.find s

