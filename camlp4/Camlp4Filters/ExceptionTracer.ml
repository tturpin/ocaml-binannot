(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright   2006    Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)


open Camlp4;

module Id = struct
  value name    = "Camlp4Filters.ExceptionTracer";
  value version = "$Id$";
end;

module Make (AstFilters : Camlp4.Sig.AstFilters.S) = struct
  open AstFilters;
  open Ast;

  value add_debug_expr e =
    (* let _loc = Loc.make_absolute (MLast.loc_of_expr e) in *)
    let _loc = Ast.loc_of_expr e in
    let msg = "camlp4-debug: exc: %s at " ^ Loc.to_string _loc ^ "@." in
    <:expr<
        try $e$
        with
        [ Stream.Failure | Exit as exc -> raise exc
        | exc -> do {
            if Debug.mode "exc" then
              Format.eprintf $`str:msg$ (Printexc.to_string exc) else ();
            raise exc
          } ] >>;

  value rec map_match_rule =
    fun
    [ <:match_case@_loc< $m1$ | $m2$ >> ->
        <:match_case< $map_match_rule m1$ | $map_match_rule m2$ >>
    | <:match_case@_loc< $p$ when $w$ -> $e$ >> ->
        <:match_case@_loc< $p$ when $w$ -> $add_debug_expr e$ >>
    | m -> m ]

  and map_expr =
    fun
    [ <:expr@_loc< fun [ $m$ ] >> -> <:expr< fun [ $map_match_rule m$ ] >>
    | x -> x ];

  register_str_item_filter (new Ast.c_expr map_expr)#str_item;

end;

let module M = Camlp4.Register.AstFilter Id Make in ();