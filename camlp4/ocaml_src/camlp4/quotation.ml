(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Id *)

type expander =
    ExStr of (bool -> string -> string)
  | ExAst of ((string -> MLast.expr) * (string -> MLast.patt))
;;

let expanders_table = ref [];;

let default = ref "";;
let translate = ref (fun x -> x);;

let expander_name name =
  match !translate name with
    "" -> !default
  | name -> name
;;

let find name = List.assoc (expander_name name) !expanders_table;;

let add name f = expanders_table := (name, f) :: !expanders_table;;
