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

(** This modules extracts the values of match expression's type and
    builds a list of patterns (match cases) *)


val build_match_comp :
  int ->
  Interface.completion_infos ->
  Interface.syntax_env ->
  string list -> bool -> Outcometree.out_type -> Outcometree.out_type
(** returns the main type of the match expression's type of performs
    some simplifications on standard types *)
  
val extract_match_cases :
  int ->
  Outcometree.out_type ->
  (Env.t * 'a) option -> (int * Parsetree.pattern list) list
  (** Builds cases a list from the mais type of match expression's type *)
