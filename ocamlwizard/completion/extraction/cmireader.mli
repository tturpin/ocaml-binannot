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

(** This module extracts information about types from .cmi files *)


val get_signature : string -> Outcometree.out_sig_item list
  (** reads a .cmi file and builds a signature = item list *)
  
  
val get_module :
  string -> Outcometree.out_sig_item list -> Outcometree.out_module_type
  (** Looks for the given module name in the signature (item list) 
      and returns the corresponding module type *)

val values_of_type :
  string list ->
  Interface.completion_infos ->
  Interface.syntax_env -> bool -> string list * Outcometree.out_type
  (** Looks for the values of the given type *)
  
val get_main_type :
  Interface.completion_infos ->
  Interface.syntax_env ->
  string list * Outcometree.out_type ->
  bool -> string list * Outcometree.out_type
  (** Looks for the main name of the given type*)
