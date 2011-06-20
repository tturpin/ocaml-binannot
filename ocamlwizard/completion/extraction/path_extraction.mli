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

(** This module extracts the propositions from the cmi files for 
    path and record completion *)


val mk_modules : Types.signature -> Interface.module_info list
(** Extracts and makes a list of modules which can complete the module 
    path *)  
  
val mk_values : Types.signature -> Interface.syntax_env ->
  Env.t * Types.type_expr -> string list -> string ->
  Interface.value_info list
(** Extracts and makes a list of modules which can complete 
    the value path *)  
    
val mk_records :
  Interface.completion_infos ->
  Interface.syntax_env ->
  Interface.path_completion ->
  Env.t * Types.type_expr -> Interface.record_kind -> 
  Interface.record_info list
  (** Extracts and makes a list of the record fields *)  
  
