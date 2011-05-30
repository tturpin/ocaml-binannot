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

(** This module implements functions which are used 
    to filter path and record completion *)

val filter_modules :
  string -> Interface.module_info list -> Interface.module_info list
  (** It filters the modules according to the given pattern *)

val filter_values :
  Outcometree.out_type ->
  string -> Interface.value_info list -> 
  Interface.value_kind -> Interface.value_info list
  (** It filters the values according to the given pattern. It also
      filters them accoring to the given type deduced from the context. *)
  
val filter_records :
  Outcometree.out_type list ->
  string ->
  Interface.record_info list ->
  Interface.record_kind -> Interface.record_info list
  
(** It filters the labels according to the given pattern. It also
    filters them accoring to the given type deduced from the context and 
    the given labels already typed by the user. *)
  
