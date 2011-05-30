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

(** This module provides three kinds of expressions/patterns :
    - a variable "ocamlwizard_ghost_variable"
    - an expression "assert false"
    - a variable "ocamlwizard_ghost_variable_any" which simulate "_" (any).
    
    These variables are added when ocamlwizard tries to complete the
    incompleted file. Before that, they are tagged. The tags are used for
    localization using regular expessions. *)

type tag = T_ogv | T_asf | T_any
    (** the three kinds of tagged expressions/ patterns*)
    
val ogv : string
  (** ocamlwizard_ghost_variable *)

val tagged_ogv : string
  (** Tagged ocamlwizard_ghost_variable *)

val asf : string
  (** assert false *)

val tagged_asf : string
  (** Tagged assert false *)

val any : string
  (** ocamlwizard_ghost_variable_any simulate the pattern _ (any)*)

val tagged_any : string
  (** Tagged any *)

val locate_tag : string -> tag -> Interface.range
  (** locate [prog] [tg]
      Given a program [prog] (string) and a tag [tg], 
      this function returns its position in the program.
      The program is supposed to contain only one tag of type [tg]*)
  
val mk_ghost_module : string list -> string
(** *)

val tagged_in : string
  (** When, we have a let-in construction, we don't know if we are in
      top level or not, so we write let-(*in*) : a "let" without "in"*)
  
val untag_letin : string -> string
  (** This function look for a tagged let-in and untaggs it *)
