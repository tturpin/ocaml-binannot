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

(**This module is used to type-check when the user request for
   a pattern matching completion with type-checking *)

exception Partial_env of Env.t * Types.type_expr
  (** When the type-checker reaches to location where the user have
      requested for a completion, this exception is raised with 
      the built environment up to this location and the 
      expression's type *)
  
val check_for_pat_var : (Typedtree.pattern * 'a) list -> unit
  (** Looks for the location where the user have requested for 
      a match completion and raises Partial_env when this location 
      is found *)

