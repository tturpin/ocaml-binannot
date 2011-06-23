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

(** The main module for completion : It coordinates the differentes 
    steps : 
    - syntactic completion 
    - expression typing 
    - proposal extraction
    - proposal filering
    - proposal printing*)

val main : Interface.completion_infos -> unit
  (** The main entry of the completion command *)

val compile_file : Parsetree.structure ->
  Interface.completion_infos ->
  Typedtree.structure * Types.signature * Interface.completion_infos

val initial_env : unit -> Env.t
