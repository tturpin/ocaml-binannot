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

(** This module prints the completion result on the std_output*)

val main :
  miss:string ->
  Format.formatter ->
  Outcometree.out_type list ->
  Interface.completion_result -> Interface.tprinter -> unit
  (** Prints the completion result on the std_output :
      For now, the result can be printed in xml-format or in
      ocaml-format according to the value of the flag "-printer".
      - The user can easily define a new printer and integrate it.
      To do this He/she have to : 
      
      1 - Define the new printer in a file "my_printer.ml" : the main entry
      of this file must takes a completion_result parameter
      (this type is defined in interface.mli)
      
      2 - Add a value "My_printer" to the type tprinter defined in 
      interface.mli
      
      3 - Add a value to the printer option ("my-printer" for example) in
      Common_config.set_printer and bind it with My_printer.

      4 - Add a case in the main entry of proposal_printing :
      (| My_printer -> My_printer.print <parameters>)
  *)
