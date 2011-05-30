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

open Common_config
open Location
open Lexing

let reloc loc =
  let add pos = 
    let file = pos.pos_fname in
    if file = !fic_source || Filename.dirname file = "standard_library" then 
      pos 
    else 
      { pos with pos_fname = Filename.concat !root_dir file } 
  in
  { loc with 
    loc_start = add loc.loc_start; 
    loc_end   = add loc.loc_end }

let print_location fmt loc = 
  let loc = if !Common_config.root_dir = "" then loc else reloc loc in
  Location.print fmt loc

let print_loc_opt f = function
    Some x -> f x | None ->   () 
	  
let print_decl d = 
  Format.fprintf Format.std_formatter "%adeclaration@." print_location d
 
let print_defn d = 
  Format.fprintf  Format.std_formatter "%adefinition@." print_location d
  
let print_use d = 
  Format.fprintf  Format.std_formatter "%autilization @." print_location d
 
