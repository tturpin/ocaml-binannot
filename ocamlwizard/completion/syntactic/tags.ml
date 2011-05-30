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

open Format
open Interface
open Util
(** *)
type tag =  T_ogv | T_asf | T_any
    
(** *)
let ogv_tag           = "(*[/TAG_OGV]*)"
let ogv_tag_len       = String.length ogv_tag
  
let ogv               = "ocamlwizard_ghost_variable"
let ogv_len           = String.length ogv
  
let tagged_ogv        = sprintf "%s%s%s" ogv_tag ogv ogv_tag
let tagged_ogv_regexp = Str.regexp_string tagged_ogv
  
(** *)
let asf_tag           = "(*[/TAG_ASF]*)"
let asf_tag_len       = String.length asf_tag
  
let asf               = "assert false"
let asf_len           = String.length asf
  
let tagged_asf        = sprintf "%s%s%s" asf_tag asf asf_tag
let tagged_asf_regexp = Str.regexp_string tagged_asf
  

(** *)
let any_tag           = "(*[/TAG_ANY]*)"
let any_tag_len       = String.length any_tag
  
let any               = "ocamlwizard_ghost_variable_any"
let any_len           = String.length any
  
let tagged_any        = sprintf "%s%s%s" any_tag any any_tag
let tagged_any_regexp = Str.regexp_string tagged_any

  
(** *)
let locate_tag_aux str tag_regexp tag_len exp_len = 
  let pos = Str.search_forward tag_regexp str 0     in
  let exp_beg = pos + tag_len                       in
  { b = exp_beg ; e = exp_beg + exp_len }
    
(** *)
let locate_tag str = function
  | T_ogv -> locate_tag_aux str tagged_ogv_regexp ogv_tag_len ogv_len
  | T_asf -> locate_tag_aux str tagged_asf_regexp asf_tag_len asf_len
  | T_any -> locate_tag_aux str tagged_any_regexp any_tag_len any_len
      

let mk_ghost_module md = 
  sprintf "\nmodule Ocamlwizard_Ghost_Module = %s " (module_name md)


(** *)
let tagged_in = "(*[/TAG_IN] in assert false [/TAG_IN]*)"
  
(** *)
let tagged_in_regexp = Str.regexp_string tagged_in
  
(** *)
let untagged_in = "(*[/UNTAG_IN]*) in assert false (*[/UNTAG_IN]*)"
  
let untag_letin code = 
  Str.replace_first tagged_in_regexp untagged_in code
      
