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
open Parsetree
let basic = ["int" ; "bool" ; "float" ; "string"; "list"; "char" ; "unit"; "ref" ;
	     "exn"]
let basic_assoc = 
  ["int","n" ; "bool","b" ; "float","f" ; "string","s"; "list","lis"; 
   "char","c" ; "unit","u"; "ref","rf"; "exn","exc"]

let is_basic t = List.mem t basic

let find_basic t =  List.assoc t basic_assoc

let mk_option_sum lis = 
  let none = ("None", []) in
  let some = ("Some", lis) in
  Outcometree.Otyp_sum [none; some]

let mk_option_list lis =
  let empty = ("[]", []) in
  let other = ("(hd :: tl)", []) in
  Outcometree.Otyp_sum [empty;other]  
  
    
let mk_bool_pats () = 
  let loc = Location.none in
  let yea = Longident.lident Location.none "true" in
  let yea = {ppat_desc = Ppat_construct(yea, None, false); ppat_loc = loc} in
  let no = Longident.lident Location.none "false" in
  let no = {ppat_desc = Ppat_construct(no, None, false); ppat_loc = loc} in 
  [yea ; no]
