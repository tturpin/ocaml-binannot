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

val get_c_num : Location.t -> int * int
val module_name : string list -> string
val concat_string_list : string list -> string
val add_qualif : Longident.t -> Longident.t -> Longident.t
val max_patterns_depth : Parsetree.pattern list -> int
val pattern_depth : Parsetree.pattern -> int
val lid_head : Longident.t -> string

(** Return the flat path to the module containing the given type. For
    example,  type_path <A.B.t> = ["A" ; "B"]. *)
val type_path : Outcometree.out_type -> string list

val lid_to_str : Longident.t -> string
val add_qualif_oid :
  Outcometree.out_ident -> Outcometree.out_ident -> Outcometree.out_ident
val flatten : Outcometree.out_ident -> string list
val qualif_from_oid :
  Outcometree.out_ident -> Parsetree.pattern_desc -> Parsetree.pattern_desc

val is_list : Longident.t -> bool

module Lpp :
sig
  val print_pattern : Format.formatter -> Parsetree.pattern_desc -> unit
end

val debug : ('a, out_channel, unit) format -> 'a
val debugln : ('a, out_channel, unit) format -> 'a
