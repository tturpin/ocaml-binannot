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

val dummy_range : Interface.range
val parser_state : Interface.parser_env
val no_space : unit -> bool
val locate_completion : string -> unit
val end_of_file : int -> bool
val set_rec_inited : unit -> unit
val rec_is_not_inited : unit -> bool
val init_completion_env : string -> unit
val print : string -> unit
val update_cut_pos : int -> unit
val get_expr : int -> int -> string
val add_closing : string -> string -> int -> int -> unit
val patterns : ('a * 'b) list -> 'a list
val mod_and_ident : unit -> string list * string
val update_match : Parsetree.expression -> Interface.pm_completion -> int -> string -> bool
val update_try : 'a -> Interface.pm_completion -> int -> string -> unit
val update_module : string list -> string -> int -> int -> unit
val update_value :
  Parsetree.expression -> string list -> string -> int -> int -> Interface.value_kind -> unit
val update_lbl_longid : string list -> string -> int -> int -> unit
val update_expr_longid : Parsetree.expression -> string list -> string -> int -> int -> int -> unit
val update_not_inited_redef :
  string list -> string -> Longident.t list -> int -> int -> int -> unit
val update_value_kind : Interface.value_kind -> unit
val update_value_in_redef : int -> int -> 'a -> unit
val update_not_inited_def :
  string list -> string -> Longident.t list -> int -> int -> int -> unit
val update_value_in_def : int -> int -> 'a -> unit
val update_left_imbr : unit -> unit
val update_pattern : string list -> string -> Longident.t list -> int -> unit
val untagg_in : unit -> unit
val rewrite_function : int -> unit
