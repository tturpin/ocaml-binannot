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

type tprinter =
  | Default_printer
  | Ocaml_printer
type tparser =
  | Default_parser

(** *)
type range = {
    mutable b : int ; (*from_char*)
    mutable e : int ; (*to_char*)
  }

(** The acceptable kimd of types for value completion *)    
type value_kind = 
    | V_all (* accept any type *)
    | V_record (* only accept record types ( { v } ) *)

(** *)    
type record_kind = 
    | Fdummy
    | Faccess of Parsetree.expression
    | Fdef of Longident.t list
    | Fpat of Longident.t list
	
(** Type of path completion to perform *)
type path_kind =
    | Module (* Complete a module name *)
    | Record of record_kind (* Complete a record field name *)
    | Value of  value_kind (* Complete a value name *)
	
	
(** *)
type path_completion = {
    p_kd         : path_kind;
    p_md         : string list; (* The qualified part (module path) *)
    p_id         : string; (* The short ident to complete *)
  }
    
  (** *)
type pm_completion =
    | AllCs
    (* match e with . EOF *)
    | MissCs   of Parsetree.pattern list
    (* match e with p -> e' . EOF *)
    | BranchCs of
	Parsetree.pattern (* The whole current (and last) case, until -> *)
      * Parsetree.pattern list (* The other (previous) cases *)
    (* match e with p -> e' | (.. x. ..) -> EOF *)
	
(** Sort of approptiate completion, determined by parsing *)
type completion_sort = 
    | Match of pm_completion
    | Try   of pm_completion
    | Path  of path_completion
    | Other 
    | Error of exn
	
(* Parser state with respect to completion. *)
type parser_env = {
  mutable c_sort       : completion_sort ; (* The kind of completion
					      to perform *)
  mutable match_exp : Parsetree.expression option; (* The matched
						      expression *)
  mutable miss : string; (* possible missing "with" *)
  mutable c_cut_pos    : int; (* *)
  mutable rec_inited   : bool;
  mutable closing      : string list;
  mutable prog      : string;
  mutable eof_pos   : int;
  mutable ends_with_space  : bool;
  mutable mods_path : string list;
}
    
(** *)
type completion_infos = {
  fb_name     : string; (* temporary file with the contents of the
			   buffer, up to the current point *)
  f_path      : string;
  includ      : string list;
  c_rg        : range;
  c_kind      : string;
  c_parser    : tparser;
  c_printer   : tprinter;
}
    
type syntax_env = {
  ast         : Parsetree.structure;
  cprog       : string;
  mpath       : string list;
  comp        : completion_sort;
  closures    : string list;
  exp_rg      : range;
}

(*--------------------------------------------------------------------------*)

(** *)
type taffect = Tnone | Tvoid | Tmutable | Tref

(** *)
type module_info = {
    m_name    : string;
    m_miss    : string;
    m_level    : int;
}

(** *)
type value_info = {
    vl_name     : string;
    vl_miss     : string;
    vl_level    : int;
    vl_type     : Outcometree.out_type;
    vl_affect   : taffect;
    vl_fpat     : bool;
    vl_ftype    : bool;
    vl_kind     : value_kind;
}

(** *)
type label_info = {
    l_name    : string;
    l_type    : Outcometree.out_type;
    l_miss    : string;
    l_mut     : bool;

    l_affect   : taffect;
    l_fpat     : bool;
    l_ftype    : bool;
    l_kind     : value_kind;
}

(** *)
type record_info = {
    r_name    : string list;
    r_labels  : label_info list;
    r_level   : int;
}

(** *)
type var_info = {
  v_name :string;
  v_type :string;
}

(** *)
type match_info = {
    ma_pattern : Parsetree.pattern;
    ma_level   : int;
    ma_selected: bool;
    ma_vars    : var_info list;
  }



type match_depths = int * Parsetree.pattern list

type match_step = ME of (match_depths list) | MF of (match_info list)


(** *)
type completion_result = 
    | C_module of module_info list * string
    | C_value  of value_info list  * value_kind * string
    | C_record of record_info list * record_kind * string
    | C_match  of match_step * pm_completion
    | C_error  of exn
    | C_other

