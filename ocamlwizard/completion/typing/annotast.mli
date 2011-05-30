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

(** 
    This interface provides an "abstract syntax tree" 
    to annot_files parser.
*)


(** An annot file is a list of headers within the content associated 
    with each header.*)

type annot_file =  ( header * annot_content ) list

(** An annot content (which is assotiated with a header) is a list 
    of annot items *)

and annot_content = annot_item list

(** An annot item (which is contained in an annot content) can be an :
    - A_type  : Which contains information about a type
    (the type is embedded in an Outcometree.out_type).
    - A_call  : It can be set to Stack, Tail or Inline?
    - A_ident : It contains information about the definition and
    the use of identifiers *)
 
and annot_item = 
    | A_type of  Outcometree.out_type
    | A_ident of ident_kw
    | A_call of call_kw
	

(** *)

and call_kw = Stack | Tail | Inline

(** *)

and ident_kw = 
  | Def of header
  | Int_ref of header
  | Ext_ref of Outcometree.out_ident

(** *)

and header =  Lexing.position * Lexing.position
