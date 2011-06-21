(**************************************************************************)
(*                                                                        *)
(*  Ocamlwizard-Binannot                                                  *)
(*  Tiphaine Turpin                                                       *)
(*  Copyright 2011 INRIA Saclay - Ile-de-France                           *)
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

(** Parsing with backtracking. *)

(** Parse a modified file with backtracking. The table, start and
    lexfun arguments are those usually provided to
    Parsing.yyparse. The typing is obviously unsafe: the parameters
    should match the token and parsetree types. *)
val backtracking_parser :
  Parsing.parse_tables -> int -> (Lexing.lexbuf -> 'token) ->
  Diff.chunk list -> 'ast

val implementation : Diff.chunk list -> Parsetree.structure
