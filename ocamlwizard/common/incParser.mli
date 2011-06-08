(** Parsing with backtracking. *)

(** Parse a modified file with backtracking. The table, start and
    lexfun arguments are those usually provided to
    Parsing.yyparse. The typing is obviously unsafe: the parameters
    should match the token and parsetree types. *)
val backtracking_parser :
  Parsing.parse_tables -> int -> (Lexing.lexbuf -> 'token) ->
  Diff.chunk list -> 'ast

val implementation : Diff.chunk list -> Parsetree.structure
