(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Id *)

(* Module [Token]: lexers for Camlp4 grammars *)

(* This module defines the Camlp4 lexer type to be used in extensible
   grammars (see module [Grammar]). It also provides some useful functions
   to create lexers (this module should be renamed [Glexer] one day). *)

(*** Token type *)

type t = string * string;;
type pattern = string * string;;
     (* Token and token patterns. Token are build by the lexer. Token
        patterns come from the EXTEND statement.
-       The first string is the constructor name (must start with
        an uppercase character). When it is empty, the second string
        is supposed to be a keyword.
-       The second string is the constructor parameter. Empty if it
        has no parameter.
-       The way tokens pattern are interpreted to parse tokens is
        done by the lexer, function [tparse] below. *)

exception Error of string;;
     (* An lexing error exception to be used by lexers. *)

(*** Lexer type *)

type location = int * int;;
type location_function = int -> location;;
   (* The type for a function associating a number of a token in a stream
      (starting from 0) to its source location. *)
type lexer_func = char Stream.t -> t Stream.t * location_function;;
   (* The type for a lexer function. The character stream is the input
      stream to be lexed. The result is a pair of a token stream and
      a location function for this tokens stream. *)

type lexer =
  { func : lexer_func;
    using : pattern -> unit;
    removing : pattern -> unit;
    tparse : pattern -> (t Stream.t -> string) option;
    text : pattern -> string }
;;
    (* The type for a lexer used by Camlp4 grammars.
-      The field [func] is the main lexer function. See [lexer_func]
       type above. This function may be created from a [char stream parser]
       or for an [ocamllex] function using the functions below.
-      The field [using] is a function telling the lexer that the grammar
       uses this token (pattern). The lexer can check that its constructor
       is correct, and interpret some kind of tokens as keywords (to record
       them in its tables). Called by [EXTEND] statements.
-      The field [removing] is a function telling the lexer that the grammar
       does not uses the given token (pattern) any more. If the lexer has a
       notion of "keywords", it can release it from its tables. Called by
       [DELETE_RULE] statements.
-      The field [tparse] is a function returning either [Some f], f being
       the parsing function associated with the pattern, or [None] if it
       is a standard parsing of the pattern.
-      The field [text] returns the name of some token pattern,
       used in error messages. *)

val lexer_text : pattern -> string;;
    (* A simple [text] function for lexers *)

(*** Lexer from char stream parsers or ocamllex function *)

(* The functions below create lexer functions either from a [char stream]
   parser or for an [ocamllex] function. With the returned function [f],
   the simplest [Token.lexer] can be written:
-  [       { Token.func = f;                    ]
-  [         Token.using = (fun _ -> ());       ]
-  [         Token.removing = (fun _ -> ());    ]
-  [         Token.tparse = (fun _ -> None);    ]
-  [         Token.text = Token.lexer_text }    ]
   Note that a better [using] function should check the used tokens
   and raise [Token.Error] for incorrect ones. The other functions
   [removing], [tparse] and [text] may have other implementations
   as well. *)

val lexer_func_of_parser : (char Stream.t -> t * location) -> lexer_func;;
    (* A lexer function from a lexer written as a char stream parser
       returning the next token and its location. *)
val lexer_func_of_ocamllex : (Lexing.lexbuf -> t) -> lexer_func;;
    (* A lexer function from a lexer created by [ocamllex] *)

val make_stream_and_location :
  (unit -> t * location) -> t Stream.t * location_function;;

(*** Useful functions *)

val eval_char : string -> char;;
val eval_string : string -> string;;
    (* Convert a char or a string token, where the backslashes have not
       been interpreted into a real char or string; raise [Failure] if
       bad backslash sequence found; [Token.eval_char (Char.escaped c)]
       returns [c] and [Token.eval_string (String.escaped s)] returns [s] *)
