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

(* $Id: annotlexer.mll,v 1.1.2.2 2008/09/09 14:20:46 filliatr Exp $ *)

{
  open Lexing
  open Annotparser
 
  exception Lexing_error of string
      
  type item_kind_t = Is_type | Is_ident | Is_call 
  
  let item_kd = ref Is_type 

  let set_item kd = item_kd := kd

  let mk_lower_ident s = 
    match ( s , !item_kd ) with
      | ("def"          , Is_ident) -> DEF  
      | ("internal_ref" , Is_ident) -> INT_REF
      | ("external_ref" , Is_ident) -> EXT_REF
      | ("tail"         , Is_call)  -> TAIL
      | ("inline"       , Is_call)  -> INLINE
      | ("stack"        , Is_call)  -> STACK
      | (_              , Is_ident) -> LIDENT s  
      | (_              , Is_call)  -> LIDENT s
      | (_              , Is_type)  -> LIDENT s
      
}


let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let space = [' ' '\t']
let guill = '"'


let file = guill ([^ '"']#['\n']#['(']#[')']#space)* guill 
  
let int = ['0'-'9']+
let typ = ([^ '"']#['\n']#['(']#[')']#space)*

let t= ([^'(' ')' '\t' '\n' ' '])+

let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
 
rule nexttoken = parse
  | "#"  { SHARP }
  | "["  { LBRACKET }
  | "]"  { RBRACKET }
  | "[>" { LBRACKETGREATER }
  | "[<" { LBRACKETLESS }
  | "|"  { BAR }
  | "of" { OF }
  | ";"  { SEMI }
  | ".." { DOTDOT }      
  | "`"  { BACKQUOTE }
  | ">"  { GREATER }
  | '\'' { QUOTE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "--" { END }
  | '.' { DOT }      
  | '*'  { STAR }
  | "->" { ARROW } 
  | '\n' { nexttoken lexbuf } 
  | ':' { COLON }
  | "<"  { LESS }
  | ">"  { GREATER }
  | "&"  { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "||" { BARBAR }
  | ":=" { COLONEQUAL }
  | "+"  { PLUS }
  | "="  { EQUAL }
  | "-"  { MINUS }
  | "-." { MINUSDOT }
  | ","  { COMMA }

  | "as" { AS }



  | "_"  { UNDERSCORE }
  | '?' { QUESTION }
  | "?" lowercase identchar * ':'
      { 
	let s = Lexing.lexeme lexbuf in
	OPTLABEL (String.sub s 1 (String.length s - 2)) 
      }

  | lowercase identchar *
      { mk_lower_ident (Lexing.lexeme lexbuf) }

  | uppercase identchar *
      { UIDENT(Lexing.lexeme lexbuf) } 
  
  | int as s 
      {
	let n = int_of_string s 
	in INT n 
      }
    
  
  | '"'
      { 
	  let buf = Buffer.create 512 in 
	  string buf lexbuf;
	  FILE (Buffer.contents buf) 
      }

  | space+ { nexttoken lexbuf }
 

  | "\nident"  { set_item Is_ident ; IDENT }

  | "\ncall"   { set_item Is_call  ; CALL }

  | "\ntype"   { set_item Is_type  ; TYPE }
 
  | "!" symbolchar *
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['~' '?'] symbolchar +
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['=' '<' '>' '|' '&' '$'] symbolchar *
            { INFIXOP0(Lexing.lexeme lexbuf) }
  | ['@' '^'] symbolchar *
            { INFIXOP1(Lexing.lexeme lexbuf) }
  | ['+' '-'] symbolchar *
            { INFIXOP2(Lexing.lexeme lexbuf) }
  | "**" symbolchar *
            { INFIXOP4(Lexing.lexeme lexbuf) }
  | ['*' '/' '%'] symbolchar *
            { INFIXOP3(Lexing.lexeme lexbuf) }

(* END NEW *)

  
  | eof  { EOF }
  | _ { raise (Lexing_error (lexeme lexbuf))} 


and string buf = parse
  | '"' { () }
  | '\\' 'n' 
      {  string buf lexbuf }
  | '\\' '\\' 
      {  string buf lexbuf }
  | '\\' '"' 
      {  string buf lexbuf }
  | [^ '\\' '"' '\n']+ 
      { Buffer.add_string buf (lexeme lexbuf);
	string buf lexbuf 
      }
  | '\\' 
      { raise (Lexing_error "illegal escape character") }
  | '\n' | eof
      { raise (Lexing_error "unterminated string") }
  | _ 
      { raise (Lexing_error ("illegal character: " ^ lexeme lexbuf)) }

