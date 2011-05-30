/**************************************************************************/
/*                                                                        */
/*  Ocamlwizard                                                           */
/*  David Baudet and Mohamed Iguernelala                                  */
/*  Copyright 2008 INRIA Saclay - Ile-de-France                           */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2.1, with the special exception on linking            */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

/* $Id: annotparser.mly,v 1.1.2.2 2008/09/09 14:20:46 filliatr Exp $ */
/* The parser definition */

%{ 
 open Annotast
 open Outcometree
 open Lexing

%}

/* Tokens */



/****************/
%token AS
%token <string> OPTLABEL
%token UNDERSCORE
%token SHARP
%token LBRACKET
%token LBRACKETGREATER
%token LBRACKETLESS
%token RBRACKET
%token BAR
%token OF
%token SEMI
%token DOTDOT
%token BACKQUOTE

/****************/

%token QUOTE
%token QUESTION
%token COLON
%token <int>INT
%token EOF
%token LPAREN RPAREN
%token <string>FILE
%token TYPE
%token IDENT
%token CALL
%token STACK TAIL INLINE
%token EXT_REF INT_REF DEF
%token END
%token ARROW STAR DOT
%token <string>LIDENT
%token <string>UIDENT
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token <string> PREFIXOP
%token COMMA
%token PLUS
%token MINUS
%token MINUSDOT
%token EQUAL
%token LESS
%token GREATER
%token OR
%token BARBAR
%token AMPERSAND
%token AMPERAMPER
%token COLONEQUAL


%right    ARROW    
%left     STAR  




%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS MINUS MINUSDOT  /* expr (e OP e OP e) */
%left     INFIXOP3 
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus              /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT





























%start annot_parse                   /* for the #use directive */
%type <Annotast.annot_file> annot_parse


%%

/* Entry points */



annot_parse : 
    | annots { $1 }
;

annots:
  | EOF          { [] }
  | annot annots { $1::$2}
;



annot: 
  | ent annot_content    { ($1,$2)  }
;

annot_content :
  | annot_item                { [$1]  }
  | annot_item annot_content  { $1::$2 }
;

annot_item:
  | typ                       { A_type  $1 }
  | ident_kw                  { A_ident $1 }
  | call                      { A_call  $1 }
;


call:
  | CALL LPAREN call_content RPAREN  { $3 }

;
call_content:
  | STACK                            { Stack }
  | TAIL                             { Tail }
  | INLINE                           { Inline }
ent:
  | lex_pos lex_pos                  { $1,$2 }
;
lex_pos:
  | FILE INT INT INT {
	{ 
	  pos_fname = $1;
	  pos_lnum  = $2; 
	  pos_bol   = $3; 
	  pos_cnum  = $4;
	}
      }

;

ident_kw:
  | IDENT LPAREN id_content RPAREN  { $3 }
;
id_content:
  | DEF ident def_pos                     { Def     $3 }
  | INT_REF ent                     { Int_ref $2 }
  | EXT_REF val_longident           { Ext_ref $2 }
;
def_pos:
  | lex_pos lex_pos                 { $1, $2 }
  | lex_pos END                     { $1, Lexing.dummy_pos }
  | END END                         { Lexing.dummy_pos, Lexing.dummy_pos }
;

  
typ:
  TYPE LPAREN core_type RPAREN      { $3 }
;

typevar_list:
  QUOTE ident                       { [$2] }
  | typevar_list QUOTE ident        { $3 :: $1 }
;
poly_type:
  core_type                         { Otyp_poly([], $1) }
  | typevar_list DOT core_type      { Otyp_poly(List.rev $1, $3) }
;

core_type:
  | core_type2                      { $1 }
  | core_type2 AS QUOTE ident       { Otyp_alias($1, $4) }
;

core_type2:
  | simple_core_type_or_tuple        
      { $1 }
      
  | QUESTION LIDENT COLON core_type2 ARROW core_type2
      { Otyp_arrow("?" ^ $2 ,Otyp_constr(Oide_ident "option", [$4]), $6) }
      
  | OPTLABEL core_type2 ARROW core_type2
      { Otyp_arrow("?" ^ $1 ,Otyp_constr(Oide_ident "option", [$2]), $4) }
      
  | LIDENT COLON core_type2 ARROW core_type2 
      { Otyp_arrow($1, $3, $5) }
  
  | core_type2 ARROW core_type2 
      { Otyp_arrow("", $1, $3) }
;

simple_core_type:
  | simple_core_type2 %prec below_SHARP
      { $1 }
  | LPAREN core_type_comma_list RPAREN %prec below_SHARP
      { match $2 with [sty] -> sty | _ -> raise Parse_error }
;
simple_core_type2:
  | QUOTE ident
      /* The boolean is not used */
      { Otyp_var (false,$2) }
  /*
  | UNDERSCORE
      { Otyp_any }
    */  
  | type_longident
      { Otyp_constr($1, []) }

  | simple_core_type2 type_longident
      { Otyp_constr($2, [$1]) }
  | LPAREN core_type_comma_list RPAREN type_longident
      { Otyp_constr($4, List.rev $2) }
  
    /* None is not used */
  | LESS meth_list GREATER
      { Otyp_object ($2,None) }
  | LESS GREATER
      { Otyp_object ([],None) }
    
   
    /* TO CHECK */
  | SHARP class_longident opt_present
      { Otyp_class(false,$2, []) }
      
  | simple_core_type2 SHARP class_longident opt_present
      { Otyp_class(false,$3, [$1]) }
  | LPAREN core_type_comma_list RPAREN SHARP class_longident opt_present
      { Otyp_class(false,$5, List.rev $2)}


  /* The first boolean is not used */

  | LBRACKET tag_field RBRACKET
      {Otyp_variant(false,Ovar_fields[$2], true, None)}
    
  | LBRACKET BAR row_field_list RBRACKET
      { Otyp_variant(false,Ovar_fields(List.rev $3), true, None) }
  | LBRACKET row_field BAR row_field_list RBRACKET
      { Otyp_variant(false,Ovar_fields($2 :: List.rev $4), true, None)}
  | LBRACKETGREATER opt_bar row_field_list RBRACKET
      { Otyp_variant(false,Ovar_fields(List.rev $3), false, None) }

  | LBRACKETGREATER RBRACKET
      { Otyp_variant(false,Ovar_fields [], false, None)}
  
  | UNDERSCORE LBRACKETGREATER opt_bar row_field_list RBRACKET
      { Otyp_variant(false,Ovar_fields(List.rev $4), false, None) }

    /* TO CHECK
  | UNDERSCORE LBRACKETLESS opt_bar row_field_list RBRACKET
      { Otyp_variant(false,Ovar_fields (List.rev $4), true, Some []) }
  */
  | LBRACKETLESS opt_bar row_field_list RBRACKET
      { Otyp_variant(false,Ovar_fields (List.rev $3), true, Some []) }

  | LBRACKETLESS opt_bar row_field_list GREATER name_tag_list RBRACKET
      { Otyp_variant(false , Ovar_fields (List.rev $3), true, Some (List.rev $5)) }


  | UNDERSCORE LBRACKETLESS opt_bar row_field_list GREATER name_tag_list RBRACKET
      { Otyp_variant(false , Ovar_fields (List.rev $4), true, Some (List.rev $6)) }
    
    /* PR#3835: this is not LR(1), would need lookahead=2
  | LBRACKET simple_core_type2 RBRACKET
      { Otyp_variant(false,Ovar_fields[$2], true, None) }
*/

;
row_field_list:
    row_field                                   {  [$1] }
  | row_field_list BAR row_field                {  $3 :: $1 }
;
row_field:
    tag_field                                   {  $1 }
  | simple_core_type2                           {  ("", true, [$1])(*Rinherit $1*) }
;

 /**
 | Ovar_fields of (string * bool * out_type list) list
  | Ovar_name of out_ident * out_type list
**/

tag_field:
    name_tag OF opt_ampersand amper_type_list
      {  ($1, $3, List.rev $4) (*Rtag ($1, $3, List.rev $4)*) }
  | name_tag
      {  ($1, true, [])(*Rtag ($1, true, []) *)}
;
opt_ampersand:
    AMPERSAND                                   {  true }
  | /* empty */                                 {  false }
;
amper_type_list:
    core_type                                   {  [$1] }
  | amper_type_list AMPERSAND core_type         {  $3 :: $1 }
;
opt_present:
    LBRACKETGREATER name_tag_list RBRACKET      {  List.rev $2 }
  | /* empty */                                 {  [] }
;
name_tag_list:
    name_tag                                    {  [$1] }
  | name_tag_list name_tag                      {  $2 :: $1 }
;
simple_core_type_or_tuple:
    simple_core_type                            {  $1 }
  | simple_core_type STAR core_type_list
      { Otyp_tuple($1 :: List.rev $3) }
;
core_type_comma_list:
    core_type                                   {  [$1] }
  | core_type_comma_list COMMA core_type        {  $3 :: $1 }
;
core_type_list:
    simple_core_type                            {  [$1] }
  | core_type_list STAR simple_core_type        {  $3 :: $1 }
;
meth_list:
  | field SEMI meth_list                        {  $1 :: $3 }
  | field opt_semi                              {  [$1] }
  | DOTDOT                                      {  [] }
  | UNDERSCORE DOTDOT                           {  [] }

;
field:
  | label COLON poly_type                       {  ($1, $3) }
 
;
label:
    LIDENT                                      {  $1 }
;


/* Identifiers and long identifiers */

ident:
    UIDENT                                      {  $1 }
  | LIDENT                                      {  $1 }
;
val_ident:
    LIDENT                                      {  $1 }
  | LPAREN operator RPAREN                      {  $2 }
;


val_longident:
  | val_ident                                   {  Oide_ident $1 }
  | mod_longident DOT val_ident                 {  Oide_dot($1, $3) }
  | mod_longident DOT operator                  {  Oide_dot($1, $3) }

;
type_longident:
    LIDENT                                      {  Oide_ident $1 }
  | mod_ext_longident DOT LIDENT                {  Oide_dot($1, $3) }
;
mod_longident:
    UIDENT                                      {  Oide_ident $1 }
  | mod_longident DOT UIDENT                    {  Oide_dot($1, $3) }
;
mod_ext_longident:
    UIDENT                                      {  Oide_ident $1 }
  | mod_ext_longident DOT UIDENT                {  Oide_dot($1, $3) }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN { Oide_apply($1, $3) }
;
class_longident:
    LIDENT                                      { Oide_ident $1 }
  | mod_longident DOT LIDENT                    { Oide_dot($1, $3) }
;


/* Miscellaneous */

name_tag:
    BACKQUOTE ident                             { $2 }
;
opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;



operator:
  | PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | PLUS                                        { "+" }
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }



