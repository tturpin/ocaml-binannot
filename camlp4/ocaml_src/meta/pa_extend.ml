(* camlp4r pa_extend.cmo q_MLast.cmo *)
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

open Stdpp;;

let split_ext = ref false;;

Pcaml.add_option "-split_ext" (Arg.Set split_ext)
  "   Split EXTEND by functions to turn around a PowerPC problem.";;

Pcaml.add_option "-split_gext" (Arg.Set split_ext)
  "  Old name for the option -split_ext.";;

type name = { expr : MLast.expr; tvar : string; loc : int * int };;

type ('e, 'p, 't) entry =
  { name : name; pos : 'e option; levels : ('e, 'p, 't) level list }
and ('e, 'p, 't) level =
  { label : string option; assoc : 'e option; rules : ('e, 'p, 't) rule list }
and ('e, 'p, 't) rule =
  { prod : ('e, 'p, 't) psymbol list; action : 'e option }
and ('e, 'p, 't) psymbol =
  { pattern : 'p option; symbol : ('e, 'p, 't) symbol }
and ('e, 'p, 't) symbol =
  { used : name list; text : string -> string -> 'e; styp : string -> 't }
;;

type used = Unused | UsedScanned | UsedNotScanned;;

let mark_used modif ht n =
  try
    let rll = Hashtbl.find_all ht n.tvar in
    List.iter
      (fun (r, _) ->
         if !r == Unused then begin r := UsedNotScanned; modif := true end)
      rll
  with
    Not_found -> ()
;;

let rec mark_symbol modif ht symb =
  List.iter (fun e -> mark_used modif ht e) symb.used
;;

let check_use nl el =
  let ht = Hashtbl.create 301 in
  let modif = ref false in
  List.iter
    (fun e ->
       let u =
         match e.name.expr with
           MLast.ExLid (_, _) -> Unused
         | _ -> UsedNotScanned
       in
       Hashtbl.add ht e.name.tvar (ref u, e))
    el;
  List.iter
    (fun n ->
       try
         let rll = Hashtbl.find_all ht n.tvar in
         List.iter (fun (r, _) -> r := UsedNotScanned) rll
       with
         _ -> ())
    nl;
  modif := true;
  while !modif do
    modif := false;
    Hashtbl.iter
      (fun s (r, e) ->
         if !r = UsedNotScanned then
           begin
             r := UsedScanned;
             List.iter
               (fun level ->
                  let rules = level.rules in
                  List.iter
                    (fun rule ->
                       List.iter (fun ps -> mark_symbol modif ht ps.symbol)
                         rule.prod)
                    rules)
               e.levels
           end)
      ht
  done;
  Hashtbl.iter
    (fun s (r, e) ->
       if !r = Unused then
         !(Pcaml.warning) e.name.loc ("Unused local entry \"" ^ s ^ "\""))
    ht
;;

let locate n = let loc = n.loc in n.expr;;

let new_type_var =
  let i = ref 0 in fun () -> incr i; "e__" ^ string_of_int !i
;;

let used_of_rule_list rl =
  List.fold_left
    (fun nl r -> List.fold_left (fun nl s -> s.symbol.used @ nl) nl r.prod) []
    rl
;;

let retype_rule_list_without_patterns loc rl =
  try
    List.map
      (function
         {prod = [{pattern = None; symbol = s}]; action = None} ->
           {prod = [{pattern = Some (MLast.PaLid (loc, "x")); symbol = s}];
            action = Some (MLast.ExLid (loc, "x"))}
       | {prod = []; action = Some _} as r -> r
       | _ -> raise Exit)
      rl
  with
    Exit -> rl
;;

let text_of_psymbol_list loc gmod psl tvar =
  List.fold_right
    (fun ps txt ->
       let x = ps.symbol.text gmod tvar in
       MLast.ExApp (loc, MLast.ExApp (loc, MLast.ExUid (loc, "::"), x), txt))
    psl (MLast.ExUid (loc, "[]"))
;;

let text_of_action loc psl rtvar act tvar =
  let locid = MLast.PaLid (loc, !(Stdpp.loc_name)) in
  let act =
    match act with
      Some act -> act
    | None -> MLast.ExUid (loc, "()")
  in
  let e =
    MLast.ExFun
      (loc,
       [MLast.PaTyc
          (loc, locid,
           MLast.TyTup
             (loc, [MLast.TyLid (loc, "int"); MLast.TyLid (loc, "int")])),
        None, MLast.ExTyc (loc, act, MLast.TyQuo (loc, rtvar))])
  in
  let txt =
    List.fold_left
      (fun txt ps ->
         match ps.pattern with
           None -> MLast.ExFun (loc, [MLast.PaAny loc, None, txt])
         | Some p ->
             let t = ps.symbol.styp tvar in
             MLast.ExFun (loc, [MLast.PaTyc (loc, p, t), None, txt]))
      e psl
  in
  MLast.ExApp
    (loc,
     MLast.ExAcc
       (loc, MLast.ExUid (loc, "Gramext"), MLast.ExLid (loc, "action")),
     txt)
;;

let text_of_rule_list loc gmod rtvar rl tvar =
  List.fold_left
    (fun txt r ->
       let sl = text_of_psymbol_list loc gmod r.prod tvar in
       let ac = text_of_action loc r.prod rtvar r.action tvar in
       MLast.ExApp
         (loc,
          MLast.ExApp
            (loc, MLast.ExUid (loc, "::"), MLast.ExTup (loc, [sl; ac])),
          txt))
    (MLast.ExUid (loc, "[]")) rl
;;

let text_of_entry loc gmod e =
  let ent =
    let x = e.name in
    let loc = e.name.loc in
    MLast.ExTyc
      (loc, x.expr,
       MLast.TyApp
         (loc,
          MLast.TyAcc
            (loc,
             MLast.TyAcc
               (loc, MLast.TyUid (loc, gmod), MLast.TyUid (loc, "Entry")),
             MLast.TyLid (loc, "e")),
          MLast.TyQuo (loc, x.tvar)))
  in
  let pos =
    match e.pos with
      Some pos -> MLast.ExApp (loc, MLast.ExUid (loc, "Some"), pos)
    | None -> MLast.ExUid (loc, "None")
  in
  let txt =
    List.fold_right
      (fun level txt ->
         let lab =
           match level.label with
             Some lab ->
               MLast.ExApp
                 (loc, MLast.ExUid (loc, "Some"), MLast.ExStr (loc, lab))
           | None -> MLast.ExUid (loc, "None")
         in
         let ass =
           match level.assoc with
             Some ass -> MLast.ExApp (loc, MLast.ExUid (loc, "Some"), ass)
           | None -> MLast.ExUid (loc, "None")
         in
         let txt =
           let rl =
             text_of_rule_list loc gmod e.name.tvar level.rules e.name.tvar
           in
           MLast.ExApp
             (loc,
              MLast.ExApp
                (loc, MLast.ExUid (loc, "::"),
                 MLast.ExTup (loc, [lab; ass; rl])),
              txt)
         in
         txt)
      e.levels (MLast.ExUid (loc, "[]"))
  in
  ent, pos, txt
;;

let let_in_of_extend loc gmod functor_version gl el args =
  match gl with
    Some (n1 :: _ as nl) ->
      check_use nl el;
      let ll =
        List.fold_right
          (fun e ll ->
             match e.name.expr with
               MLast.ExLid (_, _) ->
                 if List.exists (fun n -> e.name.tvar = n.tvar) nl then ll
                 else e.name :: ll
             | _ -> ll)
          el []
      in
      let globals =
        List.map
          (fun {expr = e; tvar = x; loc = loc} ->
             MLast.PaAny loc,
             MLast.ExTyc
               (loc, e,
                MLast.TyApp
                  (loc,
                   MLast.TyAcc
                     (loc,
                      MLast.TyAcc
                        (loc, MLast.TyUid (loc, gmod),
                         MLast.TyUid (loc, "Entry")),
                      MLast.TyLid (loc, "e")),
                   MLast.TyQuo (loc, x))))
          nl
      in
      let locals =
        List.map
          (fun {expr = e; tvar = x; loc = loc} ->
             let i =
               match e with
                 MLast.ExLid (_, i) -> i
               | _ -> failwith "internal error in pa_extend"
             in
             MLast.PaLid (loc, i),
             MLast.ExTyc
               (loc,
                MLast.ExApp
                  (loc, MLast.ExLid (loc, "grammar_entry_create"),
                   MLast.ExStr (loc, i)),
                MLast.TyApp
                  (loc,
                   MLast.TyAcc
                     (loc,
                      MLast.TyAcc
                        (loc, MLast.TyUid (loc, gmod),
                         MLast.TyUid (loc, "Entry")),
                      MLast.TyLid (loc, "e")),
                   MLast.TyQuo (loc, x))))
          ll
      in
      let e =
        if ll = [] then args
        else if functor_version then
          MLast.ExLet
            (loc, false,
             [MLast.PaLid (loc, "grammar_entry_create"),
              MLast.ExAcc
                (loc,
                 MLast.ExAcc
                   (loc, MLast.ExUid (loc, gmod), MLast.ExUid (loc, "Entry")),
                 MLast.ExLid (loc, "create"))],
             MLast.ExLet (loc, false, locals, args))
        else
          MLast.ExLet
            (loc, false,
             [MLast.PaLid (loc, "grammar_entry_create"),
              MLast.ExFun
                (loc,
                 [MLast.PaLid (loc, "s"), None,
                  MLast.ExApp
                    (loc,
                     MLast.ExApp
                       (loc,
                        MLast.ExAcc
                          (loc,
                           MLast.ExAcc
                             (loc, MLast.ExUid (loc, gmod),
                              MLast.ExUid (loc, "Entry")),
                           MLast.ExLid (loc, "create")),
                        MLast.ExApp
                          (loc,
                           MLast.ExAcc
                             (loc, MLast.ExUid (loc, gmod),
                              MLast.ExLid (loc, "of_entry")),
                           locate n1)),
                     MLast.ExLid (loc, "s"))])],
             MLast.ExLet (loc, false, locals, args))
      in
      MLast.ExLet (loc, false, globals, e)
  | _ -> args
;;

let text_of_extend loc gmod gl el f =
  if !split_ext then
    let args =
      List.map
        (fun e ->
           let (ent, pos, txt) = text_of_entry e.name.loc gmod e in
           let ent =
             MLast.ExApp
               (loc,
                MLast.ExAcc
                  (loc,
                   MLast.ExAcc
                     (loc, MLast.ExUid (loc, gmod),
                      MLast.ExUid (loc, "Entry")),
                   MLast.ExLid (loc, "obj")),
                ent)
           in
           let e = MLast.ExTup (loc, [ent; pos; txt]) in
           MLast.ExLet
             (loc, false,
              [MLast.PaLid (loc, "aux"),
               MLast.ExFun
                 (loc,
                  [MLast.PaUid (loc, "()"), None,
                   MLast.ExApp
                     (loc, f,
                      MLast.ExApp
                        (loc, MLast.ExApp (loc, MLast.ExUid (loc, "::"), e),
                         MLast.ExUid (loc, "[]")))])],
              MLast.ExApp
                (loc, MLast.ExLid (loc, "aux"), MLast.ExUid (loc, "()"))))
        el
    in
    let args = MLast.ExSeq (loc, args) in
    let_in_of_extend loc gmod false gl el args
  else
    let args =
      List.fold_right
        (fun e el ->
           let (ent, pos, txt) = text_of_entry e.name.loc gmod e in
           let ent =
             MLast.ExApp
               (loc,
                MLast.ExAcc
                  (loc,
                   MLast.ExAcc
                     (loc, MLast.ExUid (loc, gmod),
                      MLast.ExUid (loc, "Entry")),
                   MLast.ExLid (loc, "obj")),
                ent)
           in
           let e = MLast.ExTup (loc, [ent; pos; txt]) in
           MLast.ExApp
             (loc, MLast.ExApp (loc, MLast.ExUid (loc, "::"), e), el))
        el (MLast.ExUid (loc, "[]"))
    in
    let args = let_in_of_extend loc gmod false gl el args in
    MLast.ExApp (loc, f, args)
;;

let text_of_functorial_extend loc gmod gl el =
  let args =
    let el =
      List.map
        (fun e ->
           let (ent, pos, txt) = text_of_entry e.name.loc gmod e in
           let e =
             MLast.ExApp
               (loc,
                MLast.ExApp
                  (loc,
                   MLast.ExApp
                     (loc,
                      MLast.ExAcc
                        (loc, MLast.ExUid (loc, gmod),
                         MLast.ExLid (loc, "extend")),
                      ent),
                   pos),
                txt)
           in
           if !split_ext then
             MLast.ExLet
               (loc, false,
                [MLast.PaLid (loc, "aux"),
                 MLast.ExFun (loc, [MLast.PaUid (loc, "()"), None, e])],
                MLast.ExApp
                  (loc, MLast.ExLid (loc, "aux"), MLast.ExUid (loc, "()")))
           else e)
        el
    in
    MLast.ExSeq (loc, el)
  in
  let_in_of_extend loc gmod true gl el args
;;

let expr_of_delete_rule loc gmod n sl =
  let sl =
    List.fold_right
      (fun s e ->
         MLast.ExApp
           (loc, MLast.ExApp (loc, MLast.ExUid (loc, "::"), s.text gmod ""),
            e))
      sl (MLast.ExUid (loc, "[]"))
  in
  n.expr, sl
;;

let sself loc gmod n =
  MLast.ExAcc (loc, MLast.ExUid (loc, "Gramext"), MLast.ExUid (loc, "Sself"))
;;
let snext loc gmod n =
  MLast.ExAcc (loc, MLast.ExUid (loc, "Gramext"), MLast.ExUid (loc, "Snext"))
;;
let snterm loc n lev gmod tvar =
  match lev with
    Some lab ->
      MLast.ExApp
        (loc,
         MLast.ExApp
           (loc,
            MLast.ExAcc
              (loc, MLast.ExUid (loc, "Gramext"),
               MLast.ExUid (loc, "Snterml")),
            MLast.ExApp
              (loc,
               MLast.ExAcc
                 (loc,
                  MLast.ExAcc
                    (loc, MLast.ExUid (loc, gmod),
                     MLast.ExUid (loc, "Entry")),
                  MLast.ExLid (loc, "obj")),
               MLast.ExTyc
                 (loc, n.expr,
                  MLast.TyApp
                    (loc,
                     MLast.TyAcc
                       (loc,
                        MLast.TyAcc
                          (loc, MLast.TyUid (loc, gmod),
                           MLast.TyUid (loc, "Entry")),
                        MLast.TyLid (loc, "e")),
                     MLast.TyQuo (loc, n.tvar))))),
         MLast.ExStr (loc, lab))
  | None ->
      if n.tvar = tvar then sself loc gmod tvar
      else
        MLast.ExApp
          (loc,
           MLast.ExAcc
             (loc, MLast.ExUid (loc, "Gramext"), MLast.ExUid (loc, "Snterm")),
           MLast.ExApp
             (loc,
              MLast.ExAcc
                (loc,
                 MLast.ExAcc
                   (loc, MLast.ExUid (loc, gmod), MLast.ExUid (loc, "Entry")),
                 MLast.ExLid (loc, "obj")),
              MLast.ExTyc
                (loc, n.expr,
                 MLast.TyApp
                   (loc,
                    MLast.TyAcc
                      (loc,
                       MLast.TyAcc
                         (loc, MLast.TyUid (loc, gmod),
                          MLast.TyUid (loc, "Entry")),
                       MLast.TyLid (loc, "e")),
                    MLast.TyQuo (loc, n.tvar)))))
;;
let slist loc min sep symb gmod n =
  let txt = symb.text gmod "" in
  match min, sep with
    false, None ->
      MLast.ExApp
        (loc,
         MLast.ExAcc
           (loc, MLast.ExUid (loc, "Gramext"), MLast.ExUid (loc, "Slist0")),
         txt)
  | true, None ->
      MLast.ExApp
        (loc,
         MLast.ExAcc
           (loc, MLast.ExUid (loc, "Gramext"), MLast.ExUid (loc, "Slist1")),
         txt)
  | false, Some s ->
      let x = s.text gmod n in
      MLast.ExApp
        (loc,
         MLast.ExApp
           (loc,
            MLast.ExAcc
              (loc, MLast.ExUid (loc, "Gramext"),
               MLast.ExUid (loc, "Slist0sep")),
            txt),
         x)
  | true, Some s ->
      let x = s.text gmod n in
      MLast.ExApp
        (loc,
         MLast.ExApp
           (loc,
            MLast.ExAcc
              (loc, MLast.ExUid (loc, "Gramext"),
               MLast.ExUid (loc, "Slist1sep")),
            txt),
         x)
;;
let sopt loc symb gmod n =
  let txt = symb.text gmod "" in
  MLast.ExApp
    (loc,
     MLast.ExAcc
       (loc, MLast.ExUid (loc, "Gramext"), MLast.ExUid (loc, "Sopt")),
     txt)
;;
let srules loc t rl gmod tvar =
  let e = text_of_rule_list loc gmod t rl "" in
  MLast.ExApp
    (loc,
     MLast.ExAcc
       (loc, MLast.ExUid (loc, "Gramext"), MLast.ExLid (loc, "srules")),
     e)
;;

let rec ident_of_expr =
  function
    MLast.ExLid (_, s) -> s
  | MLast.ExUid (_, s) -> s
  | MLast.ExAcc (_, e1, e2) -> ident_of_expr e1 ^ "__" ^ ident_of_expr e2
  | _ -> failwith "internal error in pa_extend"
;;

let mk_name loc e = {expr = e; tvar = ident_of_expr e; loc = loc};;

open Pcaml;;
let symbol = Grammar.Entry.create gram "symbol";;

Grammar.extend
  (let _ = (expr : 'expr Grammar.Entry.e)
   and _ = (symbol : 'symbol Grammar.Entry.e) in
   let grammar_entry_create s =
     Grammar.Entry.create (Grammar.of_entry expr) s
   in
   let extend_body : 'extend_body Grammar.Entry.e =
     grammar_entry_create "extend_body"
   and gextend_body : 'gextend_body Grammar.Entry.e =
     grammar_entry_create "gextend_body"
   and delete_rule_body : 'delete_rule_body Grammar.Entry.e =
     grammar_entry_create "delete_rule_body"
   and gdelete_rule_body : 'gdelete_rule_body Grammar.Entry.e =
     grammar_entry_create "gdelete_rule_body"
   and efunction : 'efunction Grammar.Entry.e =
     grammar_entry_create "efunction"
   and global : 'global Grammar.Entry.e = grammar_entry_create "global"
   and entry : 'entry Grammar.Entry.e = grammar_entry_create "entry"
   and position : 'position Grammar.Entry.e = grammar_entry_create "position"
   and level_list : 'level_list Grammar.Entry.e =
     grammar_entry_create "level_list"
   and level : 'level Grammar.Entry.e = grammar_entry_create "level"
   and assoc : 'assoc Grammar.Entry.e = grammar_entry_create "assoc"
   and rule_list : 'rule_list Grammar.Entry.e =
     grammar_entry_create "rule_list"
   and rule : 'rule Grammar.Entry.e = grammar_entry_create "rule"
   and psymbol : 'psymbol Grammar.Entry.e = grammar_entry_create "psymbol"
   and pattern : 'pattern Grammar.Entry.e = grammar_entry_create "pattern"
   and patterns_comma : 'patterns_comma Grammar.Entry.e =
     grammar_entry_create "patterns_comma"
   and name : 'name Grammar.Entry.e = grammar_entry_create "name"
   and qualid : 'qualid Grammar.Entry.e = grammar_entry_create "qualid"
   and string : 'string Grammar.Entry.e = grammar_entry_create "string" in
   [Grammar.Entry.obj (expr : 'expr Grammar.Entry.e),
    Some (Gramext.After "top"),
    [None, None,
     [[Gramext.Stoken ("", "GDELETE_RULE");
       Gramext.Snterm
         (Grammar.Entry.obj
            (gdelete_rule_body : 'gdelete_rule_body Grammar.Entry.e));
       Gramext.Stoken ("", "END")],
      Gramext.action
        (fun _ (e : 'gdelete_rule_body) _ (loc : int * int) -> (e : 'expr));
      [Gramext.Stoken ("", "DELETE_RULE");
       Gramext.Snterm
         (Grammar.Entry.obj
            (delete_rule_body : 'delete_rule_body Grammar.Entry.e));
       Gramext.Stoken ("", "END")],
      Gramext.action
        (fun _ (e : 'delete_rule_body) _ (loc : int * int) -> (e : 'expr));
      [Gramext.Stoken ("", "GEXTEND");
       Gramext.Snterm
         (Grammar.Entry.obj (gextend_body : 'gextend_body Grammar.Entry.e));
       Gramext.Stoken ("", "END")],
      Gramext.action
        (fun _ (e : 'gextend_body) _ (loc : int * int) -> (e : 'expr));
      [Gramext.Stoken ("", "EXTEND");
       Gramext.Snterm
         (Grammar.Entry.obj (extend_body : 'extend_body Grammar.Entry.e));
       Gramext.Stoken ("", "END")],
      Gramext.action
        (fun _ (e : 'extend_body) _ (loc : int * int) -> (e : 'expr))]];
    Grammar.Entry.obj (extend_body : 'extend_body Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj (efunction : 'efunction Grammar.Entry.e));
       Gramext.Sopt
         (Gramext.Snterm
            (Grammar.Entry.obj (global : 'global Grammar.Entry.e)));
       Gramext.Slist1
         (Gramext.srules
            [[Gramext.Snterm
                (Grammar.Entry.obj (entry : 'entry Grammar.Entry.e));
              Gramext.Stoken ("", ";")],
             Gramext.action
               (fun _ (e : 'entry) (loc : int * int) -> (e : 'e__1))])],
      Gramext.action
        (fun (el : 'e__1 list) (sl : 'global option) (f : 'efunction)
           (loc : int * int) ->
           (text_of_extend loc "Grammar" sl el f : 'extend_body))]];
    Grammar.Entry.obj (gextend_body : 'gextend_body Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("UIDENT", "");
       Gramext.Sopt
         (Gramext.Snterm
            (Grammar.Entry.obj (global : 'global Grammar.Entry.e)));
       Gramext.Slist1
         (Gramext.srules
            [[Gramext.Snterm
                (Grammar.Entry.obj (entry : 'entry Grammar.Entry.e));
              Gramext.Stoken ("", ";")],
             Gramext.action
               (fun _ (e : 'entry) (loc : int * int) -> (e : 'e__2))])],
      Gramext.action
        (fun (el : 'e__2 list) (sl : 'global option) (g : string)
           (loc : int * int) ->
           (text_of_functorial_extend loc g sl el : 'gextend_body))]];
    Grammar.Entry.obj (delete_rule_body : 'delete_rule_body Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (name : 'name Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Slist1sep
         (Gramext.Snterm
            (Grammar.Entry.obj (symbol : 'symbol Grammar.Entry.e)),
          Gramext.Stoken ("", ";"))],
      Gramext.action
        (fun (sl : 'symbol list) _ (n : 'name) (loc : int * int) ->
           (let (e, b) = expr_of_delete_rule loc "Grammar" n sl in
            MLast.ExApp
              (loc,
               MLast.ExApp
                 (loc,
                  MLast.ExAcc
                    (loc, MLast.ExUid (loc, "Grammar"),
                     MLast.ExLid (loc, "delete_rule")),
                  e),
               b) :
            'delete_rule_body))]];
    Grammar.Entry.obj
      (gdelete_rule_body : 'gdelete_rule_body Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Stoken ("UIDENT", "");
       Gramext.Snterm (Grammar.Entry.obj (name : 'name Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Slist1sep
         (Gramext.Snterm
            (Grammar.Entry.obj (symbol : 'symbol Grammar.Entry.e)),
          Gramext.Stoken ("", ";"))],
      Gramext.action
        (fun (sl : 'symbol list) _ (n : 'name) (g : string)
           (loc : int * int) ->
           (let (e, b) = expr_of_delete_rule loc g n sl in
            MLast.ExApp
              (loc,
               MLast.ExApp
                 (loc,
                  MLast.ExAcc
                    (loc, MLast.ExUid (loc, g),
                     MLast.ExLid (loc, "delete_rule")),
                  e),
               b) :
            'gdelete_rule_body))]];
    Grammar.Entry.obj (efunction : 'efunction Grammar.Entry.e), None,
    [None, None,
     [[],
      Gramext.action
        (fun (loc : int * int) ->
           (MLast.ExAcc
              (loc, MLast.ExUid (loc, "Grammar"),
               MLast.ExLid (loc, "extend")) :
            'efunction));
      [Gramext.Stoken ("UIDENT", "FUNCTION"); Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (qualid : 'qualid Grammar.Entry.e));
       Gramext.Stoken ("", ";")],
      Gramext.action
        (fun _ (f : 'qualid) _ _ (loc : int * int) -> (f : 'efunction))]];
    Grammar.Entry.obj (global : 'global Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("UIDENT", "GLOBAL"); Gramext.Stoken ("", ":");
       Gramext.Slist1
         (Gramext.Snterm (Grammar.Entry.obj (name : 'name Grammar.Entry.e)));
       Gramext.Stoken ("", ";")],
      Gramext.action
        (fun _ (sl : 'name list) _ _ (loc : int * int) -> (sl : 'global))]];
    Grammar.Entry.obj (entry : 'entry Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (name : 'name Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Sopt
         (Gramext.Snterm
            (Grammar.Entry.obj (position : 'position Grammar.Entry.e)));
       Gramext.Snterm
         (Grammar.Entry.obj (level_list : 'level_list Grammar.Entry.e))],
      Gramext.action
        (fun (ll : 'level_list) (pos : 'position option) _ (n : 'name)
           (loc : int * int) ->
           ({name = n; pos = pos; levels = ll} : 'entry))]];
    Grammar.Entry.obj (position : 'position Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("UIDENT", "LEVEL");
       Gramext.Snterm (Grammar.Entry.obj (string : 'string Grammar.Entry.e))],
      Gramext.action
        (fun (n : 'string) _ (loc : int * int) ->
           (MLast.ExApp
              (loc,
               MLast.ExAcc
                 (loc, MLast.ExUid (loc, "Gramext"),
                  MLast.ExUid (loc, "Level")),
               n) :
            'position));
      [Gramext.Stoken ("UIDENT", "AFTER");
       Gramext.Snterm (Grammar.Entry.obj (string : 'string Grammar.Entry.e))],
      Gramext.action
        (fun (n : 'string) _ (loc : int * int) ->
           (MLast.ExApp
              (loc,
               MLast.ExAcc
                 (loc, MLast.ExUid (loc, "Gramext"),
                  MLast.ExUid (loc, "After")),
               n) :
            'position));
      [Gramext.Stoken ("UIDENT", "BEFORE");
       Gramext.Snterm (Grammar.Entry.obj (string : 'string Grammar.Entry.e))],
      Gramext.action
        (fun (n : 'string) _ (loc : int * int) ->
           (MLast.ExApp
              (loc,
               MLast.ExAcc
                 (loc, MLast.ExUid (loc, "Gramext"),
                  MLast.ExUid (loc, "Before")),
               n) :
            'position));
      [Gramext.Stoken ("UIDENT", "LAST")],
      Gramext.action
        (fun _ (loc : int * int) ->
           (MLast.ExAcc
              (loc, MLast.ExUid (loc, "Gramext"), MLast.ExUid (loc, "Last")) :
            'position));
      [Gramext.Stoken ("UIDENT", "FIRST")],
      Gramext.action
        (fun _ (loc : int * int) ->
           (MLast.ExAcc
              (loc, MLast.ExUid (loc, "Gramext"),
               MLast.ExUid (loc, "First")) :
            'position))]];
    Grammar.Entry.obj (level_list : 'level_list Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "[");
       Gramext.Slist0sep
         (Gramext.Snterm (Grammar.Entry.obj (level : 'level Grammar.Entry.e)),
          Gramext.Stoken ("", "|"));
       Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (ll : 'level list) _ (loc : int * int) ->
           (ll : 'level_list))]];
    Grammar.Entry.obj (level : 'level Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Sopt (Gramext.Stoken ("STRING", ""));
       Gramext.Sopt
         (Gramext.Snterm
            (Grammar.Entry.obj (assoc : 'assoc Grammar.Entry.e)));
       Gramext.Snterm
         (Grammar.Entry.obj (rule_list : 'rule_list Grammar.Entry.e))],
      Gramext.action
        (fun (rules : 'rule_list) (ass : 'assoc option) (lab : string option)
           (loc : int * int) ->
           ({label = lab; assoc = ass; rules = rules} : 'level))]];
    Grammar.Entry.obj (assoc : 'assoc Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("UIDENT", "NONA")],
      Gramext.action
        (fun _ (loc : int * int) ->
           (MLast.ExAcc
              (loc, MLast.ExUid (loc, "Gramext"), MLast.ExUid (loc, "NonA")) :
            'assoc));
      [Gramext.Stoken ("UIDENT", "RIGHTA")],
      Gramext.action
        (fun _ (loc : int * int) ->
           (MLast.ExAcc
              (loc, MLast.ExUid (loc, "Gramext"),
               MLast.ExUid (loc, "RightA")) :
            'assoc));
      [Gramext.Stoken ("UIDENT", "LEFTA")],
      Gramext.action
        (fun _ (loc : int * int) ->
           (MLast.ExAcc
              (loc, MLast.ExUid (loc, "Gramext"),
               MLast.ExUid (loc, "LeftA")) :
            'assoc))]];
    Grammar.Entry.obj (rule_list : 'rule_list Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "[");
       Gramext.Slist1sep
         (Gramext.Snterm (Grammar.Entry.obj (rule : 'rule Grammar.Entry.e)),
          Gramext.Stoken ("", "|"));
       Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (rules : 'rule list) _ (loc : int * int) ->
           (retype_rule_list_without_patterns loc rules : 'rule_list));
      [Gramext.Stoken ("", "["); Gramext.Stoken ("", "]")],
      Gramext.action (fun _ _ (loc : int * int) -> ([] : 'rule_list))]];
    Grammar.Entry.obj (rule : 'rule Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Slist0sep
         (Gramext.Snterm
            (Grammar.Entry.obj (psymbol : 'psymbol Grammar.Entry.e)),
          Gramext.Stoken ("", ";"))],
      Gramext.action
        (fun (psl : 'psymbol list) (loc : int * int) ->
           ({prod = psl; action = None} : 'rule));
      [Gramext.Slist0sep
         (Gramext.Snterm
            (Grammar.Entry.obj (psymbol : 'psymbol Grammar.Entry.e)),
          Gramext.Stoken ("", ";"));
       Gramext.Stoken ("", "->");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (act : 'expr) _ (psl : 'psymbol list) (loc : int * int) ->
           ({prod = psl; action = Some act} : 'rule))]];
    Grammar.Entry.obj (psymbol : 'psymbol Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (symbol : 'symbol Grammar.Entry.e))],
      Gramext.action
        (fun (s : 'symbol) (loc : int * int) ->
           ({pattern = None; symbol = s} : 'psymbol));
      [Gramext.Snterm
         (Grammar.Entry.obj (pattern : 'pattern Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (symbol : 'symbol Grammar.Entry.e))],
      Gramext.action
        (fun (s : 'symbol) _ (p : 'pattern) (loc : int * int) ->
           ({pattern = Some p; symbol = s} : 'psymbol));
      [Gramext.Stoken ("LIDENT", "");
       Gramext.Sopt
         (Gramext.srules
            [[Gramext.Stoken ("UIDENT", "LEVEL");
              Gramext.Stoken ("STRING", "")],
             Gramext.action
               (fun (s : string) _ (loc : int * int) -> (s : 'e__3))])],
      Gramext.action
        (fun (lev : 'e__3 option) (i : string) (loc : int * int) ->
           (let name = mk_name loc (MLast.ExLid (loc, i)) in
            let text = snterm loc name lev in
            let styp _ = MLast.TyQuo (loc, i) in
            let symb = {used = [name]; text = text; styp = styp} in
            {pattern = None; symbol = symb} :
            'psymbol));
      [Gramext.Stoken ("LIDENT", ""); Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (symbol : 'symbol Grammar.Entry.e))],
      Gramext.action
        (fun (s : 'symbol) _ (p : string) (loc : int * int) ->
           ({pattern = Some (MLast.PaLid (loc, p)); symbol = s} :
            'psymbol))]];
    Grammar.Entry.obj (symbol : 'symbol Grammar.Entry.e), None,
    [Some "top", Some Gramext.NonA,
     [[Gramext.Stoken ("UIDENT", "OPT"); Gramext.Sself],
      Gramext.action
        (fun (s : 'symbol) _ (loc : int * int) ->
           (let styp n =
              let t = s.styp n in
              MLast.TyApp (loc, MLast.TyLid (loc, "option"), t)
            in
            {used = s.used; text = sopt loc s; styp = styp} :
            'symbol));
      [Gramext.Stoken ("UIDENT", "LIST1"); Gramext.Sself;
       Gramext.Sopt
         (Gramext.srules
            [[Gramext.Stoken ("UIDENT", "SEP");
              Gramext.Snterm
                (Grammar.Entry.obj (symbol : 'symbol Grammar.Entry.e))],
             Gramext.action
               (fun (t : 'symbol) _ (loc : int * int) -> (t : 'e__5))])],
      Gramext.action
        (fun (sep : 'e__5 option) (s : 'symbol) _ (loc : int * int) ->
           (let used =
              match sep with
                Some symb -> symb.used @ s.used
              | None -> s.used
            in
            let styp n =
              let t = s.styp n in
              MLast.TyApp (loc, MLast.TyLid (loc, "list"), t)
            in
            {used = used; text = slist loc true sep s; styp = styp} :
            'symbol));
      [Gramext.Stoken ("UIDENT", "LIST0"); Gramext.Sself;
       Gramext.Sopt
         (Gramext.srules
            [[Gramext.Stoken ("UIDENT", "SEP");
              Gramext.Snterm
                (Grammar.Entry.obj (symbol : 'symbol Grammar.Entry.e))],
             Gramext.action
               (fun (t : 'symbol) _ (loc : int * int) -> (t : 'e__4))])],
      Gramext.action
        (fun (sep : 'e__4 option) (s : 'symbol) _ (loc : int * int) ->
           (let used =
              match sep with
                Some symb -> symb.used @ s.used
              | None -> s.used
            in
            let styp n =
              let t = s.styp n in
              MLast.TyApp (loc, MLast.TyLid (loc, "list"), t)
            in
            {used = used; text = slist loc false sep s; styp = styp} :
            'symbol))];
     None, None,
     [[Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (s_t : 'symbol) _ (loc : int * int) -> (s_t : 'symbol));
      [Gramext.Snterm (Grammar.Entry.obj (name : 'name Grammar.Entry.e));
       Gramext.Sopt
         (Gramext.srules
            [[Gramext.Stoken ("UIDENT", "LEVEL");
              Gramext.Stoken ("STRING", "")],
             Gramext.action
               (fun (s : string) _ (loc : int * int) -> (s : 'e__7))])],
      Gramext.action
        (fun (lev : 'e__7 option) (n : 'name) (loc : int * int) ->
           ({used = [n]; text = snterm loc n lev;
             styp = fun _ -> MLast.TyQuo (loc, n.tvar)} :
            'symbol));
      [Gramext.Stoken ("UIDENT", ""); Gramext.Stoken ("", ".");
       Gramext.Snterm (Grammar.Entry.obj (qualid : 'qualid Grammar.Entry.e));
       Gramext.Sopt
         (Gramext.srules
            [[Gramext.Stoken ("UIDENT", "LEVEL");
              Gramext.Stoken ("STRING", "")],
             Gramext.action
               (fun (s : string) _ (loc : int * int) -> (s : 'e__6))])],
      Gramext.action
        (fun (lev : 'e__6 option) (e : 'qualid) _ (i : string)
           (loc : int * int) ->
           (let n =
              mk_name loc (MLast.ExAcc (loc, MLast.ExUid (loc, i), e))
            in
            {used = [n]; text = snterm loc n lev;
             styp = fun _ -> MLast.TyQuo (loc, n.tvar)} :
            'symbol));
      [Gramext.Snterm (Grammar.Entry.obj (string : 'string Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'string) (loc : int * int) ->
           ({used = [];
             text =
               (fun _ _ ->
                  MLast.ExApp
                    (loc,
                     MLast.ExAcc
                       (loc, MLast.ExUid (loc, "Gramext"),
                        MLast.ExUid (loc, "Stoken")),
                     MLast.ExTup (loc, [MLast.ExStr (loc, ""); e])));
             styp = fun _ -> MLast.TyLid (loc, "string")} :
            'symbol));
      [Gramext.Stoken ("UIDENT", "");
       Gramext.Snterm (Grammar.Entry.obj (string : 'string Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'string) (x : string) (loc : int * int) ->
           ({used = [];
             text =
               (fun _ _ ->
                  MLast.ExApp
                    (loc,
                     MLast.ExAcc
                       (loc, MLast.ExUid (loc, "Gramext"),
                        MLast.ExUid (loc, "Stoken")),
                     MLast.ExTup (loc, [MLast.ExStr (loc, x); e])));
             styp = fun _ -> MLast.TyLid (loc, "string")} :
            'symbol));
      [Gramext.Stoken ("UIDENT", "")],
      Gramext.action
        (fun (x : string) (loc : int * int) ->
           ({used = [];
             text =
               (fun _ _ ->
                  MLast.ExApp
                    (loc,
                     MLast.ExAcc
                       (loc, MLast.ExUid (loc, "Gramext"),
                        MLast.ExUid (loc, "Stoken")),
                     MLast.ExTup
                       (loc, [MLast.ExStr (loc, x); MLast.ExStr (loc, "")])));
             styp = fun _ -> MLast.TyLid (loc, "string")} :
            'symbol));
      [Gramext.Stoken ("", "[");
       Gramext.Slist0sep
         (Gramext.Snterm (Grammar.Entry.obj (rule : 'rule Grammar.Entry.e)),
          Gramext.Stoken ("", "|"));
       Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (rl : 'rule list) _ (loc : int * int) ->
           (let rl = retype_rule_list_without_patterns loc rl in
            let t = new_type_var () in
            {used = used_of_rule_list rl; text = srules loc t rl;
             styp = fun _ -> MLast.TyQuo (loc, t)} :
            'symbol));
      [Gramext.Stoken ("UIDENT", "NEXT")],
      Gramext.action
        (fun _ (loc : int * int) ->
           (let styp n =
              if n = "" then
                Stdpp.raise_with_loc loc
                  (Stream.Error "'NEXT' illegal in anonymous entry level")
              else MLast.TyQuo (loc, n)
            in
            {used = []; text = snext loc; styp = styp} :
            'symbol));
      [Gramext.Stoken ("UIDENT", "SELF")],
      Gramext.action
        (fun _ (loc : int * int) ->
           (let styp n =
              if n = "" then
                Stdpp.raise_with_loc loc
                  (Stream.Error "'SELF' illegal in anonymous entry level")
              else MLast.TyQuo (loc, n)
            in
            {used = []; text = sself loc; styp = styp} :
            'symbol))]];
    Grammar.Entry.obj (pattern : 'pattern Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ",");
       Gramext.Snterm
         (Grammar.Entry.obj
            (patterns_comma : 'patterns_comma Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (pl : 'patterns_comma) _ (p : 'pattern) _ (loc : int * int) ->
           (MLast.PaTup (loc, (p :: pl)) : 'pattern));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (p : 'pattern) _ (loc : int * int) -> (p : 'pattern));
      [Gramext.Stoken ("", "_")],
      Gramext.action
        (fun _ (loc : int * int) -> (MLast.PaAny loc : 'pattern));
      [Gramext.Stoken ("LIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) ->
           (MLast.PaLid (loc, i) : 'pattern))]];
    Grammar.Entry.obj (patterns_comma : 'patterns_comma Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Sself; Gramext.Stoken ("", ",");
       Gramext.Snterm
         (Grammar.Entry.obj (pattern : 'pattern Grammar.Entry.e))],
      Gramext.action
        (fun (p : 'pattern) _ (pl : 'patterns_comma) (loc : int * int) ->
           (pl @ [p] : 'patterns_comma))];
     None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj (pattern : 'pattern Grammar.Entry.e))],
      Gramext.action
        (fun (p : 'pattern) (loc : int * int) -> ([p] : 'patterns_comma))]];
    Grammar.Entry.obj (name : 'name Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (qualid : 'qualid Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'qualid) (loc : int * int) -> (mk_name loc e : 'name))]];
    Grammar.Entry.obj (qualid : 'qualid Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Sself; Gramext.Stoken ("", "."); Gramext.Sself],
      Gramext.action
        (fun (e2 : 'qualid) _ (e1 : 'qualid) (loc : int * int) ->
           (MLast.ExAcc (loc, e1, e2) : 'qualid))];
     None, None,
     [[Gramext.Stoken ("LIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) ->
           (MLast.ExLid (loc, i) : 'qualid));
      [Gramext.Stoken ("UIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) ->
           (MLast.ExUid (loc, i) : 'qualid))]];
    Grammar.Entry.obj (string : 'string Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) ->
           (let shift = fst loc + String.length "$" in
            let e =
              try Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string i) with
                Exc_located ((bp, ep), exc) ->
                  raise_with_loc (shift + bp, shift + ep) exc
            in
            Pcaml.expr_reloc (fun (bp, ep) -> shift + bp, shift + ep) 0 e :
            'string));
      [Gramext.Stoken ("STRING", "")],
      Gramext.action
        (fun (s : string) (loc : int * int) ->
           (MLast.ExStr (loc, s) : 'string))]]]);;
