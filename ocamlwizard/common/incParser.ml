open Diff
open IncLexer
open Lexing
open Parsing

(* Ultimately, we would like to save parsing some states permanently,
   and get modifications directly from the editor so that the parser
   can restore just before the modification. Symetrically, we could
   also cache the result of parsing some part of the file with a given
   initial parsing stack to avoid re-parsing the end of the file
   (though this would require shifting all locations). *)

(* Un essai par continuation, qui ne peut pas marcher puisque la fonction
   refill n'a pas de continuation *)

(*
let rec best_parse acc cond = function
  | Same s :: d ->
    best_parse (acc ^ s) cond d
  | Changed (old, last) :: d ->
    (try
       best_parse (acc ^ last) cond d
     with
	 Not_found ->
	   best_parse (acc ^ old) cond d)
  | [] -> cond acc

type ('a, 'b) lazy_list =
  | Nil
  | Cons of (('a -> ('a, 'b) lazy_list -> 'b) -> 'b)

let rec best_parse = function
  | Same s :: d ->
    Cons (function k -> k s (best_parse d))
  | Changed (old, last) :: d ->
    Cons
      (function k ->
	try
	  k last (best_parse d)
	with
	    Not_found ->
	      k old (best_parse d))
  | [] -> Nil

let rec fold_left k f acc = function
  | Nil -> k acc
  | Cons k' -> k' (fun t q -> fold_left k f (f acc t) q)

let to_list k l =
  fold_left (function l -> k (List.rev l)) (fun q t -> t :: q) [] l

(*
let rec best_parse f = function
  | Same s :: d ->
    f (Cons (fun k -> k s (best_parse (assert false) d)))
(*
    cond (`some (s, (function cond -> (best_parse cond d))))
  | Changed (old, last) :: d ->
    (try
       cond (`some (last, (function cond -> (best_parse cond d))))
     with
	 Not_found ->
	   cond (`some (old, (function cond -> (best_parse cond d)))))
*)
  | [] -> f Nil

let rec length = function
  | `nil -> 0
  | `cons (_, q) -> q (function l -> length l + 1)

let rec best_parse cond = function
  | Same s :: d ->
    cond (`some (s, (function cond -> (best_parse cond d))))
  | Changed (old, last) :: d ->
    (try
       cond (`some (last, (function cond -> (best_parse cond d))))
     with
	 Not_found ->
	   cond (`some (old, (function cond -> (best_parse cond d)))))
  | [] -> cond `none
*)

(*
let best_parse =
  let rec cond = function
    | `some s, l -> l (function l -> cond (s :: l))
  in
()
*)

let try_parse parse lexer file =
  prerr_endline "***************** Try parsing *****************\n";
  prerr_endline file;
  let lexbuf = Lexing.from_string file in
  try
    let ast = parse lexer lexbuf in
    print_endline file;
    ast
  with
      _ -> raise Not_found

let try_parse parse lexer file =
  prerr_endline "***************** Try parsing *****************\n";
  let file = List.fold_left ( ^ ) "" file in
  prerr_endline file;
  let lexbuf = Lexing.from_string file in
  try
    let ast = parse lexer lexbuf in
    print_endline file;
    ast
  with
      _ -> raise Not_found

let parse_with_errors parse lexer old_file new_file =
  let l = best_parse (read_modified_file old_file new_file) in
  to_list (try_parse parse lexer) l

(* string lazy_list -> token lazy_list *)
let lexe l =
  let lexbuf =
    {Lexing.from_string "" with
      refill_buff = function b ->
	match l with
	  | Nil -> b.lex_eof_reached <- true
	  | Cons k ->
	    k (fun t q ->
	      b.lex_buffer <- b.lex_buffer ^  }

(*
let try_parse parse lexer file rev_diff =
  let file =
    List.fold_left
      (fun file l -> file ^ l)
      ""
      (apply_diff (1, List.rev rev_diff, file))
  in
    prerr_endline "***************** Try parsing *****************\n";
    prerr_endline file;
  let lexbuf = Lexing.from_string file in
    try
      parse lexer lexbuf
    with
	_ -> raise Not_found
*)

(* Given a list l, best_lexicographic [] l returns cond (rev l') for
   the best sub-list l' of l such that cond (rev l') does not raise Not_found *)
let rec best_lexicographic acc cond = function
  | i :: d ->
      (try
	 best_lexicographic (i :: acc ) cond d
       with
	   Not_found ->
	     best_lexicographic acc cond d)
  | [] -> cond acc

let parse_with_errors parse lexer old_file new_file =
  let diff_file = Filename.temp_file
    (Filename.basename new_file ^ "-" ^ Filename.basename old_file) ".diff" in
    (match Sys.command ("diff -f " ^ old_file ^ " " ^ new_file ^ " >" ^ diff_file) with
       | 0 -> ()
       | _ -> failwith "error when invoking diff");
    let old = lines_of old_file
    and diff = parse_diff_file diff_file in
      best_lexicographic [] (try_parse parse lexer old) diff
*)
(*
let initial_env () =
  { s_stack = Array.create 100 0;
    v_stack = Array.create 100 (Obj.repr ());
    symb_start_stack = Array.create 100 Lexing.dummy_pos;
    symb_end_stack = Array.create 100 Lexing.dummy_pos;
    stacksize = 100;
    stackbase = 0;
    curr_char = 0;
    lval = Obj.repr ();
    symb_start = Lexing.dummy_pos;
    symb_end = Lexing.dummy_pos;
    asp = 0;
    rule_len = 0;
    rule_number = 0;
    sp = 0;
    state = 0;
    errflag = 0 }
*)

let print_env env =
  Printf.printf "stackbase=%d ; curr_char=%d, asp=%d, sp=%d\n%!"
    env.stackbase env.curr_char env.asp env.sp

(* Copy of Parsing.grow_stacks *)
let grow_stacks env =
  let oldsize = env.stacksize in
  let newsize = oldsize * 2 in
  let new_s = Array.create newsize 0
  and new_v = Array.create newsize (Obj.repr ())
  and new_start = Array.create newsize dummy_pos
  and new_end = Array.create newsize dummy_pos in
    Array.blit env.s_stack 0 new_s 0 oldsize;
    env.s_stack <- new_s;
    Array.blit env.v_stack 0 new_v 0 oldsize;
    env.v_stack <- new_v;
    Array.blit env.symb_start_stack 0 new_start 0 oldsize;
    env.symb_start_stack <- new_start;
    Array.blit env.symb_end_stack 0 new_end 0 oldsize;
    env.symb_end_stack <- new_end;
    env.stacksize <- newsize

let copy_env env = {
  env with
    s_stack = Array.copy env.s_stack;
    v_stack = Array.copy env.v_stack;
    symb_start_stack = Array.copy env.symb_start_stack;
    symb_end_stack = Array.copy env.symb_end_stack
  }

let restore_env_from env =
  Parsing.env.s_stack <- Array.copy env.s_stack;
  Parsing.env.v_stack <- Array.copy env.v_stack;
  Parsing.env.symb_start_stack <- Array.copy env.symb_start_stack;
  Parsing.env.symb_end_stack <- Array.copy env.symb_end_stack;
  Parsing.env.stacksize <- env.stacksize;
  Parsing.env.stackbase <- env.stackbase;
  Parsing.env.curr_char <- env.curr_char;
  Parsing.env.lval <- env.lval;
  Parsing.env.symb_start <- env.symb_start;
  Parsing.env.symb_end <- env.symb_end;
  Parsing.env.asp <- env.asp;
  Parsing.env.rule_len <- env.rule_len;
  Parsing.env.rule_number <- env.rule_number;
  Parsing.env.sp <- env.sp;
  Parsing.env.state <- env.state;
  Parsing.env.errflag <- env.errflag


(* Backtracking version of Parsing.yyparse. The lexer function now
   takes two additional arguments: push is called by the lexer when it
   saves a backtracking point and allows the parser to save its state
   as well ; pop is called in case of backtracking, and should be used
   to restore the parser state. Finally, a backtracking function is
   returned together with the lexer, which should be called in case of
   parse error. *)
let backtracking_yyparse tables start make_lexer =
(*
  let env = ref (initial_env ())
*)
  let env_snapshot = ref (copy_env env) (*(initial_env ())*)
  and cmd = ref Start
  and arg = ref (Obj.repr ())
  and stack = Stack.create ()
  and current_lookahead_fun = ref (fun (x : Obj.t) -> false) in
  let push () =
    (*
      print_endline "pushing snapshot";
      print_env ! env_snapshot;
    *)
    Stack.push
      Owz_lexer.(
	!string_buff, !string_index, !string_start_loc, !comment_start_loc,
	!env_snapshot, !cmd, !arg
      )
      stack
  and pop () =
    let buff, index, string_start, comment_start, env', cmd', arg' =
      Stack.pop stack in
    Owz_lexer.(
      string_buff := buff;
      string_index := index;
      string_start_loc := string_start;
      comment_start_loc := comment_start;
      (* We need a second copy because the snapshot can be shared
	 between several states on the backtracking stack. *)
(*
      env := copy_env env';
*)
      restore_env_from env';
      env_snapshot := env';
      cmd := cmd';
      arg := arg'
    )
  in
  let initial_pos, lexe, backtrack = make_lexer ~push ~pop in

  let init_asp = env.asp
  and init_sp = env.sp
  and init_stackbase = env.stackbase
  and init_state = env.state
  and init_curr_char = env.curr_char
  and init_errflag = env.errflag in
  env.stackbase <- env.sp + 1;
  env.curr_char <- start;
  env.symb_end <- initial_pos;
  try
    let loop c a = cmd := c ; arg := a in
    while true do
      env_snapshot := copy_env env;
      (*
	print_env env;
      *)
      try 
	match Parsing.parse_engine tables env !cmd !arg with
	    Read_token ->
	      let t, s, e = lexe () in
              env.symb_start <- s;
              env.symb_end <- e;
              loop Token_read (Obj.repr t)
	  | Raise_parse_error ->
	    raise Parse_error
	  | Compute_semantic_action ->
            let (action, value) =
              try
		(Semantic_action_computed, tables.actions.(env.rule_number) env)
              with Parse_error ->
		(Error_detected, Obj.repr ()) in
            loop action value
	  | Grow_stacks_1 ->
            grow_stacks env; loop Stacks_grown_1 (Obj.repr ())
	  | Grow_stacks_2 ->
            grow_stacks env; loop Stacks_grown_2 (Obj.repr ())
	  | Call_error_function ->
            tables.error_function "syntax error";
            loop Error_detected (Obj.repr ())
      with
	| YYexit _ as e ->
	  (*
	    print_endline "OK";
	  *)
	  raise e
	| e ->
	  (*
	    print_endline (Printexc.to_string e);
	    Printexc.print_backtrace stdout;
	    print_endline "backtrack";
	  *)
	  try backtrack ();
	  with No_backtracking -> raise e
    done;
    assert false
  with exn ->
    let curr_char = env.curr_char in
    env.asp <- init_asp;
    env.sp <- init_sp;
    env.stackbase <- init_stackbase;
    env.state <- init_state;
    env.curr_char <- init_curr_char;
    env.errflag <- init_errflag;
    match exn with
      | YYexit v -> Obj.magic v
      | _ ->
        current_lookahead_fun :=
          (fun tok ->
            if Obj.is_block tok
            then tables.transl_block.(Obj.tag tok) = curr_char
            else tables.transl_const.(Obj.magic tok) = curr_char);
        raise exn

let backtracking_parser tables start lexer chunks =
  backtracking_yyparse tables start
    (fun ~push ~pop ->
      let l, backtrack = backtracking_lexbuf ~push ~pop chunks in
      let initial_pos = l.lexbuf.lex_curr_p
      and lexe () =
	let t = lexer l.lexbuf in
	take_snapshot l;
	Util.debug "|%d-%d: %s| %!"
	  l.lexbuf.lex_start_pos l.lexbuf.lex_curr_pos (Lexing.lexeme l.lexbuf);
	t, l.lexbuf.lex_start_p, l.lexbuf.lex_curr_p
      in
      initial_pos, lexe, backtrack
    )

let implementation = backtracking_parser Owz_parser.yytables 1 Owz_lexer.token

(*
let implementation_with_errors = parse_with_errors Parser.implementation Lexer.token

let i = implementation_with_errors "test/test.ml" "test/test2.ml"
*)

(*
let _ = set_trace true


let _ = exit 0

let i = implementation (read_modified_file "test/test.ml" "test/test2.ml")
let _ = Printast.implementation Format.std_formatter i ; exit 0

*)
