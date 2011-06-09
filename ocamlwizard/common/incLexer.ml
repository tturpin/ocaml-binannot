open Lexing
open Diff

let copy_lexbuf lexbuf = {
  lexbuf with
    lex_buffer = String.copy lexbuf.lex_buffer; (* Will be needed later*)
    lex_mem = Array.copy lexbuf.lex_mem
}

let copy_lexbuf_to l l' =
  l'.lex_buffer <- l.lex_buffer;
  l'.lex_buffer_len <- l.lex_buffer_len;
  l'.lex_abs_pos <- l.lex_abs_pos;
  l'.lex_start_pos <- l.lex_start_pos;
  l'.lex_curr_pos <- l.lex_curr_pos;
  l'.lex_last_pos <- l.lex_last_pos;
  l'.lex_last_action <- l.lex_last_action;
  l'.lex_eof_reached <- l.lex_eof_reached;
  l'.lex_mem <- l.lex_mem;
  l'.lex_start_p <- l.lex_start_p;
  l'.lex_curr_p <- l.lex_curr_p

let zero_pos = {
  pos_fname = "";
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 0;
}

type backtracking_lexbuf = {
  lexbuf : lexbuf;
  mutable snapshot : lexbuf; (* Snapshot of the lexbuff after
				the last successfully read token *)
  mutable next_shift : int; (* Number of characters to shift the position
			       when entering the next chunk. *)
  mutable chunks : chunk list; (* What remains to read *)
  stack : (lexbuf * string * int * chunk list) Stack.t
(* The backtracking stack. We save the last lexbuf snapshot, the old
   version of the chunk to recover, the length of the new version (the
   difference between the two will be used to shift the next chunk),
   and the remaining chunks. *)
}

(* Call this between each successfull call to the lexer. *)
let take_snapshot l = l.snapshot <- copy_lexbuf l.lexbuf

(* A backtracking buffer is a non-empty list of the above frames. *)

exception No_backtracking

(* Restore the last saved state of a backtracking lexer. Do not call
   directly ! *)
let backtrack l =
  try
    let lexbuf, old, new_len, chunks = Stack.pop l.stack in
    copy_lexbuf_to lexbuf l.lexbuf;
(*
    print_endline "BACKTRACK";
    Printf.printf "REFILL OLD %s END\n%!" old;
*)
    l.lexbuf.lex_buffer <- l.lexbuf.lex_buffer ^ old;
    l.lexbuf.lex_buffer_len <- String.length l.lexbuf.lex_buffer;
    take_snapshot l;
    l.chunks <- chunks;
    l.next_shift <- new_len - String.length old
  with
      Stack.Empty -> raise No_backtracking

(* Refill a backtracking lexbuf and pushes a backtracking point if
   entering a changed section. *)
let refill_buff push l =
  let lexbuf = l.lexbuf in
  match l.chunks with
    | [] ->
(*
      Printf.printf "REFILL EOF REACHED\n%!";
*)
      lexbuf.lex_eof_reached <- true
    | Same s :: q ->
(*
      Printf.printf "REFILL SAME %s END\n%!" s;
*)
      lexbuf.lex_buffer <- lexbuf.lex_buffer ^ s;
      lexbuf.lex_buffer_len <- String.length lexbuf.lex_buffer;
(*
      lexbuf.lex_curr_p <-
	{ lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_pos + l.next_shift };
*)
      lexbuf.lex_abs_pos <- lexbuf.lex_abs_pos + l.next_shift;
      l.next_shift <- 0;
      l.chunks <- q;
    (* Keep positions consistent... *)
    | Changed (old, last) :: q ->
(*
      Printf.printf "REFILL NEW %s END\n%!" last;
      Printf.printf "PUSH with start, current, last = %d, %d, %d\n%!"
	lexbuf.lex_start_pos
	lexbuf.lex_curr_pos
	lexbuf.lex_last_pos;
      Array.iter (function i -> Printf.printf "%d;" i) lexbuf.lex_mem;
*)
      Stack.push
	({ l.snapshot with
	  lex_buffer = lexbuf.lex_buffer;
	  lex_buffer_len = lexbuf.lex_buffer_len
	}, old, String.length last, q)
	l.stack;
      lexbuf.lex_buffer <- lexbuf.lex_buffer ^ last;
      lexbuf.lex_buffer_len <- String.length lexbuf.lex_buffer;
      l.chunks <- q;
      push ()

(* Premiere version : on ne vide jamais le debut du buffer et on
   s'arrange pour que la taille soit exactement egale a lex_buffer_len *)

(* A backtracking lexbuf for reading a modified file. The push
   function allows the user to save an external state in addition to
   the lexbuf itself (mutable values modified by the lexer, ...) A
   backtracking function is provided, which reverts to a previous
   state and call the provided pop function to restore the context. *)
let backtracking_lexbuf ~push ~pop chunks =
  (* This let rec is not strict OCaml *)
  let rec lexbuf = {
    refill_buff =
      (function lexbuf ->
	assert (lexbuf == l.lexbuf);
	refill_buff push l);
    lex_buffer = "";
    lex_buffer_len = 0;
    lex_abs_pos = 0;
    lex_start_pos = 0;
    lex_curr_pos = 0;
    lex_last_pos = 0;
    lex_last_action = 0;
    lex_mem = [||];
    lex_eof_reached = false;
    lex_start_p = zero_pos;
    lex_curr_p = zero_pos;
  }
  and l = {
    lexbuf = lexbuf;
    snapshot = lexbuf;
    next_shift = 0;
    chunks = chunks;
    stack = Stack.create ()
  } in
  take_snapshot l;
  l,
  (function () -> backtrack l ; pop ())

(* For testing *)
let lexe_with_errors lexer old_file new_file =
  let modified = read_modified_file old_file new_file in
  let stack = Stack.create () in
  let push () =
    Stack.push
      Owz_lexer.(
	!string_buff, !string_index, !string_start_loc, !comment_start_loc
      )
      stack
  and pop () =
    let buff, index, string_start, comment_start = Stack.pop stack in
    Owz_lexer.(
      string_buff := buff;
      string_index := index;
      string_start_loc := string_start;
      comment_start_loc := comment_start
    )
  in
  let lexbuf, backtrack = backtracking_lexbuf ~push ~pop modified in
  while true do
    try
      let lexeme = lexer lexbuf.lexbuf in
      take_snapshot lexbuf;
      if lexeme = Owz_parser.EOF then failwith "EOF";
(*
      Printf.printf "|%s| %!" (Lexing.lexeme lexbuf.lexbuf)
*)
    with
      | Failure "EOF" as e -> raise e
      | e ->
(*
	(match e with
	  | Owz_lexer.Error (e, _) ->
	    Format.printf "%a\n%!" Owz_lexer.report_error e
	  | _ ->
	    print_endline (Printexc.to_string e);
	    Printexc.print_backtrace stdout);
*)
	backtrack ()
  done

type ('a, 'b) lazy_list =
  | Nil
  | Cons of (('a -> ('a, 'b) lazy_list -> 'b) -> 'b)

let rec fold_left k f acc = function
  | Nil -> k acc
  | Cons k' -> k' (fun t q -> fold_left k f (f acc t) q)

let to_list k l =
  fold_left (function l -> k (List.rev l)) (fun q t -> t :: q) [] l

(* These two functions should be independent from OWZ *)
let rec lexe_with_errors lexer backtrack lexbuf =
  try
    let lexeme = lexer lexbuf.lexbuf in
    take_snapshot lexbuf;
    if lexeme = Owz_parser.EOF then
      Nil
    else (
      let t = Lexing.lexeme lexbuf.lexbuf in
      Printf.printf "|%s| %!" t;
      Cons (function k -> k (t, lexeme) (lexe_with_errors lexer backtrack lexbuf))
    )
  with
      e ->
	backtrack ();
	lexe_with_errors lexer backtrack lexbuf

let lexe_with_errors lexer old_file new_file =
  let modified = read_modified_file old_file new_file in
  let stack = Stack.create () in
  let push () =
    Stack.push
      Owz_lexer.(
	!string_buff, !string_index, !string_start_loc, !comment_start_loc
      )
      stack
  and pop () =
    let buff, index, string_start, comment_start = Stack.pop stack in
    Owz_lexer.(
      string_buff := buff;
      string_index := index;
      string_start_loc := string_start;
      comment_start_loc := comment_start
    )
  in
  let lexbuf, backtrack = backtracking_lexbuf ~push ~pop modified in
  lexe_with_errors lexer backtrack lexbuf

let lexe_ocaml_with_errors lexer old_file new_file =
  let l = lexe_with_errors lexer old_file new_file in
  to_list
    (function l ->
      Printf.printf "first correct list of token is:\n";
      List.iter (function t, _ -> Printf.printf "%s " t) l)
    l


(*
let _ = lexe_with_errors' Owz_lexer.token "test/test.ml" "test/test2.ml"
let _ = exit 0
*)
