(** Lexing with backtracking. *)

(** Warning ! currently, the computed locations are wrong : they
    reffer to the ideal file, which may not correspond to any of the
    old and new versions. *)

(** The backtracking points may only be saved between two tokens
    because ocamllex lexers are recursive, and there is no way to
    access the current call stack. This should only be a concern for
    long string litterals and comments however. *)

(** The type of backtracking lexing buffers, wchich store a stack of
    states for recovering. The type is only visible to allow reading
    the current lexbuf. *)
type backtracking_lexbuf = private {
  lexbuf : Lexing.lexbuf;
  mutable snapshot : Lexing.lexbuf;
  mutable chunks : Diff.chunk list;
  stack : (Lexing.lexbuf * string * Diff.chunk list) Stack.t;
}

(** Raised when the backtracking stack is empty. *)
exception No_backtracking

(** Create a backtracking lexing buffer for reading a modified
    file. The push function is called when entering a modified part
    and allows the user to save an external state in addition to the
    lexbuf itself (mutable values modified by the lexer, ...) A
    backtracking function is provided, which reverts to a previous
    state and call the provided pop function to restore the
    context. *)
val backtracking_lexbuf :
  push:(unit -> unit) -> pop:(unit -> 'a) -> Diff.chunk list ->
  backtracking_lexbuf * (unit -> 'a)


(** Take a snapshot of the current lexing state. The latest snapshot
    is pushed when entering a modified part, and poped when
    backtracking. Therefore this function should be called between
    each successfull call to the lexer. *)
val take_snapshot : backtracking_lexbuf -> unit

(** Higher-level interface to lexing with backtracking. *)

type ('a, 'b) lazy_list =
  | Nil
  | Cons of (('a -> ('a, 'b) lazy_list -> 'b) -> 'b)

val lexe_with_errors :
  (Lexing.lexbuf -> Owz_parser.token) -> string -> string ->
  (string * Owz_parser.token, 'a) lazy_list
