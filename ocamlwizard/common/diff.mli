(** Reading modified text files. *)

(** This module calls the diff -f command. *)

(** A chunk of a modified file is either an unmodified portion or
    modified one, which has an old and a new versions. Chunks may
    contain newline characters. *)
type chunk =
  | Same of string
  | Changed of string * string

(** Read a modified file as a list of chunks, given the old and the
    new versions of the file. *)
val read_modified_file : string -> string -> chunk list
