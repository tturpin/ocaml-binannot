(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* System interface *)

external get_config: unit -> string * int = "sys_get_config"
external get_argv: unit -> string * string array = "sys_get_argv"

let (executable_name, argv) = get_argv()
let (os_type, word_size) = get_config()
let max_array_length = (1 lsl (word_size - 10)) - 1;;
let max_string_length = word_size / 8 * max_array_length - 1;;

external file_exists: string -> bool = "sys_file_exists"
external remove: string -> unit = "sys_remove"
external rename : string -> string -> unit = "sys_rename"
external getenv: string -> string = "sys_getenv"
external command: string -> int = "sys_system_command"
external time: unit -> float = "sys_time"
external chdir: string -> unit = "sys_chdir"
external getcwd: unit -> string = "sys_getcwd"

let interactive = ref false

type signal_behavior =
    Signal_default
  | Signal_ignore
  | Signal_handle of (int -> unit)

external signal: int -> signal_behavior -> signal_behavior
        = "install_signal_handler"

let set_signal sig_num sig_beh = ignore(signal sig_num sig_beh)

let sigabrt = -1
let sigalrm = -2
let sigfpe = -3
let sighup = -4
let sigill = -5
let sigint = -6
let sigkill = -7
let sigpipe = -8
let sigquit = -9
let sigsegv = -10
let sigterm = -11
let sigusr1 = -12
let sigusr2 = -13
let sigchld = -14
let sigcont = -15
let sigstop = -16
let sigtstp = -17
let sigttin = -18
let sigttou = -19
let sigvtalrm = -20
let sigprof = -21

exception Break

let catch_break on =
  if on then
    set_signal sigint (Signal_handle(fun _ -> raise Break))
  else
    set_signal sigint Signal_default


(* OCaml version string, moved from utils/config.mlp.
   Must be in the format described in sys.mli. *)

let ocaml_version = "3.04+10 (2002-04-18)"
