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

(** *)
open Common_config
open Interface
open Parsing_env
open Util

(**
   Check the given file on command line. Return code 1 with all error.
   Change this for error in xml format.
*)
let check_validity () =
  if !fic_source = "" then begin
      Format.eprintf "\n%s\n@?" usage; 
    exit 1;
  end;
  if not (Filename.check_suffix !fic_source ".ml") then begin
    Format.eprintf "\n%s\n@?" usage;
    Arg.usage options usage;
    exit 1; 
  end;
  let cut_pos = 
    try 
      int_of_string (!pos) 
    with _ -> begin
	Format.eprintf " Error : The (-pos) must be a number\n@?";
	exit 1;
      end;
  in 
  cut_pos

(** Return info of Completion in record *)
let mk_info rg = 
  {
    fb_name   = check_auto_save !fic_source;
    f_path    = Filename.dirname (!fic_source);
    includ    = search_dirs !fic_source;
    c_rg      = rg;
    c_kind    = !kind;
    c_parser  = !_parser;
    c_printer = !printer;
  }
   
let catch_owz f =
  try f ()
  with
    | OwzFailure s ->
	print_string s;
	exit 1
    | Failure s ->
	Printf.printf "Error: %s\n" s;
	Printexc.print_backtrace stdout;
	exit 2
    | e ->
	Printf.printf "Error: %s\n" (Printexc.to_string e);
	Printexc.print_backtrace stdout;
	exit 2

(** Main for external completion calls *)
let main () = 
  Arg.parse options anonymous usage;
  match !command with 

    | Nothing ->
	()

    | Completion ->
	catch_owz (function () ->
	List.iter (fun s -> 
	  Clflags.include_dirs := s :: !Clflags.include_dirs)
	  (List.rev (i_dirs ()));
	let rg = {b=(-1);e=check_validity ()} in
	let ci = mk_info rg in
	let c_info = Completion.main ci in
	c_info
)
    | Refactor r ->
      (match r with
	| Rename (loc, name', file) ->
	    catch_owz (function () -> Rename.rename loc name' file ; exit 0)
	| Depend | Qualif -> failwith "not yet")

    | Locate -> failwith "not yet"

(*
    | Locate -> 
	begin
	  List.iter (fun s -> 
	    Clflags.include_dirs := s :: !Clflags.include_dirs)
	    (List.rev (i_dirs ()));
	  try Main.main [| Sys.argv.(0); !fic_source |]
	  with Exit_typing -> exit 0
	end

*)	  

let _ = main ();  Format.eprintf "@.";
