(**************************************************************************)
(*                                                                        *)
(*  Ocamlwizard-Binannot                                                  *)
(*  Tiphaine Turpin                                                       *)
(*  Copyright 2011 INRIA Saclay - Ile-de-France                           *)
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

(*
let rec backup_name file n =
  let file' = file ^ "." ^  if Sys.file_exists
*)

let buf_len = 1024
let buf = String.create buf_len

let rec copy_until_end ic oc =
  let n = input ic buf 0 buf_len in
  output oc buf 0 n;
  if n > 0 then
    copy_until_end ic oc

let rec copy len ic oc =
  if len > 0 then
    let n = input ic buf 0 (min len buf_len) in
    output oc buf 0 n;
    if n = 0 then
      invalid_arg "Edit.copy: end of file reached"
    else
      copy (len - n) ic oc

let rec replace ic oc = function
  | [] -> copy_until_end ic oc
  | (b, e, s) :: l ->
    copy (b - pos_in ic) ic oc;
    output_string oc s;
    seek_in ic e;
    replace ic oc l

let cp file new_file =
  if Sys.file_exists new_file then
    invalid_arg "Edit.cp : target file exists"
  else
    let ic = open_in file in
    let oc = open_out new_file in
    copy_until_end ic oc;
    close_out oc;
    close_in ic

let edit eds file =
  let ic = open_in file in
  let new_file, oc = Filename.open_temp_file (Filename.basename file) ".edited" in
  replace ic oc eds;
  close_out oc;
  close_in ic;
  Sys.remove file;
  Sys.rename new_file file
