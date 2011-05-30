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

open Interface
open Format

(** *)
exception Fail of string list

(** *)
let mk_fail file nb = 
  Format.sprintf "%s-%d : This case should not happen" file nb
    
(** *)
let unreachable file nb =
  raise (Fail [(mk_fail file nb)]) 
    
(** *)
let fail file nb f_lis = 
  raise (Fail ((mk_fail file nb)::f_lis))

let print_value_kd  = function
  | V_all    -> eprintf "of ( V_all )"
  | V_record  -> eprintf "of ( V_record )"
  
let print_rec_kd = function
  | Fdummy    -> eprintf "of ( Fdummy )"
  | Faccess v -> eprintf "of ( Faccess )"; print_value_kd v
  | Fdef _    -> eprintf "of ( Fdef )"
  | Fpat _    -> eprintf "of ( Fpat )"

let print_path_comp = function
  | Module     -> eprintf "of ( Module ) "
  | Record rf  -> eprintf "of ( Record " ; print_rec_kd rf;eprintf " )"
  | Value v     -> eprintf "of ( Value ) "; print_value_kd v;eprintf " )"
      
let print_pm_comp = function
  | AllCs ->    eprintf "of AllCs"
  | MissCs _ ->   eprintf "of MissCs"
  | BranchCs _ -> eprintf "of BranchCs" 

let print_c_sort c_srt =
  eprintf "> Completion sort : [";
  begin
    match c_srt with
      | Match mc -> eprintf "Match " ; print_pm_comp mc;
      | Try tc   -> eprintf "Try "   ; print_pm_comp tc;
      | Path pc  -> eprintf "Path "  ; print_path_comp pc.p_kd;
      | Other    -> eprintf "Other "
      | Error _  -> eprintf "Error "
  end;	  
  eprintf "]@."
  
let exit_with_code dot_test = function
  | Match _   ->  exit 10
  | Try   _   ->  exit 11
  | Other     ->  exit 20 
  | Path p    -> 
      if dot_test then exit 17 else (*For dot test*)
      begin
	match p.p_kd with
	  | Module   -> exit 13
	  | Record _ -> exit 14
	  | Value _  -> exit 16
      end
  | Error _   -> exit 2

    
  
