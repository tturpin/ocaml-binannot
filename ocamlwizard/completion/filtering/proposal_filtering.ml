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
open Interface


(** *)
let main c_res ty_lis = 
  match c_res with
    | C_module (mi,pat)   -> C_module (Path_filter.filter_modules pat mi,pat)
	
    | C_value (vi,vk,pat) -> 
	C_value (Path_filter.filter_values (List.hd ty_lis) pat vi vk ,vk ,pat)

    | C_record (r_inf ,rk, pat) -> 
	C_record ( Path_filter.filter_records ty_lis pat r_inf rk, rk, pat) 

    | C_match (mi_lis, pm_comp) ->
	let inf_lis = (Match_filter.filter_match mi_lis pm_comp) in 
	C_match (MF inf_lis, pm_comp)

    | C_error _ -> assert false

    | C_other   -> assert false

	
