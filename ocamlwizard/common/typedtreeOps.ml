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

open Typedtree

type 'a sfun =
  [ `structure of Typedtree.structure | `signature of Typedtree.signature]
    -> 'a

type 'a funs = {
  structure : structure -> 'a;
  signature : signature -> 'a
}

(*
module type Process = sig
  type u
  val structure : structure -> u
  val signature : signature -> u
end
*)

module type FindArgument = sig
  type t
  module IteratorArgument :
    functor (Action : sig val found : t -> unit end) -> IteratorArgument
end

module MakeIterator
  (Arg : IteratorArgument) = struct

    include MakeIterator (Arg)

    let process = function
      | `structure s -> iter_structure s
      | `signature s -> iter_signature s

    let process' = {
      structure = iter_structure;
      signature = iter_signature
    }

end

module Find (T : FindArgument) = struct

  open T

  module First = struct
    exception Found of t
    include
    MakeIterator
      (IteratorArgument
	 (struct let found x = raise (Found x) end))
    let process s =
      try
	process s;
	raise Not_found
      with
	  Found x ->
 	    x
  end

  module All = struct
    let l = ref []

    include
    MakeIterator
      (IteratorArgument
	 (struct let found x = l := x :: !l end))
    let process s =
      l := []; (* Not proud of this *)
      process s;
      List.rev !l
  end

  let find = First.process
  let find_all = All.process

end

let find_pattern (type a) cond =
  let module M = Find (struct
    type t = a
    module IteratorArgument(Action : sig val found : t -> unit end) = struct
      include DefaultIteratorArgument
      let enter_pattern p =
	match cond p with
	  | Some p -> Action.found p
	  | None -> ()
    end
  end) in
  M.find

let find_expression (type a) cond =
  let module M = Find (struct
    type t = a
    module IteratorArgument(Action : sig val found : t -> unit end) = struct
      include DefaultIteratorArgument
      let enter_expression p =
	match cond p with
	  | Some p -> Action.found p
	  | None -> ()
    end
  end) in
  M.find

(*

module type FindArgument = sig
  type t
  val pattern : pattern -> t option
  val expression : expression -> t option
(*
  val structure : structure -> t option
  val value_description : value_description -> t option
  val type_declaration : type_declaration -> t option
  val exception_declaration :
    exception_declaration -> t option
  val package_type : package_type -> t option
  val signature : signature -> t option
  val signature_item : signature_item -> t option
  val modtype_declaration : modtype_declaration -> t option
  val module_type : module_type -> t option
  val module_expr : module_expr -> t option
  val with_constraint : with_constraint -> t option
  val class_expr : class_expr -> t option
  val class_signature : class_signature -> t option
  val class_description : class_description -> t option
  val class_type_declaration :
    class_type_declaration -> t option
  val class_infos : 'a class_infos -> t option
  val class_type : class_type -> t option
  val class_type_field : class_type_field -> t option
  val core_type : core_type -> t option
  val core_field_type : core_field_type -> t option
  val class_structure : class_structure -> t option
  val class_field : class_field -> t option
  val structure_item : structure_item -> t option
  val bindings : rec_flag -> t option      
  val binding : pattern -> expression -> t option
*)
 end

module DefaultFindArgument (T : sig type t end) = struct
  include T
  let pattern _ = None
  let expression _ = None
end

module FindGeneric
  (T : FindArgument)
  (Action : sig
    type result
    val found : T.t -> unit
    val find : ('a -> unit) -> 'a -> result
  end)
  = struct

    open T

  include
  Typedtree.MakeIterator (struct

    (* Should eventually be removed *)
    include Typedtree.DefaultIteratorArgument

    let enter node p =
      match node p with
	| Some x -> Action.found x
	| None -> ()

    let enter_pattern = enter pattern
    let enter_expression = enter expression

  end)

  let structure = Action.find iter_structure
  let signature = Action.find iter_signature

  let process = function
    | `structure s -> structure s
    | `signature s -> signature s

  let process' = {
    structure = structure;
    signature = signature
  }


end

module Find(T : FindArgument) = struct

  module Generic = FindGeneric (T)

  module First =
    Generic
      (struct

	type result = T.t
	exception Found of T.t

	let found x = raise (Found x)

	let find iter s =
	  try
	    iter s;
	    raise Not_found
	  with
	      Found x ->
 		x

       end)

  module All =
    Generic
      (struct

	type result = T.t list

	let l = ref []

	let found x = l := x :: !l

	let find iter s =
	  l := []; (* Not proud of this *)
	  iter s;
	  List.rev !l

       end)

  let find = First.process
  let find' = First.process'
  let find_all = All.process
  let find_all' = All.process'

end

let find_pattern (type a) cond =
  let module M = Find (struct
    include DefaultFindArgument (struct type t = a end)
    let pattern = cond
  end) in
  M.find

let find_expression (type a) cond =
  let module M = Find (struct
    include DefaultFindArgument (struct type t = a end)
    let expression = cond
  end) in
  M.find

*)

let contains loc (b', e') =
  let b, e = Util.get_c_num loc in
  b <= b' && e' <= e

(* This implementation is notably inefficient. *)
let locate_innermost s loc =
  let module M = Find (struct
    type t = [
      `pattern of pattern
    | `expression of expression
    | `structure_item of structure_item
    ]
    module IteratorArgument(Action : sig val found : t -> unit end) = struct
      include DefaultIteratorArgument
      open Action

      let leave_pattern p =
	if Util.get_c_num p.pat_loc = loc then
	  found (`pattern p)

      let leave_expression e =
	if contains e.exp_loc loc then
	  found (`expression e)

      let leave_structure_item i =
	if contains i.str_loc loc then
	  found (`structure_item i)

    end
  end) in
  M.find s

