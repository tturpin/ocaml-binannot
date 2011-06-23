open Typedtree

type 'a process = {
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
  val pattern : pattern -> t option
  val expression : expression -> t option
end

module DefaultFindArgument (T : sig type t end) = struct
  include T
  let pattern _ = None
  let expression _ = None
end

module Find (T : FindArgument) = struct

  open T
  exception Found of T.t

  include
  Typedtree.MakeIterator (struct

    (* Should eventually be removed *)
    include Typedtree.DefaultIteratorArgument

    let enter node p =
      match node p with
	| Some x -> raise (Found x)
	| None -> ()

    let enter_pattern = enter pattern
    let enter_expression = enter expression

  end)

  let find iter s =
    try
      iter s;
      raise Not_found
    with
	Found x ->
 	  x

  let structure = find iter_structure
  let signature = find iter_signature

  let process = {
    structure = structure;
    signature = signature
  }

end


let find_pattern (type a) cond =
  let module M = Find (struct
    include DefaultFindArgument (struct type t = a end)
    let pattern = cond
  end) in
  M.process

let find_expression (type a) cond =
  let module M = Find (struct
    include DefaultFindArgument (struct type t = a end)
    let expression = cond
  end) in
  M.process

