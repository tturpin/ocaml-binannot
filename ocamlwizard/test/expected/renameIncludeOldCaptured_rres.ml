(* An occurrence of the old name would be captured if we rename it. *)
module M = struct
  let x = 1
  let y = ()
end

include M

let _ = x
File "renameIncludeOldCaptured_rres.ml", line 3, characters 6-7:
This existing definition of y would capture an occurrence of x

