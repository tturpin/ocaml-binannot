(* An existing occurrence of the new name would be captured. *)
module M = struct
  let y = ()
  let x = 1
end

include M

let _ = y
File "renameIncludeNewCaptured_rres.ml", line 4, characters 6-7:
This definition of x that you are trying to rename would capture an occurrence of an existing definition of y

