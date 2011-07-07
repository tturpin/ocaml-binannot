(* An existing occurrence of the new name would be captured. *)
module M = struct
  let y = ()
  let x = 1
end

let _ = M.y
File "renameSigNewCaptured_no_dollar.ml", line 4, characters 6-7:
This definition of x that you are trying to rename would capture an occurrence of an existing definition of y

