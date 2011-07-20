(* An occurrence of the old name would be captured if we rename it. *)
module M = struct
  let x = 1
  let y = ()
end

let _ = M.x
File "renameSigOldCaptured_rres.ml", line 4, characters 6-7:
This existing definition of y would capture an occurrence of x

