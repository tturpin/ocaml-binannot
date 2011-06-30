(* An existing occurrence of the new name would be captured. *)
module M = struct
  let y = ()
  let $x€ = 1
end

let _ = M.y
