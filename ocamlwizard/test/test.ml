module M = struct
  type t = A | B
  type u = {a : unit ; b : unit}
end

open M

let () = match M.A with 
