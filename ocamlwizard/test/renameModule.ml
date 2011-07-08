module $M€ = struct

  let x = ()
  type t = A
  type t' = {a : unit}
  module M = struct end
  module type T = sig end

end

module N = M
let y = M.x
type t = M.t
module O = M.M
module type U = M.T
let _ = match M.A with M.A -> ()
let _ = match {M.a = ()} with {M.a = ()} -> ()
