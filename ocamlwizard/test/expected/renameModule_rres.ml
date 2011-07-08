module Y = struct

  let x = ()
  type t = A
  type t' = {a : unit}
  module M = struct end
  module type T = sig end

end

module N = Y
let y = Y.x
type t = Y.t
module O = Y.M
module type U = Y.T
let _ = match Y.A with Y.A -> ()
let _ = match {Y.a = ()} with {Y.a = ()} -> ()
Renamed 1 definition(s) and 9 reference(s)

