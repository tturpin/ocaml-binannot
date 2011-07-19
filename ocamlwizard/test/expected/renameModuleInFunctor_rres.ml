module F(Y : sig

  val x : unit
  type t = A
  type t' = {a : unit}
  module M : sig end
  module type T = sig end

end) = struct

  type t = Y.t
  module type U = Y.T

end
Renamed 1 definition(s) and 2 reference(s) in 1 file(s)

