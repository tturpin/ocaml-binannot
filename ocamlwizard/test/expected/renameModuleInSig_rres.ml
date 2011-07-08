module type M = sig
  module Y : sig

    val x : unit
    type t = A
    type t' = {a : unit}
    module M : sig end
    module type T = sig end

  end

  type t = Y.t
  module type U = Y.T

end
Renamed 1 definition(s) and 2 reference(s)

