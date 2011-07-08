module type F = functor ($M€ : sig

  val x : unit
  type t = A
  type t' = {a : unit}
  module M : sig end
  module type T = sig end

end) -> sig

  type t = M.t
  module type U = M.T

end
