module M = struct
  let $x€ = 1
end

module N = (M : sig val x : int end)

let _ = N.x
