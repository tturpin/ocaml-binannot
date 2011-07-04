module M = struct
  let y = 1
end

module N = (M : sig val y : int end)

let _ = N.y
