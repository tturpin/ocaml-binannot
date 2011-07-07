module M = struct
  let y = 1
end

module N = (M : sig val y : int end)

let _ = N.y
Renamed 2 definition(s) and 1 reference(s)

