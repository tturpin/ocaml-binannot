module M = struct
  let y = 1
end

module F(X : sig val x : int end) = struct let z = X.x end

module N = F(struct let x = 2 let tot = () end)

let _ = M.y
Renamed 1 definition(s) and 1 reference(s)

