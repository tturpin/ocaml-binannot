module M = struct
  let y = 1
end

module F(X : sig val y : int end) = struct let z = X.y end

module N = F(struct let y = 2 let tot = () end)

module O = F(M)

let _ = M.y
Renamed 3 definition(s) and 2 reference(s)

