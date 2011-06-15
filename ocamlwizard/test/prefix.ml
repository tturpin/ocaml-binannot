module M = struct
  type t = A | B
  type u = {a : unit ; b : unit}
  let sort = assert false
  module N = struct end
end

open M

let r = {a = () ; b = ()}
