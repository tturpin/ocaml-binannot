module M = struct
  type t = A | B
  type u = {a : unit ; b : unit}
  type v = X of unit * int list
  let sort = assert false
  module N = struct end
end

open M

let r = {a = () ; b = ()}
let some_value = 1
