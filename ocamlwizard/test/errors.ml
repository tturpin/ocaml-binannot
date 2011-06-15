module M = struct
  type t = A | B "unterminated string
  type u = {a : unit ; b : unit}
  let sort = assert false
  module N = struct let (* unfinished value *) end
end
(* unterminated comment
open M
let () = ()

let r = {a = () ; b = ()}

let () = match [] with$
