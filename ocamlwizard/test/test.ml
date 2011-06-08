module M = struct
  type t = A | B
  type u = {a : unit ; b : unit}
end

open M

let () = match {a = () ; b = ()} with _ -> assert false

let _ = ()
