module M = struct
let toto = ()
let $x€ = 1
end

let _ = M.x
(*
module F(X : sig val x : unit end) = struct let z = X.x end

module N = F(struct let x = () let tot = () end)
module O = F(M)

module G(X : sig module type T module X : T end) = struct include X end
*)


module N = (M : sig val toto : unit end)
(*
open M
*)
let () = N.toto
(*
open M
let z = x
*)
