(* does not work *)
module rec $M€ : sig val x : unit end = struct let x = () end
and N : sig end = struct let y = M.x end
