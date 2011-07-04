(* Does not work because modtypes don't have environments. *)
module type $M€ = sig end

module type N = M

module M : M = struct end
