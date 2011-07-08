module type $M€ = sig end

module type N = M

module M : M = struct end
