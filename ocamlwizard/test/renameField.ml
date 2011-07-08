type t = {mutable $a€ : int ; b : int }

let _ = match {a = 0 ; b = 0} with {a = 0} -> () | x -> x.a <- x.a
