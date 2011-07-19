type t = {mutable y : int ; b : int }

let _ = match {y = 0 ; b = 0} with {y = 0} -> () | x -> x.y <- x.y
Renamed 1 definition(s) and 4 reference(s) in 1 file(s)

