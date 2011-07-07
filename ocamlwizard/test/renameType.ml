type $x€ = [`foo]

type z = x
let _ = (`foo : x)
let _ = match `foo with #x -> ()
let _ = match `foo with (_ : x) -> ()
