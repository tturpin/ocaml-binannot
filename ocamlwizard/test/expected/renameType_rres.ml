type y = [`foo]

type z = y
let _ = (`foo : y)
let _ = match `foo with #y -> ()
let _ = match `foo with (_ : y) -> ()
Renamed 1 definition(s) and 4 reference(s) in 1 file(s)

