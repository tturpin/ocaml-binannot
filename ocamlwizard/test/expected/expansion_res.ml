let () = match () with $_€ -> ()
=>
let () = match () with ()$EOF

let () = match () with $foo€ -> ()
=>
let () = match () with ()$EOF

let () = match [] with $_€ -> ()
=>
let () = match [] with [] | $_ :: _EOF

let () = match [] with [] -> () | $_€ -> () (* We should filter [] *)
=>
let () = match [] with [] -> () | [] | $_ :: _EOF

let () = match {a = () ; b = ()} with {a = $_€ ; b = _} -> ()
=>
let () = match {a = () ; b = ()} with {a = ()$EOF

let () = match {a = () ; b = ()} with {a = _ ; b = $_€} -> ()
=>
let () = match {a = () ; b = ()} with {a = _ ; b = ()$EOF

let () = match {a = () ; b = ()} with $_€ -> ()
=>
let () = match {a = () ; b = ()} with { a = $_; b = _ }EOF

let () = match ((), ()) with $_€ -> ()
=>
let () = match ((), ()) with ($_, _)EOF

let () = match lazy () with $_€ -> ()
=>
let () = match lazy () with lazy $_EOF

let () = match () with $fo€o -> ()
=>
let () = match () with EOF

let () = match None with Some $_€ -> ()
=>
let () = match None with Some EOF

let $_€ = ()
=>
let EOF

