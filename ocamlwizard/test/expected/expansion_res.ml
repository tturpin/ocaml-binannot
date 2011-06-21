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

let () = match Some () with Some $_€ -> ()
=>
let () = match Some () with Some ()$EOF

let () = match X ((), []) with X $_€ -> ()
=>
let () = match X ((), []) with X ($_, _)EOF

let () = match X ((), []) with X ($_€, _) -> ()
=>
let () = match X ((), []) with X (()$EOF

let () = match X ((), []) with X ((), $_€) -> ()
=>
let () = match X ((), []) with X ((), [] | $_ :: _EOF

let () = match () with $fo€o -> ()
=>
let () = match () with EOF

let () = match None with Some $_€ -> ()
=>
let () = match None with Some EOF

let () = match List.hd [`a ; `b ; `c] with $_€ -> () (* why only `a ?*)
=>
let () = match List.hd [`a ; `b ; `c] with `a$EOF

