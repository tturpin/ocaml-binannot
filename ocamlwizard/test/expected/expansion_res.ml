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
let () = match () with Error: Not_found
Raised at file "common/typedtreeOps.ml", line 181, characters 10-19
Called from file "completion/completion.ml", line 120, characters 3-85
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

let () = match None with Some $_€ -> ()
=>
let () = match None with Some Error: var
Raised at file "pervasives.ml", line 22, characters 22-33
Called from file "completion/extraction/proposal_extraction.ml", line 281, characters 11-25
Called from file "completion/completion.ml", line 161, characters 14-56
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

let () = match List.hd [`a ; `b ; `c] with $_€ -> () (* why only `a ?*)
=>
let () = match List.hd [`a ; `b ; `c] with `a$EOF

