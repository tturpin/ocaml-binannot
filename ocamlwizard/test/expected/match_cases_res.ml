let () = match () with $
=>
let () = match () with 
  | ()$EOF

let () = match true with $
=>
let () = match true with 
  | false
  | true$EOF

let () = match 1 with $ (* Saut de ligne simple ? *)
=>
let () = match 1 with 
  | $_EOF

let () = match 'a' with $
=>
let () = match 'a' with 
  | $_EOF

let () = match lazy () with $
=>
let () = match lazy () with 
  | lazy $_EOF

let () = match ((), ()) with $
=>
let () = match ((), ()) with 
  | ($_, _)EOF

let () = match () with $
=>
let () = match () with 
  | ()$EOF

let () = match () with$ (* space not necessary *)
=>
let () = match () with
  | ()$EOF

let () = match () $
=>
let () = match ()  with
  | ()$EOF

let () = match ()$ (* space not necessary *)
=>
let () = match () with
  | ()$EOF

let () = match true with true -> () $ (* Existing case *)
=>
let () = match true with true -> () 
  | false$EOF

let () = match true with true -> ()$
=>
let () = match true with true -> ()
  | false$EOF

let () = match true with true$ (* Does it work or not ? *)
=>
let () = match true with true
  | false$EOF

let () = match [] with $
=>
let () = match [] with 
  | []
  | $_ :: _EOF

let () = match [] with [] -> () $
=>
let () = match [] with [] -> () 
  | $_ :: _EOF

let () = match [||] with $
=>
let () = match [||] with 
  | [||]$EOF

let () = match [||] with [||] -> () $ (* We still add another [||] *)
=>
let () = match [||] with [||] -> () 
  | [||]$EOF

let () = match A with $
=>
let () = match A with 
  | A
  | B$EOF

let () = match A with M.A -> () $
=>
let () = match A with M.A -> () 
  | B$EOF

let () = match A with A -> () $
=>
let () = match A with A -> () 
  | B$EOF

let () = match None with $
=>
let () = match None with 
  | None
  | Some $_EOF

let () = match None with Some () -> () $
=>
let () = match None with Some () -> () 
  | None$EOF

let () = match `a with $
=>
let () = match `a with 
  | `a$EOF

let () = match `a with `b -> ()$
=>
let () = match `a with `b -> ()Error: Error while typing
Raised at file "pervasives.ml", line 22, characters 22-33
Called from file "completion/completion.ml", line 62, characters 6-35
Called from file "completion/completion.ml", line 82, characters 26-48
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

let () = match List.map2 with $
=>
let () = match List.map2 with 
  | []
  | $_ :: _EOF

let () = match {a = () ; b = ()} with $
=>
let () = match {a = () ; b = ()} with 
  | { a = $_; b = _ }EOF

let (_ : unit -> 'a) = function $
=>
let (_ : unit -> 'a) = function Error: File "completion/completion.ml", line 128, characters 19-25: Assertion failed
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

let () = match () with | $
=>
let () = match () with | Error: File "completion/completion.ml", line 128, characters 19-25: Assertion failed
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

let () = match true with true -> $
=>
let () = match true with true -> Error: Not_found
Raised at file "common/typedtreeOps.ml", line 181, characters 10-19
Called from file "completion/completion.ml", line 98, characters 3-85
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

let () = match true with true -> () | $
=>
let () = match true with true -> () | Error: File "completion/completion.ml", line 128, characters 19-25: Assertion failed
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

let () = match [] with [_] -> () $ (* missing _ :: _ :: _ *)
=>
let () = match [] with [_] -> () 
  | []$EOF

let () = match None with Some [] -> () $ (* We should add Some (_ :: _) *)
=>
let () = match None with Some [] -> () 
  | None$EOF

let () = match `a with `b -> ()$ (* because it doesn't type... *)
=>
let () = match `a with `b -> ()Error: Error while typing
Raised at file "pervasives.ml", line 22, characters 22-33
Called from file "completion/completion.ml", line 62, characters 6-35
Called from file "completion/completion.ml", line 82, characters 26-48
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

