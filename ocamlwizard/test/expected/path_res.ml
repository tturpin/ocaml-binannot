let _ = {a = () ; $
=>
let _ = {a = () ; M.b

EOF

let _ = {a = () ;$
=>
let _ = {a = () ;M.b

EOF

let _ = {a = () ; M.$
=>
let _ = {a = () ; M.M.b

EOF

let _ = {a = () ; M.b$
=>
let _ = {a = () ; M.bM.b

EOF

let _ = {r with a = () ;$
=>
let _ = {r with a = () ;M.b

EOF

let _ = {r with $
=>
let _ = {r with M.b
M.a

EOF

let _ = {r with$
=>
let _ = {r withM.b
M.a

EOF

let _ = {a = () ; b = ()}.$
=>
let _ = {a = () ; b = ()}.M.b
M.a

EOF

let _ = {a = () ; b = ()}.M$
=>
let _ = {a = () ; b = ()}.MM.b
M.a

EOF

let _ = {a = () ; b = ()}.M.$
=>
let _ = {a = () ; b = ()}.M.M.b
M.a

EOF

let _ = {a = () ; b = ()}.a$
=>
let _ = {a = () ; b = ()}.aM.a

EOF

let () = match r with {a = a ; $
=>
let () = match r with {a = a ; M.b

EOF

let () = match r with {a = a ;$
=>
let () = match r with {a = a ;M.b

EOF

let () = match {a = () ; b = ()} with {a = a ; M.$
=>
let () = match {a = () ; b = ()} with {a = a ; M.M.b

EOF

let () = match {a = () ; b = ()} with {a = a ; M.b$
=>
let () = match {a = () ; b = ()} with {a = a ; M.bM.b

EOF

let () = M.$
=>
let () = M.A 
B 
X 
sort 
EOF

let () = M.so$
=>
let () = M.sort EOF

let () = so$
=>
let () = some_value 
rt 
EOF

let () = so $ (* this does not work, purposely *)
=>
let () = so Error: Error while typing
Raised at file "pervasives.ml", line 22, characters 22-33
Called from file "completion/completion.ml", line 63, characters 6-35
Called from file "completion/completion.ml", line 83, characters 35-57
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

let () = comp$
=>
let () = compare EOF

let _ = {r$
=>
let _ = {r 
aise 
ead_float 
ead_int 
ead_line 
eally_input 
ef 
EOF

module N = Set.$
=>
module N = Set.
M
M.N
N
N.Make
EOF

let _ = {a = () $
=>
let _ = {a = () Error: File "completion/completion.ml", line 123, characters 17-23: Assertion failed
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

let _ = {a = () ; M$
=>
let _ = {a = () ; MError: File "completion/completion.ml", line 124, characters 19-25: Assertion failed
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

let _ = {r $
=>
let _ = {r Error: File "completion/completion.ml", line 123, characters 17-23: Assertion failed
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

