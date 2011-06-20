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
let () = M.sort EOF

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
let () = so EOF

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
let _ = {a = () EOF

let _ = {a = () ; M$
=>
let _ = {a = () ; MEOF

let _ = {r $
=>
let _ = {r EOF

