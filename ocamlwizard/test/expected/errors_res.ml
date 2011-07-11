module M = struct
  type t = A | B "unterminated string
  type u = {a : unit ; b : unit}
  let sort = assert false
  module N = struct let (* unfinished value *) end
end
(* unterminated comment
open M
let () = ()

let r = {a = () ; b = ()}

let () = match [] with$
=>
let () = match [] withError: File "completion/completion.ml", line 124, characters 19-25: Assertion failed
Called from file "main/ocamlwizard.ml", line 90, characters 14-32
Called from file "main/ocamlwizard.ml", line 61, characters 6-10
EOF

