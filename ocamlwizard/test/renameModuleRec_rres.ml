module rec M : sig val x : unit end = struct let x = () end
and N : sig end = struct let y = M.x end
Error: different environments
Raised at file "pervasives.ml", line 22, characters 22-33
Called from file "refactor/findName.ml", line 196, characters 24-57
Called from file "refactor/findName.ml", line 217, characters 21-33
Called from file "hashtbl.ml", line 157, characters 23-35
Called from file "hashtbl.ml", line 161, characters 12-33
Called from file "refactor/findName.ml", line 238, characters 27-62
Called from file "refactor/rename.ml", line 307, characters 6-79
Called from file "main/ocamlwizard.ml", line 96, characters 31-59
Called from file "main/ocamlwizard.ml", line 61, characters 6-10


