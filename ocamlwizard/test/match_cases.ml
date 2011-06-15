(* Tests for pattern matching completion (match_cases) (F3) *)


(* Basic types *)

let () = match () with $
let () = match true with $
let () = match 1 with $ (* Saut de ligne simple ? *)
let () = match 'a' with $
let () = match lazy () with $
let () = match ((), ()) with $


(* types of matching *)

let () = match () with $ (* we should remove the space *)
let () = match () with$ (* space not necessary *)
let () = match () $ (* we should add the with *)
let () = match ()$ (* space not necessary *)
let () = match true with true -> () $ (* Existing case *)
let () = match true with true -> ()$


(* (Polymorphic) variants types *)

let () = match [] with $
let () = match [] with [] -> () $
let () = match [||] with $
let () = match [||] with [||] -> () $ (* We still add another [||] *)
let () = match A with $
let () = match A with M.A -> () $
let () = match A with A -> () $
let () = match None with $
let () = match None with Some () -> () $
let () = match `a with $
let () = match `a with `b -> ()$

(* If the pattern matches a function type, we assume some arguments
   are missing and take the return type. *)
let () = match List.map2 with $



(* Record types *)

let () = match {a = () ; b = ()} with $

(* Not working yet *)
let (_ : unit -> 'a) = function $
let () = match true with true -> $
let () = match [] with [_] -> () $
let () = match None with Some [] -> () $ (* We should add Some (_ :: _) *)
let () = match `a with `b -> ()$
