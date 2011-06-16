(* Tests for pattern matching completion (match_cases) (F3) *)


(* Basic types *)

let () = match () with $
let () = match true with $
let () = match 1 with $ (* Saut de ligne simple ? *)
let () = match 'a' with $
let () = match lazy () with $
let () = match ((), ()) with $


(* Types of matching *)

let () = match () with $
let () = match () with$ (* space not necessary *)
let () = match () $
let () = match ()$ (* space not necessary *)
let () = match true with true -> () $ (* Existing case *)
let () = match true with true -> ()$
let () = match true with true$ (* Does it work or not ? *)


(* (Polymorphic) variant types *)

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
let () = match () with | $
let () = match true with true -> $
let () = match true with true -> () | $
let () = match [] with [_] -> () $ (* missing _ :: _ :: _ *)
let () = match None with Some [] -> () $ (* We should add Some (_ :: _) *)
let () = match `a with `b -> ()$ (* because it doesn't type... *)
(* try... *)
