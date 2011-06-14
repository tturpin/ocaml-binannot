module M = struct
  type t = A | B
  type u = {a : unit ; b : unit}
  let sort = assert false
  module N = struct end
end

open M

let () = match () with _ -> assert false
let () = match 1 with _ -> assert false (* Saut de ligne simple ? *)
let () = match true with _ -> assert false
let () = match [] with _ -> assert false
let () = match [||] with _ -> assert false
let () = match lazy () with _ -> assert false
let () = match 'a' with _ -> assert false (* Saut de ligne simple ? *)
let () = match List.map2 with _ -> assert false
let () = match ((), ()) with _ -> assert false
let () = match `a with _ -> assert false
let () = match A with _ -> assert false
let () = match None with _ -> assert false
let () = match {a = () ; b = ()} with _ -> assert false

let () = match [] with
  | [] -> assert false

let () = match A with M.A -> assert false


let () = match Some () with
   None -> assert false
 | Some   () -> assert false

(*
let () = M.
let () = M.so

let _ = {a = () ; b = ()}.
let _ = {a = () ; b = ()}.M
let _ = {a = () ; b = ()}.M.
let _ = {a = () ; b = ()}.a
*)

(* does not work:
let _ = {a = () 
let _ = {a = () ; M
*)

module N = List.

let r = {a = () ; b = ()}

let _ = {r with a = () ; M.b
let _ = {r with


let _ = {a = () ; 
let _ = {a = () ; M.
let _ = {a = () ; M.b

(* does not work:
let () = match {a = () ; b = ()} with {a = a 
let () = match {a = () ; b = ()} with {a = a ; M
*)

let () = match {a = () ; b = ()} with {a = a ; 
let () = match {a = () ; b = ()} with {a = a ; M.
let () = match {a = () ; b = ()} with {a = a ; M.b

let _ = ()
