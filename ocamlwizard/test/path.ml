(* Path completion (F12) *)

(* Qualification needs to be fixed *)


(* Record fields *)

(* In a definition *)
let _ = {a = () ; $
let _ = {a = () ;$
let _ = {a = () ; M.$
let _ = {a = () ; M.b$

(* In a redefinition *)
let _ = {r with a = () ;$
let _ = {r with $
let _ = {r with$

(* In a read/write access *)
let _ = {a = () ; b = ()}.$
let _ = {a = () ; b = ()}.M$
let _ = {a = () ; b = ()}.M.$
let _ = {a = () ; b = ()}.a$

(* In a pattern *)
let () = match r with {a = a ; $
let () = match r with {a = a ;$
let () = match {a = () ; b = ()} with {a = a ; M.$
let () = match {a = () ; b = ()} with {a = a ; M.b$


(* Values *)

let () = M.$
let () = M.so$
let () = so$

(* Modules *)

module N = Set.$

(* does not work: *)
let _ = {a = () $
let _ = {a = () ; M$
let _ = {r $
