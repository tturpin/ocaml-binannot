(* Pattern expansion (F11) *)

(* Warning, the position is the one of the last '->' ! *)

let () = match () with $_€ -> ()
let () = match () with $foo€ -> ()
let () = match [] with $_€ -> ()
let () = match [] with [] -> () | $_€ -> () (* We should filter [] *)

(* Some more contexts *)
let () = match {a = () ; b = ()} with {a = $_€ ; b = _} -> ()
let () = match {a = () ; b = ()} with {a = _ ; b = $_€} -> ()

(* Records *)
let () = match {a = () ; b = ()} with $_€ -> ()

let () = match ((), ()) with $_€ -> ()
let () = match lazy () with $_€ -> ()

(* Constructors *)
let () = match Some () with Some $_€ -> ()
let () = match X ((), []) with X $_€ -> ()
let () = match X ((), []) with X ($_€, _) -> ()
let () = match X ((), []) with X ((), $_€) -> ()
(* Does no work, but it is expected. *)
let () = match () with $fo€o -> ()
let () = match None with Some $_€ -> ()

(* We could add this *)
(*
let _ = ()
*)

(* Or patterns *)
let () = match List.hd [`a ; `b ; `c] with $_€ -> () (* why only `a ?*)
