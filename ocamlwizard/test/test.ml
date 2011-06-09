module M = struct
  type t = A | B
  type u = {a : unit ; b : unit}
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
 | Some   () 
  -> assert false


let _ = ()
