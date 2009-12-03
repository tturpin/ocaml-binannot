let property =
  let new type t in
  fun () ->
    let module M = struct exception E of t end in
    (fun x -> M.E x), (function M.E x -> Some x | _ -> None)

let () =
  let (int_inj, int_proj) = property () in
  let (string_inj, string_proj) = property () in

  let i = int_inj 3 in
  let s = string_inj "abc" in

  Printf.printf "%b\n%!" (int_proj i = None);
  Printf.printf "%b\n%!" (int_proj s = None);
  Printf.printf "%b\n%!" (string_proj i = None);
  Printf.printf "%b\n%!" (string_proj s = None)




let sort_uniq =
  let new type s in
  fun cmp l ->
    let module S = Set.Make(struct type t = s let compare = cmp end) in
    S.elements (List.fold_right S.add l S.empty)

let () =
  print_endline (String.concat "," (sort_uniq compare [ "abc"; "xyz"; "abc" ]))

