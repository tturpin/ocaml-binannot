(* The type of editing actions output by diff -f. *)
type action =
  | D of int * int option (* Delete lines b -- e *)
  | C of int * int option * string list (* Change lines b-e by r *)
  | A of int * string list (* Add r after line b *)
(* Lines start at 1 when interpreting diff files. *)

let rec read_until_dot f =
  match input_line f with
    | "." -> []
    | s -> (s ^ "\n") :: read_until_dot f

(* Parse a single action in the output of diff -f. *)
let parse_instr f =
  let l = input_line f in
  let c = l.[0]
  and l = String.sub l 1 (String.length l - 1) in
  let b, e =
    try
      let i = String.index l ' ' in
	int_of_string (String.sub l 0 i),
        Some (int_of_string (String.sub l (i + 1) (String.length l - i - 1)))
    with
	Not_found -> int_of_string l, None
  in
    match c with
      | 'd' -> D (b, e)
      | 'c' -> C (b, e, read_until_dot f)
      | 'a' -> A (b, read_until_dot f)
      | _ -> invalid_arg "parse_instr"

let simplify = function
  | D (b, e) -> b, (match e with Some e -> e | None -> b), []
  | C (b, e, r) -> b, (match e with Some e -> e | None -> b), r
  | A (b, r) -> b + 1, b, r

let rec parse_diff f =
  try
    let i = parse_instr f in
    simplify i :: parse_diff f
  with
      End_of_file -> []

(* Parse a file generated with diff -f and return a list of changes of the form
   (b, e, r) with meaning replace [b, e - 1[ by r. *)
let parse_diff_file f =
  let c = open_in f in
  let d = parse_diff c in
    close_in c;
  d

let rec parse_lines f =
  try
    let i = input_line f in
      (i ^ "\n") :: parse_lines f
  with
      End_of_file -> []

(* Get the lines of a text file (with end of lines). *)
let lines_of f =
  let c = open_in f in
  let d = parse_lines c in
    close_in c;
    d

(* Apply a diff, so that apply_diff (1, diff a b, a) = b. For
   testing purpose. *)
let rec apply_diff = function
  | count, (b, e, r) :: d, s when b <= count && e < count ->
    r @ apply_diff (count, d, s)
  | count, ((b, _, _) :: _ as d), _ :: s when b <= count ->
    apply_diff (count + 1, d, s)
  | count, ((b, _, _) :: _ as d), l :: s ->
    l :: apply_diff (count + 1, d, s)
  | _, [], s -> s
  | _ -> invalid_arg "apply_diff"

(* A chunk of a modified file is either an unmodified portion or
   modified one, which has an old and a new versions. Chunks may
   contain newline characters. *)
type chunk =
  | Same of string
  | Changed of string * string (* old, new *)

let rec cut_new count = function
  | (Same s | Changed (_, s)) as c :: chunks ->
    let l = String.length s in
    if count <= l then
      let s = String.sub s 0 count in
      let c =
	match c with
	  | Same _ -> Same s
	  | Changed (old, _) -> Changed (old, s)
      in
      [c]
    else
      c :: cut_new (count - l) chunks
  | [] ->  if count = 0 then [] else invalid_arg "cut_new"

let print_modified c =
  List.iter
    (function
      | Same s -> Printf.fprintf c "%s" s
      | Changed (old, last) ->
	Printf.fprintf c "*** REPLACED ***\n%s*** WITH ***\n%s*** END ***\n"
	  old last)

(* goto b acc (count, l) pops elements from l while incrementing
   counts and pushes them on acc, until count >= b (so, we move b -
   count elements, or 0 if count > b). *)
let rec goto b acc = function
  | count, l when count >= b -> acc, count, l
  | count, t :: q -> goto b (acc ^ t) (count + 1, q)
  | _ -> invalid_arg "goto"

let rec modified_file = function
  | count, (b, e, r) :: d, s ->
    let same, count, s = goto b "" (count, s) in
    let old, count, s = goto (e + 1) "" (count, s) in
    let r = List.fold_left ( ^ ) "" r in
    Same (same) :: Changed (old, r) :: modified_file (count, d, s)
  | _, [], [] -> []
  | _, [], s -> [Same (List.fold_left ( ^ ) "" s)]

(* Compute a modified file, i.e., a list of chunks, given the old
   version of the file and the diff between the old and the new
   versions. *)
let modified_file old diff = modified_file (1, diff, old)

let read_modified_file ?(empty_absent = true) old_file new_file =
  match Sys.file_exists old_file, Sys.file_exists new_file with
    | true, true ->
      let diff_file = Filename.temp_file
	(Filename.basename new_file ^ "-" ^ Filename.basename old_file) ".diff" in
      (match Sys.command ("diff -f " ^ old_file ^ " " ^ new_file ^ " >" ^ diff_file) with
	| 0 | 1 -> ()
	| _ -> failwith "error when invoking diff");
      let old = lines_of old_file
      and diff = parse_diff_file diff_file in
      modified_file old diff
    | _ when not empty_absent -> invalid_arg "read_modified_file"
    | false, true -> [Changed ("", List.fold_left ( ^ ) "" (lines_of new_file))]
    | true, false -> [Changed (List.fold_left ( ^ ) "" (lines_of old_file), "")]
    | false, false -> [Same ""]

(*
  let o = lines_of "../test/test.ml"
  let d = parse_diff_file "../test/diff.f"
  let f = apply_diff (1, d, o)
  let _ = List.iter print_endline f

  let o = lines_of "../../parsing/parser.mly"
  let d = parse_diff_file "../test/diff"
  let f = apply_diff (1, d, o)
  let _ =
  let c = open_out "../test/patched.mly" in
  List.iter (function l -> output_string c l ; output_char c '\n') f;
  close_out c
*)
