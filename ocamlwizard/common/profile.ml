open Printf

let init_time = Unix.gettimeofday ()

(* The (abstract) call stack *)
let stack = ref ["main", init_time]

(* Maps each pair of functions to the time spent in this context *)
let edges = Hashtbl.create 100

let lookup e =
  try
    Hashtbl.find edges e
  with Not_found ->
    let r = ref 0. in
      Hashtbl.add edges e r;
      r

(* Operations for instrumentation *)
let time_op op =
  let time = Unix.gettimeofday () in
    (match op with
       | `push f ->
	   stack := (f, time) :: ! stack
       | `pop ->
	   (match ! stack with
	      | (f, t) :: ((f', _) :: _ as st) ->
		  let r = lookup (f', f) in
		    r := ! r +. time -. t;
		    stack := st
	      | [] | [_] -> invalid_arg "time_pop")
       | `switch_to f'' ->
	   (match ! stack with
	      | (f, t) :: ((f', _) :: _ as st) ->
		  let r = lookup (f', f) in
		    r := ! r +. time -. t;
		    stack := (f'', time) :: st
	      | [] | [_] -> invalid_arg "time_switch_to")
    )

let time_switch_to op = time_op (`switch_to op)
let time_push op = time_op (`push op)
let time_pop () = time_op `pop

let time_call name f x =
  (* try ... finally *)
  time_push name;
  try
    let r = f x in
      time_pop ();
      r
  with e ->
    time_pop ();
    raise e

(*
  let time_call _ = identity
*)

let time_stack c =
  match !stack with
    | [] -> ()
    | (f, _) :: stack ->
      fprintf c "%s" f;
      List.iter
	(function f, _ ->
	  fprintf c "->%s" f)
	stack

let hashtbl_keys t =
  Hashtbl.fold
    (fun k _ l ->
      match l with
	| k' :: _ as l when k = k' -> l
	| l -> k :: l)
    t
    []

let rec remove_duplicates = function
  | x :: (y :: _ as l) ->
    if x = y then
      remove_duplicates l
    else
      x :: remove_duplicates l
  | l -> l

let remove_duplicates l =
  List.fold_right
    (fun x l -> if List.mem x l then l else x :: l)
    l
    []


(* Computing node values from edge values *)
let time_stats () =
  while List.tl ! stack <> [] do time_pop () done;
  let total_time = Unix.gettimeofday () -. init_time in
  let nodes = "main" :: (List.map (function _, f -> f) (hashtbl_keys edges)) in
  let nodes = remove_duplicates nodes in
  let node_times =
    List.map
      (function f ->
        f,
        (if f = "main" then total_time else
	    Hashtbl.fold
	      (fun (_, f') r n -> if f' = f then !r +. n else n)
	      edges
	      0.),
	Hashtbl.fold
	  (fun (f', _) r n -> if f' = f then !r +. n else n)
	  edges
	  0.)
      nodes
  in
  total_time, node_times

(* Writing data to a file *)
let write_stats (total_time, node_times) =
  let c = open_out "profile.out" in
  fprintf c
    "total time: %f, monitored functions: %d\n"
    total_time (List.length node_times);
  List.iter
    (function f, tin, tout ->
      fprintf c "function %S total %f calees %f\n"
	f tin tout)
    node_times;
  Hashtbl.iter
    (fun (f, f') t ->
      fprintf  c "edge %S calling %S total %f\n"
	f f' !t)
    edges;
    close_out c

let print_time_stats () = write_stats (time_stats ())

let rec parse_lines f =
  try
    let i = input_line f in
      (i ^ "\n") :: parse_lines f
  with
      End_of_file -> []

let lines_of f =
  let c = open_in f in
  let d = parse_lines c in
    close_in c;
    d

let rec cut acc = function
  | 0, l -> List.rev acc, l
  | n, t :: q -> cut (t :: acc) (n-1, q)
  | _, [] -> invalid_arg "cut"

let cut n l = cut [] (n, l)

(* Reading saved data *)
let read_stats () =
  match lines_of "profile.out" with
    | first :: lines ->
      let total_time, n =
	Scanf.sscanf
	  first
	  "total time: %f, monitored functions: %d" (fun t n -> t, n)
      in
      let nodes, edges = cut n lines in
      let nodes =
	List.map
	  (function l ->
	    Scanf.sscanf l
	      "function %S total %f calees %f"
	      (fun f tin tout -> f, tin, tout))
	  nodes
      in
      let edges =
	List.map
	  (function s ->
	    Scanf.sscanf s
	      "edge %S calling %S total %f"
	      (fun f f' t -> f, f', t))
	  edges
      in
      total_time, nodes, edges
    | [] -> invalid_arg "read_stats"

(* dot graph output *)
let write_graph (total_time, node_times, edge_times) =
  let print t c = fprintf c "%4.1f" (t  *. 100. /. total_time)
  and fill_color tin tout c =
    let h = 0.5
    and s = 0. +. 1. *. ((tin-.tout) /. total_time) ** 0.7
    and v = 1. in
    fprintf c "\"%f,%f,%f\"" h s v
  and color tin tout c =
    let h = 0.5
    and s = 0. +. 1. *. ((tin-.tout) /. total_time) ** 0.7
    and v = 1. -. (tin /. total_time) ** 1. in
    fprintf c "\"%f,%f,%f\"" h s v
  and edge_color t c =
    let h = 0.
    and s = 0.
    and v = 0.95 *. (1. -. (t /. total_time) ** 0.8) in
    fprintf c "\"%f,%f,%f\"" h s v
  and text_color t c =
    let h = 0.
    and s = 0.
    and v = 0.85 *. (1. -. (t /. total_time) ** 0.8) in
    fprintf c "\"%f,%f,%f\"" h s v
  in
  let node_times =
    List.sort (fun (_, t, _) (_, t', _) -> compare t' t) node_times
  in
  let c = open_out "profile.dot" in
  fprintf c
    "digraph profile {\n\
       rankdir=LR;\n\
       splines=true;\n\
       node [shape=ellipse];\n\
       node [style=\"filled,bold\"];\n\
       node [fontsize=\"10pt\"];\n\
       edge [style=bold];\n";
  List.iter
    (function f, tin, tout ->
      fprintf c
         "\"%s\" [\
           label=\"\\N\\n\\n%t / %t\",\
           fontcolor=%t,\
           color=%t,\
           fillcolor=%t];\n"
	f (print tin) (print (tin-.tout))
	(text_color tin) (color tin tout) (fill_color tin tout))
    node_times;
  List.iter
    (function f, f', t ->
      fprintf c
         "\"%s\" -> \"%s\" [\
           label=\"%t\",\
           color=%t,\
           fontcolor=%t];\n"
	f f' (print t) (edge_color t) (text_color t))
    edge_times;
  fprintf c "}";
(*
           weight=$d:int_of_float t$,\
*)
    close_out c

let _ =
  if Filename.basename Sys.argv.(0) = "profile" then
    write_graph (read_stats ())
