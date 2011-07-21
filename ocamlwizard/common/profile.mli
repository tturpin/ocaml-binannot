(** Time profiling *)

(** This module implements a flexible approach to time profiling of
    ocaml programs. We require the user to instrument the program
    explicitely, which has two advantages:
    - We avoid too fine-grained instrumentation (e.g., Pervasives.compare)
      which sometimes make the result of "-p" profiling irrelevant.
    - We can differentiate successive steps in a piece of code without
      having to put them into separate functions.
    Times are recorded for nodes (typically, functions) with and without
    child nodes (calees), and for individual edges. *)

(** {3 Usage} *)

(** The program should be instrumented using the four [time_]* functions
    (see their respective documentation) and it should eventually call
    [print_time_stats] just before termination, which dumps timing
    information in profile.out (in the current directory).

    Then, calling the executable [profile] (which only consists of the
    present module) will read profile.out and write a time-annotated
    call-graph in profile.dot (these two steps allow tuning the dot
    graph without having to re-run a one-hour long execution each
    time, as well as alternative visualisations).

    The dot file is then processed with dot, for example with [dot
    profile.dot -Tsvg -o profile.svg] for a SVG output which may be
    rendered by web browsers.

    {b Limitation:} recursion is not supported. *)

(** {3 Interface for instrumentation} *)

(** Enter a new node with given name. *)
val time_push : string -> unit

(** Leave the topmost node on the call stack. The dynamic calls to
    [time_push] and [time_pop] should match each other (be careful
    with exceptions). *)
val time_pop : unit -> unit

(** Same as pop then push. *)
val time_switch_to : string -> unit

(** The above three functions are used as follows:

    {[
    ...
    time_push "step 1";
    ... (* code for step 1 *)
    time_switch_to "step 2";
    ... (* code for step 2 *)
    time_pop ();
    ...]} *)

(** Use e.g. [let f = time_call "f" f] to instrument f as a whole (for
    currified functions, write [let f x y = time_call "f" (f x) y]).
    [time_call] takes care of exceptions. *)
val time_call : string -> ('a -> 'b) -> 'a -> 'b

(** Write raw data in profile.out. *)
val print_time_stats : unit -> unit
