(* Searching int Typedtrees *)

open Typedtree

type 'a process = {
  structure : Typedtree.structure -> 'a;
  signature : Typedtree.signature -> 'a
}

module type FindArgument = sig
  type t
  val pattern : pattern -> t option
  val expression : expression -> t option
end

module DefaultFindArgument :
  functor (T : sig type t end) -> FindArgument
  with type t = T.t

module Find :
  functor (T : FindArgument) -> sig
    val structure : structure -> T.t
    val signature : signature -> T.t
    val process : T.t process
  end

val find_pattern : (Typedtree.pattern -> 'a option) -> 'a process
val find_expression : (Typedtree.expression -> 'a option) -> 'a process
