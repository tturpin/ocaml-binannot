(** The core of renaming. *)

(** Rename all occurrences of any of a list of idents with the same
    name in a longident, given the environmemt in which this longident
    makes sense, and make sure that the new longident still reffers to
    the same thing (i.e., is not subject to masking by other
    equally-named elements). *)
val rename_in_lid :
  Resolve.specifics -> Ident.t list -> string ->
  Env.t -> Resolve.specifics -> Longident.t -> Longident.t option
