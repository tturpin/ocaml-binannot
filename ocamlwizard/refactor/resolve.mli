(** Different sort of names, and their bindings. *)

type sort = [ `Modtype | `Module | `Value ]
(* TODO: 'Type... *)

type specifics = {
  sort : sort;
  lookup : Longident.t -> Env.t -> Path.t;
  sig_item : Types.signature_item -> Ident.t option;
  summary_item : Env.summary -> Ident.t option;
}

val value_ops : specifics
val module_ops : specifics

(** Return the specific operations associated with a signature item. *)
val sig_item_ops : Types.signature_item -> specifics

(** See modtype. *)
val resolve_modtype :
  Env.t ->
  Path.t ->
  [> `func of Ident.t * Types.module_type * Types.module_type
   | `sign of Types.signature ]

(** See modtype. *)
val resolve_module : Env.t -> Path.t -> Types.signature

(** Get the signature (or functor signature) corresponding to a module type *)
val modtype :
  Env.t -> Types.module_type ->
  [ `func of Ident.t * Types.module_type * Types.module_type
  | `sign of Types.signature ]

(** Same as modtype, but the result must be a signature. *)
val modtype_signature : Env.t -> Types.module_type -> Types.signature

(** Same as modtype, but the result must be a functor. *)
val modtype_functor :
  Env.t -> Types.module_type ->
  Ident.t * Types.module_type * Types.module_type

(** [resolves_to kind env lid ids] tests whether a lid reffers to one
    of ids in environment env, i.e., if the object directly denoted by
    lid is named with one of ids. This indicates that the rightmost
    name in lid needs renaming (assuming we are renaming ids). *)
val resolves_to : specifics -> Env.t -> Longident.t -> Ident.t list -> bool

(** Raised by check to signal an impossible renaming due to a masking
    of the new name by another element *)
exception Masked_by of Ident.t

(** Check that the renaming of a list of idents (with the same name)
    into a new name would not change the meaning of this name in a
    given environment, i.e., if the old name referred to one of the
    ids, then this the new one must reffer to the same ident. *)
val check :
  specifics -> Ident.t list -> string -> Env.t -> Env.summary -> unit

(** Similar to check, but check that the renaming would not change the
    meaning of names in a signature. *)
val check_in_sig :
  specifics -> Ident.t list -> string -> Types.signature -> unit

(** Test if an id belongs to a list of ids *)
val is_one_of : Ident.t -> Ident.t list -> bool
