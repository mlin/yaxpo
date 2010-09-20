(** Defines an abstraction for UNICODE characters. *)

(** The type of UNICODE characters. *)
type t

(** Raised if something tries to use an invalid character (i.e., outside the range 0x000000-0x10FFFF) *)
exception Bad_char

val of_char : (char -> t)
val of_int : (int -> t)
val to_int : (t -> int)
val of_int32 : (int32 -> t)

(** The following determine if a character is of a type defined by the XML specification. *)

val is_char : (t -> bool)
val is_space : (t -> bool)
val is_digit : (t -> bool)
val is_basechar : (t -> bool)
val is_ideographic : (t -> bool)
val is_letter : (t -> bool)
val is_combiningchar : (t -> bool)
val is_extender : (t -> bool)
val is_namechar : (t -> bool)
val is_ncnamechar : (t -> bool)
