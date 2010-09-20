(** Defines an abstraction and some utilities for UTF-8 encoded characters. *)

(** the type of UTF-8 encoded characters. *)
type t

(** raised if an invalid UTF-8 encoding is encountered *)
exception Bad_encoding

(** Creates a UTF-8 character from a regular ASCII character.
@raise Bad_encoding if the character is outside the range 0-127 *)
val of_char : ( char -> t )

(** Creates a UTF-8 character from a string of bytes.

{b Warning!} Does not check if the bytes are valid! Use to_U_char to check. *)
val of_bytes : ( string -> t)

(** Emits a byte string from a UTF-8 character *)
val to_bytes : ( t -> string )

(** Determines the size, in bytes, of the UTF-8 character. *)
val size : ( t -> int )

(** Decode the character to a UNICODE character.
@raise Bad_encoding if there was a decoding error. *)
val to_U_char : ( t -> U_char.t )

(** Decoding utility: given just the first byte of a utf-8 character, determines how many bytes the whole character should be. (In UTF-8, this information is encoded in the first byte) *)
val utf8_character_size : ( int -> int )
