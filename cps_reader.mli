(**
This defines the interface for the "reader" objects that the parser expects, as well as a few implementations thereof. Typically, these objects will take data from some normal source, like a string, file, or socket, and feed them to the parser.

The reader methods are invoked through continuation-passing style. This means that
instead of being expected to return a value to its caller, it is passed a "continuation"
(a procedure) to which it should pass the result in a tail-call.
*)

(** The basic reader interface. *)
class type t =
object
  (** peek at the next char without removing it *)
  method peek_char : (Utf8_char.t -> unit) -> unit

  (** read the next char *)
  method read_char : (Utf8_char.t -> unit) -> unit

  (** read while pred returns true for each character, and return the result as a string
   
    this is just a performance optimization, and could in principle be implemented on top of [peek_char] and [read_char].*)
  method read_while : (Utf8_char.t -> bool) -> (string -> unit) -> unit
end

(** Creates a reader from the given ASCII string. *)
val make_string_reader : (string -> t)

(** Creates a reader from the given UTF-8 string.

@raise Utf8_char.Bad_encoding if an invalid UTF-8 encoding is encountered. *)
val make_utf8_string_reader : (string -> t)

(** Creates a reader from a UTF-8 file given the filename.

@raise Utf8_char.Bad_encoding if an invalid UTF-8 encoding is encountered.
@raise Sys_error if the file couldn't be opened. *)
val make_utf8_file_reader : (string -> t)
