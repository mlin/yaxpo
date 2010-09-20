(** Provides a SAX-like interface, where the parser invokes user callbacks as it processes a document. *)

open Pull

(** {C {b Definition of callbacks}} *)

(** Each field should be set to a function that will be invoked when the corresponding token is parsed from the XML data.

Each function consumes and produces a value of arbitrary type which is passed along sequentially as the parsing proceeds. The first callback is given a seed value, the second callback is passed the value produced by the first callback, and so on.

The callbacks are invoked in {e continuation-passing style} (CPS). They are each passed an additional argument besides what you expect, a procedure of type [('a->unit)]. When your callback is finished processing, instead of returning control, it should issue a tail-call to this procedure. *)
type 'a xml_callbacks =
    {
      start_element : (qname -> att list -> 'a -> ('a->unit) -> unit);
      characters : (string -> 'a -> ('a->unit) -> unit);
      cdata : (string -> 'a -> ('a->unit) -> unit);
      comment : (string -> 'a -> ('a->unit) -> unit);
      pi : (string -> string -> 'a -> ('a->unit) -> unit);
      end_element : (qname -> 'a -> ('a->unit) -> unit);
    }

(** {C {b Parsing routines}} *)

(** Parse the next element from the reader. Accepts a [Cps_reader.t], your callbacks, a seed value of your datatype, and a continuation which will accept the final transformed value of your datatype.

@raise U_char.Bad_char if an invalid character is encountered.
@raise Utf8_char.Bad_encoding if a UTF-8 decoding error occurs.
@raise Lex_error if a lexical error is encountered.
@raise Parse_error if a parsing error is encountered.*)
val parse_element : (#Cps_reader.t -> 'a xml_callbacks -> 'a -> ('a->unit) -> unit)

(** Parse a document (consisting of some comments and PIs followed by a document element)

@raise U_char.Bad_char if an invalid character is encountered.
@raise Utf8_char.Bad_encoding if a UTF-8 decoding error occurs.
@raise Lex_error if a lexical error is encountered.
@raise Parse_error if a parsing error is encountered, or if content comes in the wrong order. *)
val parse_document : (#Cps_reader.t -> 'a xml_callbacks -> 'a -> ('a->unit) -> unit)

(**/**)

(* internal use *)
val parse_rest_of_element : (#Cps_reader.t -> 'a xml_callbacks -> qname -> 'a -> ('a->unit) -> unit)
