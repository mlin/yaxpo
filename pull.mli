(** Provides XML lexing and basic parsing through a "pull" interface. This is mostly just a lexer; see the [Pull] and [DOM] modules for more conventional parsers. *)

(** {C {b Basic datatypes}} *)

(** thrown with a description when an error occurs during lexical analysis. *)
exception Lex_error of string

(** the type of literal character data *)
type txt = string

(** the type of CDATA section data *)
type cdata = string

(** the type of QNames (prefix:local). pfx = "" means no prefix. *)
type qname = { pfx:txt; local:txt } (* pfx = "" -> no prefix *)

(** the type of attributes *)
type att = { att_name:qname; mutable att_value:txt }


(** {C {b Serializers - useful for debugging}} *)

val string_of_qname : (qname -> string)

val string_of_att : (att -> string)


(** {C {b Pull Interface}} *)


(** The type of XML tokens. *)
type xml_token =
    Tok_text of txt
  | Tok_cdata of cdata
  | Tok_start_tag of qname*att list
  | Tok_empty_ele of qname*att list
  | Tok_end_tag of qname
  | Tok_comment of txt
  | Tok_pi of txt*txt

(** thrown with a description when an error occurs during lexical analysis. May also include the offending token if appropriate.

The distinction between a parse error and a lex error is not too well defined, so you should take it lightly. *)
exception Parse_error of string*xml_token option

(** Pulls the next token from the stream. The second argument is a {e continuation}, which is a function you write. When [pull_next_token] completes, it invokes your continuation with the new token, instead of returning control. Depending on what reader you use, it is possible for [pull_next_token] to return control without having invoked your continuation; this may occur if there is not enough data available.

{b Important:} [pull_next_token] only performs local lexical analysis on the next token in the stream. It maintains no call-to-call state and will {b not} raise errors if tokens come in incorrect order, such as an unmatched end tag. Although it is not very difficult to do at all, this is the responsibility of higher-level parsers.

@raise U_char.Bad_char if an invalid character is encountered.
@raise Utf8_char.Bad_encoding if a UTF-8 decoding error occurs.
@raise Lex_error if a lexical error is encountered.
@raise Parse_error if a local parsing error is encountered; for example, an attribute value with no end quote. *) 
val next_xml_token : (#Cps_reader.t -> (xml_token->unit) -> unit)
