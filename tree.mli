(**
Defines an XML 1.0 + XMLNS data model and provides constructors, accessors, mutators, serializers, and parsers for that model.
*)

(** {C {b Data Model}} *)

(** UTF-8 encoded text *)
type txt = string

(** UTF-8 encoded CDATA section *)
type cdata = string

(** A QName consists of [(prefix,local_part,nsURI)]. nsURI is provided purely as a convenience; the parser will resolve the prefix into the corresponding URI for you, but everything else ignores it. *)
type qname =
    QName of txt*txt*txt                               

(** An attribute is [(name,value)]. The value is a [ref] to enable mutation. *)
type att =
    Att of qname*txt ref

(** The type of any XML elements: [(name,atts,children)]. *)
type ele =
    Element of qname*att list ref*content list ref

(** The type of element content. *)
and content =
    Text of txt
  | CDATA of cdata
  | Comment of txt
  | PI of txt*txt                                      (** [(target,value)] *)
  | Subele of ele

(** A document is a list of comments and PIs, and the document element. *)
type doc = content list ref*ele ref

(**
Raised if some namespace processing error occurs. examples:
- use of undeclared namespace
- two attributes on the same element with the same local part and different prefixes that
  resolve to the same URI (unique att. spec)
*)
exception Namespace_error


(** {C {b Constructors, Accessors, and Mutators}} *)

(** Retrieve the prefix of a QName. *)
val qname_pfx : (qname -> txt)

(** Retrieve the local-part of a QName. *)
val qname_local : (qname -> txt)

(** Retrieve the nsURI of a QName. *)
val qname_nsURI : (qname -> txt)

(** Returns true if the two [qname]s have the same prefix and local part. *)
val qname_eq : (qname -> qname -> bool)

(** Returns true if the two [qname]s have the same local part and nsURI. (Use = to determine if [qname]s are equal in all respects) *)
val qname_eq' : (qname -> qname -> bool)

(** Retrieve the QName of an attribute. *)
val att_name : (att -> qname)

(** Retrieve the value of an attribute. *)
val att_value : (att -> txt)

(** Find an attribute given its QName, using [qname_eq] to determine [qname] equality.
  @raise Not_found if the attribute isn't in the list. *)
val find_att : (qname -> att list -> att)

(** Ditto, except use [qname_eq'] *)
val find_att' : (qname -> att list -> att)

(** Create an empty element. *)
val make_ele : (qname -> ele)

(** Retrieve the name of an element *)
val ele_name : (ele -> qname)

(** Retrieve the attributes of an element *)
val ele_atts : (ele -> att list)

(** Replace the attributes of an element. *)
val set_ele_atts : (ele -> att list -> unit)

(** Retrieve the children of an attribute *)
val ele_children : (ele -> content list)

(** Replace the children of an element. *)
val set_ele_children : (ele -> content list -> unit)

(** Append a child node to an element. *)
val ele_append_child : (ele -> content -> unit)

(** Prepend a child node to an element's existing children. *)
val ele_prepend_child : (ele -> content -> unit)

(** Get an attribute value, using [qname_eq] to determine [qname] equality.
@raise Not_found if the attribute doesn't exist *)
val ele_get_att : (ele -> qname -> txt)

(** Ditto, except use [qname_eq'] *)
val ele_get_att' : (ele -> qname -> txt)

(** Set an attribute value, or add the attribute if it isn't already there. *)
val ele_set_att : (ele -> qname -> txt -> unit)

(** Remove an attribute, using [qname_eq] to determine [qname] equality. *)
val ele_remove_att : (ele -> qname -> unit)

(** Ditto, except use [qname_eq'] *)
val ele_remove_att' : (ele -> qname -> unit)

(** Returns the text contained in the first Text child of the element. Useful for processing elements expected to be of the form [<ele>some-text</ele>] *)
val ele_inner_text : (ele -> txt)

(** Returns the prolog (comments and PIs) of a document *)
val doc_prolog : (doc -> content list)

(** Returns the document element *)
val doc_ele : (doc -> ele)

val clone_qname : (qname -> qname)
val clone_att : (att -> att)
val clone_ele : (ele -> ele)
val clone_content : (content -> content)
(** Deep copy *)

(** {C {b Parsers}} *)

(** Parse one XML element.

@param namespace_declarations A stack of lists of namespace declaration attributes. This is used to let the parser know about namespace prefixes declared in parent elements if you are only parsing a fragment of the document (e.g. in XMPP). Default: empty stack.
@raise U_char.Bad_char if an invalid character is encountered.
@raise Utf8_char.Bad_encoding if a UTF-8 decoding error occurs.
@raise Pull.Lex_error if a lexical error is encountered.
@raise Pull.Parse_error if a parsing error is encountered.
@raise Namespace_error if a namespace processing error is encountered.

{b CPS Note:} [build_element_tree] will fail if the underlying Cps_reader.t attempts to pause parsing. To use a pausing reader, you should use the CPS version of this function below.
*)
val parse_element : (?namespace_declarations:(Pull.att list Stack.t) -> #Cps_reader.t -> ele)

(** Parse a complete XML document.

@raise U_char.Bad_char if an invalid character is encountered.
@raise Utf8_char.Bad_encoding if a UTF-8 decoding error occurs.
@raise Pull.Lex_error if a lexical error is encountered.
@raise Pull.Parse_error if a parsing error is encountered, or if content comes in the wrong order.
@raise Namespace_error if a namespace processing error is encountered.

{b CPS Note:} [build_doc_tree] will fail if the underlying Cps_reader.t attempts to pause parsing. To use a pausing reader, you should use the CPS version of this function below.
*)
val parse_doc : (#Cps_reader.t -> doc)

(** CPS version of [parse_element] *)
val parse_element_cps : (?namespace_declarations:(Pull.att list Stack.t) -> #Cps_reader.t -> (ele->unit) -> unit)

(** CPS version of [parse_doc] *)
val parse_doc_cps : (#Cps_reader.t -> (doc->unit) -> unit)

(** {C {b Serializers}} *)

val string_of_qname : (qname -> string)
val serialize_ele : ?indent:int -> Buffer.t -> ele -> unit
val serialize_doc : ?indent:int -> Buffer.t -> doc -> unit
val string_of_ele : ?indent:int -> ele -> string
val string_of_doc : ?indent:int -> doc -> string

(** {b General note:} the parser is pretty rigorous about verifying the well-formedness of its input. however, the same is not true of the serializers! you are expected to pass the serializers only valid data. Especially note that the serializers pay {b no attention} to the [nsURI] parts of [qname]s; you are expected to emit [xmlns] declarations in the right places. However, the following may be of use. *)

module Namespace_utils :
sig
  (** {b Experimental.} Walks the tree of the element, and adds namespace declaration attributes as needed to ensure that in the resulting tree, all namespace prefixes are consistent with the [nsURI] parts of the [qname]s. The second parameter is a list of lists of namespace declaration attributes representing known prefixes (which do not need to be redeclared); normally you should pass the empty list. *)
  val qualify_namespaces : (ele -> att list list -> unit)
end

