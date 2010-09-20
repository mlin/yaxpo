open Pull

type 'a xml_callbacks =
    {
      start_element : (qname -> att list -> 'a -> ('a->unit) -> unit);
      characters : (string -> 'a -> ('a->unit) -> unit);
      cdata : (string -> 'a -> ('a->unit) -> unit);
      comment : (string -> 'a -> ('a->unit) -> unit);
      pi : (string -> string -> 'a -> ('a->unit) -> unit);
      end_element : (qname -> 'a -> ('a->unit) -> unit);
    }

let rec parse_element (reader:#Cps_reader.t)(callbacks:'a xml_callbacks) (x:'a) (k:('a->unit)) =
  next_xml_token reader
    (function
	 Tok_start_tag(name,atts) -> 
	   callbacks.start_element name atts x (fun x' -> parse_rest_of_element reader callbacks name x' k)
       | Tok_empty_ele(name,atts) ->
	   callbacks.start_element name atts x (fun x' -> callbacks.end_element name x' k)
       | _ as bogus -> raise (Parse_error ("Unexpected token",(Some bogus))))
and parse_rest_of_element reader callbacks name x (k:('a->unit)) =
  let krec = (fun x' -> parse_rest_of_element reader callbacks name x' k) in
    next_xml_token reader
      (function
	   Tok_text(s) -> callbacks.characters s x krec
	 | Tok_start_tag(sub_name,atts) -> (* this is the heart of the CPS descent. *)
	     callbacks.start_element sub_name atts x
	     (fun x' -> parse_rest_of_element reader callbacks (**) sub_name (**) x' krec)
	 | Tok_empty_ele(name,atts) ->
	     callbacks.start_element name atts x (fun x' -> callbacks.end_element name x' krec)
	 | Tok_end_tag(end_name) when end_name = name -> callbacks.end_element name x k
	 | Tok_end_tag(bogus) as tok -> raise (Parse_error (("Expected close tag for " ^ (string_of_qname name)),Some tok))
	 | Tok_comment(s) -> callbacks.comment s x krec
	 | Tok_pi(target,s) -> callbacks.pi target s x krec
	 | Tok_cdata(s) -> callbacks.cdata s x krec)

let rec parse_document (reader:#Cps_reader.t) callbacks x (k:('a->unit)) =
let krec = (fun x' -> parse_document reader callbacks x' k) in
  next_xml_token reader
    (function
	 Tok_pi(target,s) -> callbacks.pi target s x krec
       | Tok_comment(s) -> callbacks.comment s x krec
       | Tok_start_tag(name,atts) -> (* document element *)
	   callbacks.start_element name atts x
	   (fun x' -> parse_rest_of_element reader callbacks name x' k)
       | Tok_empty_ele(name,atts) ->
	   callbacks.start_element name atts x
	   (fun x' ->
	      callbacks.end_element name x' k)
       | _ as bogus -> raise (Parse_error ("unexpected token in document prolog",Some bogus)))

