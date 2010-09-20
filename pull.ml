(*
pull.ml
Mike Lin
mikelin@mit.edu
*)

(* TODO handle DOCTYPE *)

(* lexing routines *)

exception Lex_error of string

type txt = string
type cdata = string
type qname = { pfx:txt; local:txt } (* pfx = "" -> no prefix *)
type att = { att_name:qname; mutable att_value:txt }

let string_of_qname name = 
  if (String.length name.pfx) > 0 then
    name.pfx ^ ":" ^ name.local
  else
    name.local

let string_of_att att = (string_of_qname att.att_name) ^ "=\"" ^ att.att_value ^ "\""

let skip_ws (reader:#Cps_reader.t) (k:(unit->unit)) =
  reader#read_while
    (fun c ->
       match Utf8_char.to_bytes c with
	   " " | "\n" | "\r" | "\t" -> true
	 | _ -> false)
    (fun whitespace -> k ())

let read_n (reader:#Cps_reader.t) n (k:(string->unit)) =
  let sofar = ref (-1) in
    reader#read_while
      (fun c -> sofar := !sofar + 1; !sofar < n)
      k

(* Name    ::=    (Letter | '_' | ':') (NameChar)* *)
let read_name (reader:#Cps_reader.t) (k:(string->unit)) =
  reader#read_char
    (fun firstc ->
       let chr = Utf8_char.to_U_char firstc in
	 if not ((U_char.is_letter chr) || chr = U_char.of_char '_' || chr = U_char.of_char ':') then
	   raise (Lex_error ("Invalid character in name: " ^ (Utf8_char.to_bytes firstc)));
	 reader#read_while
	   (fun c -> U_char.is_namechar (Utf8_char.to_U_char c))
	   (fun rest ->
	      k ((Utf8_char.to_bytes firstc) ^ rest)))

(* NCName  ::= (Letter | '_') (NCNameChar)* *)
let read_ncname (reader:#Cps_reader.t) (k:(string->unit)) =
  reader#read_char
    (fun firstc ->
       let chr = Utf8_char.to_U_char firstc in
	 if not ((U_char.is_letter chr) || chr = U_char.of_char '_') then
	   raise (Lex_error ("Invalid character in name: " ^ (Utf8_char.to_bytes firstc)));
	 reader#read_while
	   (fun c -> U_char.is_ncnamechar (Utf8_char.to_U_char c))
	   (fun rest ->
	      k ((Utf8_char.to_bytes firstc) ^ rest)))

(* picks up reading a decimal character reference from &#
   (reader should point to first digit) *)
let read_rest_dec_char_reference (reader:#Cps_reader.t) (k:(string->unit)) =
  reader#read_while
    (fun c ->
       if (Utf8_char.size c) <> 1 then
	 raise (Lex_error ("Invalid character in character reference: " ^ (Utf8_char.to_bytes c)));
       (match (String.get (Utf8_char.to_bytes c) 0) with
	    '0' .. '9' -> true
	  | ';' -> false
	  | _ -> raise (Lex_error ("Invalid character in character reference: " ^ (Utf8_char.to_bytes c) ))))
    (fun dec_char ->
       reader#read_char
       (fun semicolon ->
	  k ("&#" ^ dec_char ^ ";"))) (* TODO: check numerical validity! *)

(* picks up reading a hex character reference from &#
   (assumes the x has already been peeked - reader should point to x) *)
let read_rest_hex_char_reference (reader:#Cps_reader.t) (k:(string->unit)) =
  reader#read_char
    (fun x ->
       reader#read_while
       (fun c ->
	  if (Utf8_char.size c) <> 1 then
	    raise (Lex_error ("Invalid character in character reference: " ^ (Utf8_char.to_bytes c)));
	  (match (String.get (Utf8_char.to_bytes c) 0) with
	       '0' .. '9' -> true
	     | 'a' .. 'f' -> true
	     | 'A' .. 'F' -> true
	     | ';' -> false
	     | _ -> raise (Lex_error ("Invalid character in character reference: " ^ (Utf8_char.to_bytes c)))))
       (fun hex_char ->
	  reader#read_char
	  (fun semicolon ->
	     k ("&#x" ^ hex_char ^ ";")))) (* TODO: check numerical validity! *)

(* picks up reading a character reference from &#
   (assumes the # has already been peeked - reader should point to #) *)
let read_rest_char_reference (reader:#Cps_reader.t) (k:(string->unit)) =
  reader#read_char
    (fun number_sign ->
       assert (number_sign = Utf8_lit.hash);
       reader#peek_char
       (fun c ->
	  if (Utf8_char.size c) <> 1 then
	    raise (Lex_error ("Invalid character in character reference: " ^ (Utf8_char.to_bytes c)));
	  (match (String.get (Utf8_char.to_bytes c) 0) with
	       'x' -> read_rest_hex_char_reference reader k
	     | '0' .. '9' -> read_rest_dec_char_reference reader k
	     | _ -> raise (Lex_error ("Invalid character in character reference: " ^ (Utf8_char.to_bytes c))))))

(* picks up reading an entity reference from &<x>, where <x> is not #
   (reader should point to <x>) *)
let read_rest_entity_reference reader k =
  read_name reader
    (fun name ->
       reader#read_char
       (fun semicolon ->
	  if not (semicolon = Utf8_lit.semicolon) then
	    raise (Lex_error "Semicolon expected");
	  k ("&" ^ name ^ ";"))	  )

(* reads a reference.
   (reader should point to &)
   nb: references are only checked for well-formedness; they are not resolved.
*)
let read_reference (reader:#Cps_reader.t) (k:(string->unit)) =
  reader#read_char
    (fun ampersand ->
       if not (ampersand = Utf8_lit.amp) then
	 raise (Lex_error "'&' expected");
       reader#peek_char
	 (fun nextc ->
	    if nextc = Utf8_lit.hash then
	      read_rest_char_reference reader k
	    else
	      read_rest_entity_reference reader k))

let read_chardata (reader:#Cps_reader.t) (k:(string->unit)) =
  let b = Buffer.create 32 in
  let rec helper () =
    reader#read_while
    (fun c -> not (c = Utf8_lit.lt or c = Utf8_lit.amp))
    (fun str ->
       Buffer.add_string b str;
       reader#peek_char
	 (fun c ->
	    if c = Utf8_lit.amp then
	      (read_reference reader
		 (fun str ->
		    Buffer.add_string b str;
		    helper()))
	    else
	      k (Buffer.contents b))) in
    helper ()

(* TODO: attribute value normalization *)		
let read_attvalue (reader:#Cps_reader.t) (k:(string->unit)) =
  let quote = ref Utf8_lit.quot in
    reader#read_char
      (fun c ->
	 if c = Utf8_lit.quot or c = Utf8_lit.apos then
	   quote := c
	 else
	   raise (Lex_error "Expected \" or '");
	 let b = Buffer.create 8 in
	 let rec helper () =
	   reader#read_while
	     (fun c -> not (c = Utf8_lit.lt or c = Utf8_lit.amp or c = !quote))
	     (fun str ->
		Buffer.add_string b str;
		reader#peek_char
		  (fun c ->
		     if c = Utf8_lit.amp then
		       (read_reference reader
			  (fun str ->
			     Buffer.add_string b str;
			     helper()))
		     else if c = !quote then
		       (reader#read_char (fun endquote -> k (Buffer.contents b)))
		     else
		       raise (Lex_error ("Invalid character in attribute value: " ^ (Utf8_char.to_bytes c))))) in
	   helper())

(* Basic parsing routines *)

type xml_token =
    Tok_text of txt
  | Tok_cdata of cdata
  | Tok_start_tag of qname*att list
  | Tok_empty_ele of qname*att list
  | Tok_end_tag of qname
  | Tok_comment of txt
  | Tok_pi of txt*txt

exception Parse_error of string * xml_token option

(*
parse a qname
pre: reader points to first character of qname.
post: reader is left on first character following qname.
*)

let parse_qname reader (k:(qname->unit)) =
  read_ncname reader
  (fun firstpart ->
     reader#peek_char
     (fun nextc ->
	if nextc = Utf8_lit.colon then
	  reader#read_char
	    (fun colon ->
	       read_ncname reader
	       (fun secondpart -> k { pfx=firstpart; local=secondpart; }))
	else
	  k { pfx=""; local=firstpart; }))
  
(*
parsees an attribute
pre: reader is on first character of attribute's qname.
post: reader is on first character following the close-quote
*)

let parse_att reader (k:(att->unit)) =
  parse_qname reader
  (fun name ->
     skip_ws reader
     (fun () ->
	reader#read_char
	(fun eq ->
	   if not (eq = Utf8_lit.eq) then
	     raise (Lex_error "Expected =");
	   skip_ws reader
	     (fun () ->
		read_attvalue reader
		(fun value ->
		   k { att_name = name; att_value = value; })))))

(*
parses a list of attributes
pre: none
post: reader is left on first character that is not part of an attribute
*)

let rec parse_atts (reader:#Cps_reader.t) init_list (k:(att list -> unit)) =
  reader#peek_char
    (fun c ->
       let chr = Utf8_char.to_U_char c in
	 if ((U_char.is_letter chr) || chr = U_char.of_char '_') then
	   parse_att reader
	     (fun next_att ->
		(* WFC: unique att *)
		List.iter (fun some_att ->
			     if some_att.att_name = next_att.att_name then
			       raise (Parse_error (("Attribute " ^ (string_of_qname next_att.att_name) ^
						   " is specified more than once"),None));
			     ()) init_list;
		skip_ws reader
		(fun () ->
		   parse_atts reader (next_att :: init_list) k))
	 else
	   k (List.rev init_list))

type xml_token_kind =
    Tok_kind_text
  | Tok_kind_cdata
  | Tok_kind_start_tag (* could indicate either start tag or empty ele *)
  | Tok_kind_end_tag
  | Tok_kind_comment
  | Tok_kind_pi

(*
parses exactly far enough to determine the kind of the next token
reader postconditions:
Tok_kind_text -> reader is unchanged
Tok_kind_cdata -> reader points to the first [ in <![CDATA[...
Tok_kind_start_tag -> reader points to first character of qname
Tok_kind_end_tag -> reader points to /
Tok_kind_comment -> reader points to the first - in <!--
Tok_kind_pi -> reader points to the ? in <?...
*)

let determine_next_token_kind (reader:#Cps_reader.t) (k:(xml_token_kind->unit)) =
  reader#peek_char 
    (fun c ->
       if c = Utf8_lit.lt then
	 reader#read_char
	   (fun lt ->
	      reader#peek_char
	      (fun nextc ->
		 if nextc = Utf8_lit.slash then k Tok_kind_end_tag
		 else if nextc = Utf8_lit.ques then k Tok_kind_pi
		 else if nextc = Utf8_lit.excl then
		   reader#read_char
		     (fun excl ->
			reader#peek_char
			(fun nextc ->
			   if nextc = Utf8_lit.openbr then k Tok_kind_cdata
			   else if nextc = Utf8_lit.dash then k Tok_kind_comment
			   else raise (Lex_error "expected [ or -")))
		 else if U_char.is_ncnamechar (Utf8_char.to_U_char nextc) then k Tok_kind_start_tag
		 else raise (Lex_error "Invalid < in content")))
       else
	 k Tok_kind_text)

(*
picks up parsing a start tag where determine_next_token left off.
pre: reader points to first character of tag qname
post: reader points to first character after >, and kont is told whether this
      was a start tag or an empty element.
*)
let finish_parse_start_tag (reader:#Cps_reader.t) (k:(xml_token -> unit)) =
  parse_qname reader
    (fun name ->
       skip_ws reader
       (fun () ->
	  parse_atts reader []
	  (fun atts ->
	     reader#read_char
	     (fun c ->
		if c = Utf8_lit.slash then
		  reader#read_char
		    (fun gt ->
		       if not (gt = Utf8_lit.gt) then
			 raise (Lex_error "expected >");
		       k (Tok_empty_ele (name,atts)))
		else if c = Utf8_lit.gt then k (Tok_start_tag (name,atts))
		else raise (Lex_error ("expected / or >, instead found " ^ (Utf8_char.to_bytes c)))))))

let finish_parse_end_tag (reader:#Cps_reader.t) (k:(xml_token -> unit)) =
  reader#read_char
  (fun slash ->
     if not (slash = Utf8_lit.slash) then
       raise (Lex_error ("expected /"));
     parse_qname reader
       (fun name ->
	  skip_ws reader
	  (fun () ->
	     reader#read_char
	     (fun gt ->
		if not (gt = Utf8_lit.gt) then
		  raise (Lex_error "expected >");
		k (Tok_end_tag name)))))

let finish_parse_comment (reader:#Cps_reader.t) (k:(xml_token -> unit)) =
  read_n reader 2
    (fun twodashes ->
       if not (twodashes = Utf8_lit.twodashes) then
	 raise (Lex_error "malformed comment");
       let b = Buffer.create 16 in
       let rec helper () =
	 reader#read_while
	   (fun c ->
	      if not (U_char.is_char (Utf8_char.to_U_char c)) then
		raise (Lex_error "invalid character");
	      not (c = Utf8_lit.dash))
	   (fun s ->
	      Buffer.add_string b s;
	      read_n reader 2
		(fun twodashes ->
		   if twodashes = Utf8_lit.twodashes then
		     reader#read_char
		       (fun gt ->
			  if not (gt = Utf8_lit.gt) then
			    raise (Lex_error "malformed comment");
			  k (Tok_comment (Buffer.contents b)))
		   else
		     (Buffer.add_string b twodashes;
		      helper())))
       in
	 helper())

let finish_parse_pi (reader:#Cps_reader.t) (k:(xml_token->unit)) =
  reader#read_char
    (fun q ->
       if not (q = Utf8_lit.ques) then
	 raise (Lex_error "malformed processing instruction");
       read_name reader
	 (fun target ->
	    (* TODO: check that name does not contain "xml" *)
	    reader#read_char
	    (fun nextc ->
	       if U_char.is_space (Utf8_char.to_U_char nextc) then
		 let b = Buffer.create 16 in
		 let rec helper () =
		   reader#read_while
		     (fun c ->
			if not (U_char.is_char (Utf8_char.to_U_char c)) then
			  raise (Lex_error "invalid character");
			not (c = Utf8_lit.ques))
		     (fun s ->
			Buffer.add_string b s;
			reader#read_char
			  (fun q ->
			     assert (q = Utf8_lit.ques);
			     reader#peek_char
			       (fun gt ->
				  if gt = Utf8_lit.gt then
				    reader#read_char
				      (fun gt ->
					 k (Tok_pi (target,(Buffer.contents b))))
				  else
				    (Buffer.add_string b (Utf8_char.to_bytes q);
				     helper()))))
		 in
		   helper()
	       else
		 read_n reader 2
		   (fun qgt ->
		      if qgt = Utf8_lit.quesgt then
			k (Tok_pi (target,""))
		      else
			raise (Lex_error "malformed processing instruction")))))

let finish_parse_cdata (reader:#Cps_reader.t) (k:(xml_token->unit)) =
  read_n reader 7
    (fun opener ->
       if not (opener = Utf8_lit.openbr_cdata_openbr) then
	 raise (Lex_error "malformed CDATA section");
       let b = Buffer.create 32 in
       let rec helper () =
	 reader#read_while
	   (fun c ->
	      if not (U_char.is_char (Utf8_char.to_U_char c)) then
		raise (Lex_error "invalid character");
	      not (c = Utf8_lit.closebr))
	   (fun str ->
	      Buffer.add_string b str;
	      reader#read_while
		(fun something ->
		   if not (U_char.is_char (Utf8_char.to_U_char something)) then
		     raise (Lex_error "invalid character");
		   (something = Utf8_lit.closebr))
		(fun closebrs ->
		   assert ((String.length closebrs) > 0);
		   if (String.length closebrs) >= 2 then
		     reader#peek_char
		       (fun something ->
			  if (something = Utf8_lit.gt) then
			    reader#read_char
			      (fun gt ->
				 assert (gt = Utf8_lit.gt);
				 if (String.length closebrs) > 2 then
				   Buffer.add_string b (String.make ((String.length closebrs) - 2) ']');
				 k (Tok_cdata (Buffer.contents b)))
			  else
			    (Buffer.add_string b closebrs;
			     helper()))
		   else
		     (Buffer.add_string b closebrs;
		      helper()))) in
	 helper())

let next_xml_token (reader:#Cps_reader.t) (k:(xml_token->unit)) =
  skip_ws reader
    (fun () ->
       determine_next_token_kind reader
       (function
	    Tok_kind_text -> read_chardata reader (fun data -> k (Tok_text(data)))
	  | Tok_kind_start_tag -> finish_parse_start_tag reader k
	  | Tok_kind_end_tag -> finish_parse_end_tag reader k
	  | Tok_kind_comment -> finish_parse_comment reader k
	  | Tok_kind_pi -> finish_parse_pi reader k
	  | Tok_kind_cdata -> finish_parse_cdata reader k))
