type txt = string
type cdata = string
type qname =
    QName of txt*txt*txt                               
type att =
    Att of qname*txt ref
type content =
    Text of txt
  | CDATA of cdata
  | Comment of txt
  | PI of txt*txt                                      (** [(target,value)] *)
  | Subele of ele
and ele =
    Element of qname*att list ref*content list ref     (** [(name,atts,children)] *)

(** A document is a list of comments and PIs, and the document element. *)
type doc = content list ref*ele ref

(* constructors, accessors and mutators *)

let qname_pfx (QName(pfx,local,nsURI)) = pfx
let qname_local (QName(pfx,local,nsURI)) = local
let qname_nsURI (QName(pfx,local,nsURI)) = nsURI

let qname_eq (QName(pfx,local,_)) (QName(pfx',local',_)) =
  pfx = pfx' && local = local'

let qname_eq' (QName(_,local,nsURI)) (QName(_,local',nsURI')) =
  local = local' && nsURI = nsURI'

let att_name ((Att(name,value)):att) = name
let att_value ((Att(name,value)):att) = !value

let find_att name atts =
  List.find (fun (Att(name',_)) -> qname_eq name name') atts

let find_att' name atts =
  List.find (fun (Att(name',_)) -> qname_eq' name name') atts

let make_ele name = (Element(name,ref [], ref[]))

let ele_name (Element(name,atts,children)) = name

let ele_atts (Element(name,atts,children)) = !atts

let set_ele_atts (Element(name,atts,children)) a = atts := a

let ele_children (Element(name,atts,children)) = !children

let set_ele_children (Element(name,atts,children)) c = children := c

let ele_prepend_child (Element(name,atts,children)) c = children := (c :: !children)
let ele_append_child (Element(name,atts,children)) c = children := (!children @ (c :: []))

let ele_set_att ((Element(ele_name,atts,children)) as the_ele) name value =
  try 
    (let (Att(name',value')) = find_att name !atts in
       value' := value)
  with
      Not_found -> atts := (Att(name,ref value) :: !atts)

let ele_get_att (Element(ele_name,atts,children)) name =
  match (find_att name !atts) with
       Att(name,value) -> !value

let ele_get_att' (Element(ele_name,atts,children)) name =
  match (find_att' name !atts) with
      Att(name,value) -> !value

let ele_remove_att ((Element(ele_name,atts,children)) as the_ele) name =
  atts := List.filter (fun (Att(name',_)) -> not (qname_eq name name')) !atts

let ele_remove_att' ((Element(ele_name,atts,children)) as the_ele) name =
  atts := List.filter (fun (Att(name',_)) -> not (qname_eq' name name')) !atts

let ele_inner_text (Element(_,_,children)) =
  let rec helper children =
    (match children with
	 Text(s) :: rest -> s
       | _ :: rest -> helper rest
       | [] -> "") in
    helper !children

let doc_prolog ((prolog,ele):doc) = !prolog

let doc_ele ((prolog,ele):doc) = !ele

let clone_qname (QName(pfx,local,nsURI)) =
  QName(String.copy pfx,String.copy local,String.copy nsURI)

let clone_att (Att(name,value)) =
  Att(clone_qname name,ref !value)

let rec
  clone_ele (Element(name,atts,children)) =
  Element(clone_qname name,
	  ref (List.map clone_att !atts),
	  ref (List.map clone_content !children)) and
  clone_content = function
      Subele(ele) -> Subele(clone_ele ele)
    | Text(s) -> Text(String.copy s)
    | CDATA(s) -> CDATA(String.copy s)
    | Comment(s) -> Comment(String.copy s)
    | PI(t,v) -> PI(String.copy t,String.copy v)

(* serializers *)
(* TODO: make these output to a Buffer.t for efficiency *)

let string_of_qname (QName(pfx,local,nsURI)) = 
  if (String.length pfx) > 0 then
    pfx ^ ":" ^ local
  else
    local
  
let sp b n = for i = 1 to n do Buffer.add_char b ' ' done
let rec serialize_ele ?(indent=0) b (Element (qname,atts,children)) =
  sp b indent;
   Buffer.add_char b '<';
   Buffer.add_string b (string_of_qname qname);
   List.iter
        (fun (Att (attname, attvalue)) ->
           (Buffer.add_char b ' ';
            Buffer.add_string b (string_of_qname attname);
            Buffer.add_string b "=\"";
            Buffer.add_string b !attvalue;
            Buffer.add_char b '"'))
		!atts;
   (match !children with
    | [] -> Buffer.add_string b "/>\n"
    | [ Text txt ] ->
        (Buffer.add_char b '>';
         Buffer.add_string b txt;
         Buffer.add_string b "</";
         Buffer.add_string b (string_of_qname qname);
         Buffer.add_string b ">\n")
    | children ->
        (Buffer.add_string b ">\n";
		 List.iter (serialize_content (if indent>=0 then indent+1 else indent) b) children;
         sp b indent;
         Buffer.add_string b "</";
         Buffer.add_string b (string_of_qname qname);
         Buffer.add_string b ">\n"))
and serialize_content indent b = function
 | Subele el -> serialize_ele ~indent:indent b el
 | Text s -> (sp b indent; Buffer.add_string b s; Buffer.add_char b '\n') 
 | CDATA s ->
              (sp b indent;
               Buffer.add_string b "<![CDATA[";
               Buffer.add_string b s;
               Buffer.add_string b "]]>\n")
 | Comment s -> (sp b indent;
                 Buffer.add_string b "<!--";
				 Buffer.add_string b s;
				 Buffer.add_string b "-->\n")
 | PI (t,v) -> (sp b indent;
                Buffer.add_string b "<?";
				Buffer.add_string b t;
				if String.length v > 0 then
				  (Buffer.add_char b ' ';
				   Buffer.add_string b v);
				Buffer.add_string b "?>\n")

let serialize_doc ?(indent=0) b (prolog,doc_ele) =
 List.iter (serialize_content indent b) !prolog;
 serialize_ele ~indent:indent b !doc_ele
 
let string_of_ele ?indent ele =
 let b = Buffer.create 128 in
 serialize_ele ?indent b ele;
 Buffer.contents b
 
let string_of_doc ?indent doc =
 let b = Buffer.create 128 in
 serialize_doc ?indent b doc;
 Buffer.contents b

(* Namespace processing utilities *)

exception Namespace_error

let is_xmlns_prefix_decl yaxpoatt =
  yaxpoatt.Pull.att_name.Pull.pfx = "xmlns"

let is_xmlns_default_decl yaxpoatt =
  yaxpoatt.Pull.att_name.Pull.pfx = "" && yaxpoatt.Pull.att_name.Pull.local = "xmlns"

let is_xmlns_decl yaxpoatt =
  is_xmlns_prefix_decl yaxpoatt || is_xmlns_default_decl yaxpoatt

let do_lookup pred decls =
  let rslt = ref (None:Pull.att option) in
    Stack.iter
      (fun some_decls ->
	 (match !rslt with
	      None ->
		(try
		   rslt := Some(List.find pred some_decls)
		 with
		     Not_found -> ())
	    | _ -> ()))
      decls;
    (match !rslt with
	 None -> raise Namespace_error
       | Some(the_att) -> the_att.Pull.att_value)

let lookup_prefix a_pfx decls =
  do_lookup
    (fun an_att ->
       an_att.Pull.att_name.Pull.local = a_pfx)
    decls

let lookup_default decls =
  try
    do_lookup is_xmlns_default_decl decls
  with
      Namespace_error -> ""

let qname_of_Yaxpo name uri =
  QName(name.Pull.pfx,name.Pull.local,uri)

let resolve_yaxpo_qname name decls allow_default =
  qname_of_Yaxpo name
    (match name.Pull.pfx with
	 "" -> if allow_default then lookup_default decls else ""
       | _ as a_pfx -> lookup_prefix a_pfx decls)

let att_of_Yaxpo att decls =
  Att((if is_xmlns_decl att then 
	 qname_of_Yaxpo att.Pull.att_name ""
       else
	 resolve_yaxpo_qname att.Pull.att_name decls false;),
      ref att.Pull.att_value)

let rec check_atts_uniqueness = function
    [] -> ()
  | (Att(QName(pfx1,local1,uri1),_)) :: rest ->
      List.iter
      (fun (Att(QName(pfx2,local2,uri2),_)) ->
	if local1 = local2 && uri1 = uri2 then
	  raise (Pull.Parse_error ("Attribute specified multiple times",None)))
      rest;
      check_atts_uniqueness rest

let proc_start_tag name atts decls =
  let some_decls = List.filter is_xmlns_decl atts in
    Stack.push some_decls decls;
    let new_ele = make_ele (resolve_yaxpo_qname name decls true) in
    let new_atts = (List.map (fun an_att -> att_of_Yaxpo an_att decls) atts) in
    check_atts_uniqueness new_atts;
    set_ele_atts new_ele new_atts;
    new_ele

let rec continue_build_element_tree (reader:#Cps_reader.t) (name:Pull.qname) (the_ele:ele) (decls: Pull.att list Stack.t) (k:ele->unit) =
  let rec helper () =
    Pull.next_xml_token reader
      (function
	   Pull.Tok_text(s) -> ele_prepend_child the_ele (Text s); helper ()
	 | Pull.Tok_cdata(s) -> ele_prepend_child the_ele (CDATA s); helper ()
	 | Pull.Tok_start_tag(child_name,atts) ->
	     let child = proc_start_tag child_name atts decls in
	       continue_build_element_tree reader child_name child decls
		 (fun finished ->
		    assert (child = finished);
		    ele_prepend_child the_ele (Subele finished); helper ())
	 | Pull.Tok_empty_ele(child_name,atts) ->
	     let child = proc_start_tag child_name atts decls in
	     ignore (Stack.pop decls);
	     ele_prepend_child the_ele (Subele child);
	     helper ()
	 | Pull.Tok_end_tag(end_name) as tok ->
	     if name = end_name then
	       (set_ele_children the_ele (List.rev (ele_children the_ele));
		ignore (Stack.pop decls);
		k the_ele)
	     else
	       raise (Pull.Parse_error ("Unexpected end tag",Some tok))
	 | Pull.Tok_comment(s) ->
	     ele_prepend_child the_ele (Comment s); helper ()
	 | Pull.Tok_pi(t,v) ->
	     ele_prepend_child the_ele (PI (t,v)); helper ()) in
    helper ()
	     

let parse_element_cps ?(namespace_declarations=Stack.create()) reader k =
  let decls = namespace_declarations in
  Pull.next_xml_token reader
    (function
	 Pull.Tok_start_tag(name,atts) ->
	   let the_ele = proc_start_tag name atts decls in
	     continue_build_element_tree reader name the_ele decls
	       (fun finished ->
		  assert (finished = the_ele);
		  k finished)
       | Pull.Tok_empty_ele(name,atts) ->
	   let the_ele = proc_start_tag name atts decls in
	   ignore (Stack.pop decls);
	   k the_ele
       | _ as tok -> raise (Pull.Parse_error ("unexpected token; expected start tag",Some tok)))

let parse_element ?namespace_declarations reader =
  let rslt = ref None in
    parse_element_cps ?namespace_declarations reader
      (fun the_ele -> rslt := Some(the_ele));
    (match !rslt with
	 Some(e) -> e
       | _ -> failwith "Reader paused parsing, but parser is not CPS")

let parse_doc_cps reader k = 
  let prolog = ref [] in
  let decls = Stack.create () in
  let rec helper () =
    Pull.next_xml_token reader
      (function
	   Pull.Tok_pi(t,v) -> prolog := (PI (t,v)) :: !prolog; helper ()
	 | Pull.Tok_comment(s) -> prolog := (Comment s) :: !prolog; helper ()
	 | Pull.Tok_start_tag(name,atts) ->
	     let the_ele = proc_start_tag name atts decls in
	       continue_build_element_tree reader name the_ele decls
		 (fun finished ->
		    assert (finished = the_ele);
		    prolog := (List.rev !prolog);
		    k ((prolog, ref finished):doc))
	 | Pull.Tok_empty_ele(child_name,atts) ->
	     let the_ele = proc_start_tag child_name atts decls in
	       prolog := (List.rev !prolog);
	       k ((prolog, ref the_ele):doc)
	 | _ as tok-> raise (Pull.Parse_error ("unexpected token",Some tok))) in
    helper ()

let parse_doc reader =
  let rslt = ref None in
    parse_doc_cps reader
      (fun the_doc ->
	 rslt := Some(the_doc));
    (match !rslt with
	 Some(the_doc) -> the_doc
       | _ -> failwith "Reader paused parsing, but parser is not CPS")

module Namespace_utils =
struct
  let is_xmlns_prefix_decl = function
      Att(QName("xmlns",_,""),_) -> true
    | _ -> false

  let is_xmlns_default_decl = function
      Att(QName("","xmlns",""),_) -> true
    | _ -> false

  let is_xmlns_decl att =
    (is_xmlns_prefix_decl att) || (is_xmlns_default_decl att)

  let xmlns_decl_pfx att =
    if is_xmlns_prefix_decl att then
      (qname_local (att_name att))
    else if is_xmlns_default_decl att then
      ""
    else
      assert false

  let xmlns_decl_uri att =
    att_value att

  let rec lookup prefix decls =
    let rec lookup_in decls =
      (att_value
	 (List.find
	    (function
		 (Att(QName("","xmlns",_),value)) when prefix = "" -> true
	       | (Att(QName("xmlns",prefix',""),_)) when prefix = prefix' -> true
	       | _ -> false)
	    decls)) in
      (match decls with
	   [] -> raise Not_found
	 | these :: rest -> (try lookup_in these with Not_found -> lookup prefix rest))
     
  let out = function
      Some(x) -> x
    | None -> assert false

  let rec qualify_namespaces ele decls =
    let to_att (pfx,nsURI) =
      if pfx = "" then
	Att(QName("","xmlns",""),ref nsURI)
      else
	Att(QName("xmlns",pfx,""),ref nsURI) in
    let from_name name decls =
      (match name with
	   QName(pfx,_,nsURI) ->
	     (try
		if lookup pfx decls = nsURI then
		  None
		else
		  Some(pfx,nsURI)
	      with
		  Not_found -> Some(pfx,nsURI))) in
    let rec from_atts atts decls =
      (match atts with
	   [] -> []
	 | hd :: tl when is_xmlns_decl hd -> from_atts tl decls
	 | Att(name,value) :: tl ->
	     if (qname_pfx name) = "" then
	       from_atts tl decls
	     else
	       let decl = from_name name decls in
		 if decl = None then
		   from_atts tl decls
		 else
		   let decl' = out decl in
		     decl' :: (List.filter (fun x -> not (x = decl'))
				 (from_atts tl decls))) in
    let rec check_consistency = function
	[] -> ()
      | a_decl :: tl ->
	  List.iter
	  (fun another_decl ->
	     if (xmlns_decl_pfx a_decl) = (xmlns_decl_pfx another_decl) &&
	       not ((xmlns_decl_uri a_decl) = (xmlns_decl_uri another_decl)) then
		 failwith "Self-contradictory nsURIs") tl in
    let internal_decls = List.filter is_xmlns_decl (ele_atts ele) in
    let name_decl = (from_name (ele_name ele) (internal_decls :: decls)) in
    let needed_decls = if name_decl = None then [] else [ to_att (out name_decl) ] in
    let needed_decls' =
      needed_decls @ (List.map to_att (from_atts (ele_atts ele) (needed_decls :: internal_decls :: decls))) in
      check_consistency (needed_decls' @ internal_decls);
      List.iter (fun (Att(name,value)) -> ele_set_att ele name !value) needed_decls';
      List.iter (function Subele(e) -> qualify_namespaces e (needed_decls' :: decls) | _ -> ())
	(ele_children ele)
end
