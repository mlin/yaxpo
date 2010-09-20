type xml =
	| XElement of string*((string*string) list)*(xml list)
	| XText of string
	| XCDATA of string
	| XComment of string
	| XPI of string*string

let unparse_qname (Tree.QName (pfx,local,nsURI)) =
	if pfx = "" then
		local
	else
		(pfx ^ ":" ^ local)

let rec xml_of_tree_ele = function
	| Tree.Element (qn,ratts,rcont) -> begin
		let atts = List.map (fun (Tree.Att (qn,rval)) -> (unparse_qname qn,!rval)) !ratts in
		XElement (unparse_qname qn,atts,List.map of_tree_content !rcont) end
and of_tree_content = function
	| Tree.Subele ele -> (xml_of_tree_ele ele)
	| Tree.Text txt -> (XText txt)
	| Tree.CDATA cdata -> (XCDATA cdata)
	| Tree.Comment txt -> (XComment txt)
	| Tree.PI (k,v) -> (XPI (k,v))

let rec xml_to_tree_ele = function
	| XElement (nm,atts,conts) -> begin
		let domatts = List.map (fun (k,v) -> Tree.Att (Tree.QName ("",k,""),ref v)) atts in
		Tree.Element (Tree.QName ("",nm,""), ref domatts, ref (List.map to_tree_content conts)) end
	| _ -> invalid_arg "Yaxpo.Lite.xml_to_tree_ele: not an element"
and to_tree_content = function
	| (XElement _ as ele) -> Tree.Subele (xml_to_tree_ele ele)
	| XText txt -> Tree.Text txt
	| XCDATA cdata -> Tree.CDATA cdata
	| XComment txt -> Tree.Comment txt
	| XPI (k,v) -> Tree.PI (k,v)
