(** Defines a much simpler XML data model than the Tree module

Motivation: experience has shown that the data model defined by the [Tree] module is much more complicated (and thus clumsy) than needed for most small XML applications. The [Lite] module defines an immutable tree structure without namespace processing support. It is especially good for map/match/fold due to its simpler structuer, and for literal construction of XML data due to its cleaner syntax.

Typical usage:
{[
open Yaxpo.Lite;;
let xml = xml_of_dom_ele (Yaxpo.Tree.doc_ele (Yaxpo.Tree.parse_doc (Yaxpo.Cps_reader.make_utf8_file_reader "data.xml")));;
match xml with
...
let html =
 (XElement ("html",[],
   [
     XElement ("head",[],[XElement ("title",[],[XText "hello, world!"])]);
     XElement ("body",[],
       [
         XElement ("p",[("align", "center")],[XText "hello, world!"])
       ])
    ]))
]}
*)

type xml =
	| XElement of string*((string*string) list)*(xml list)
	| XText of string
	| XCDATA of string
	| XComment of string
	| XPI of string*string

val xml_of_tree_ele : Tree.ele -> xml
val xml_to_tree_ele : xml -> Tree.ele
