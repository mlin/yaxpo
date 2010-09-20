(*
cps_reader

This is the interface for a reader object that the parser expects. a string reader
is provided later. The programmer can implement her own reader that might read the
XML from a file, or a socket, or whatever.

The reader methods are invoked through continuation-passing style. This means that
instead of being expected to return a value to its caller, it is passed a "continuation"
(a procedure) to which it should pass the result in a tail-call.

Note that the continuations for peek_char, read_char, etc., accept a utf8char = string
and not a char. This is in order to handle multibyte UTF-8 encoded characters; they are
expected to pass the encoded version of the character.
*)

class type t =
object
  (* peek at the next char without removing it *)
  method peek_char : (Utf8_char.t -> unit) -> unit
  (* read the next char *)
  method read_char : (Utf8_char.t -> unit) -> unit
  (* read while pred returns true for each character *)
  method read_while : (Utf8_char.t -> bool) -> (string -> unit) -> unit
end

(*
cps_string_reader
an implementation of cps_reader that returns data from a single-byte character string.
reading this simple one is probably a good way to understand how to write CPS code.
*)

class cps_string_reader (str) =
object (self)
  val mutable idx = 0
  method peek_char (k:(Utf8_char.t -> unit)) =
    k (Utf8_char.of_char (String.get str idx))
  method read_char (k:(Utf8_char.t -> unit)) =
    let c = String.get str idx in
      idx <- idx + 1;
      k (Utf8_char.of_char c)
  method read_while pred (k:(string -> unit)) =
    let start = idx in
      while pred (Utf8_char.of_char (String.get str idx)) do
	idx <- idx + 1
      done;
      k (String.sub str start (idx - start))
end

let make_string_reader str =
  let r = new cps_string_reader(str) in
    (r:t)

(*
cps_utf8string_reader
this variant of the string reader is UTF-8 aware.
*)

class cps_utf8_string_reader (str) =
object
  val mutable idx = 0
  method peek_char (k:(Utf8_char.t->unit)) =
    let firstb = String.get str idx in
    let len = Utf8_char.utf8_character_size (Char.code firstb) in
      k (Utf8_char.of_bytes (String.sub str idx len))
  method read_char (k:(Utf8_char.t->unit)) =
    let firstb = String.get str idx in
    let len = Utf8_char.utf8_character_size (Char.code firstb) in
    let chr = String.sub str idx len in
      idx <- idx + len;
      k (Utf8_char.of_bytes chr)
  method read_while (pred:(Utf8_char.t->bool)) (k:(string->unit)) =
    let start = idx in
    let rec helper () =
      let firstb = String.get str idx in
      let len = Utf8_char.utf8_character_size (Char.code firstb) in
	if pred (Utf8_char.of_bytes (String.sub str idx len)) then
	  (idx <- idx + len;
	   helper ())
	else
	  k (String.sub str start (idx - start))
    in
      helper ()
end

let make_utf8_string_reader str =
  let r = new cps_utf8_string_reader(str) in
    (r:t)

(* really stupid file reader *)
class cps_utf8_file_reader (fname) =
object(self)
  val infile = open_in fname
  val nextc = ref None

  initializer
    Gc.finalise close_in infile

  method peek_char (k:(Utf8_char.t->unit)) =
    match !nextc with
	Some(c) -> k c
      | None ->
	  let firstb = input_char infile in
	  let len = Utf8_char.utf8_character_size (Char.code firstb) in
	  let chr = String.make len firstb in
	    if len > 1 then
	      really_input infile chr 1 (len - 1);
	    let c = Utf8_char.of_bytes chr in
	      nextc := Some(c);
	      k c

  method read_char (k:(Utf8_char.t->unit)) =
    (match !nextc with
	 Some(c) -> let chr = c in nextc := None; k c
       | None -> self#peek_char (fun chr ->
				   nextc := None;
				   k chr))
   
  method read_while pred k =
    let b = Buffer.create 64 in
    let rec helper () =
      self#peek_char
	(fun chr ->
	   if pred chr then
	     (Buffer.add_string b (Utf8_char.to_bytes chr);
	      self#read_char
		(fun chr' ->
		   assert (chr = chr');
		   helper ()))
	   else
	     k (Buffer.contents b)) in
      helper ()
end
  
let make_utf8_file_reader fname =
  let r = new cps_utf8_file_reader(fname) in
    (r:t)

(* TODO it would be nice to provide a wrapper will keeps track of the line & column number *)
