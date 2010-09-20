type t = string
exception Bad_encoding
  
(* given the first byte of a utf8-encoded character, determines how many bytes in total
   that character should take. *)
let utf8_character_size firstb =
  if (firstb land 0b10000000) = 0 then 1
  else if (firstb land 0b11100000) = 0b11000000 then 2
  else if (firstb land 0b11110000) = 0b11100000 then 3
  else if (firstb land 0b11111000) = 0b11110000 then 4
  else if (firstb land 0b11111100) = 0b11111000 then 5
  else if (firstb land 0b11111110) = 0b11111100 then 6
  else raise Bad_encoding
    
(* decodes the next utf8 character from a stream of bytes. *)
let utf8_decode_character_bytestream (get_nextbyte:((char->unit)->unit)) =
  let
    chr = ref (Int32.of_int 0) in
    (* reads n 10xxxxxx tails, lshift'ing/or'ing the read values into chr *)
  let rec read_tails n =
    if n > 0 then
      get_nextbyte
	(fun b ->
	   let nextb = Char.code b in
	     if (nextb land 0b11000000) = 0b10000000 then
	       let chunk = nextb land 0b00111111 in
		 chr := Int32.logor (Int32.shift_left !chr 6) (Int32.of_int chunk);
		 read_tails (n - 1)
	     else
	       raise Bad_encoding)
  in
    get_nextbyte
      (fun b ->
	 let firstb = Char.code b in
	   (* dispatch on the first byte to see how many tails we should read *)
	   match utf8_character_size firstb with
	       1 -> chr := Int32.of_int firstb
	     | 2 -> (chr := (Int32.of_int (firstb land 0b00011111));
		     read_tails 1)
	     | 3 -> (chr := (Int32.of_int (firstb land 0b00001111));
		     read_tails 2)
	     | 4 -> (chr := (Int32.of_int (firstb land 0b00000111));
		       read_tails 3)
	     | 5 -> (chr := (Int32.of_int (firstb land 0b00000011));
		     read_tails 4)
	     | 6 -> (chr := (Int32.of_int (firstb land 0b00000001));
		     read_tails 5)
	     | _ -> raise Bad_encoding);
    U_char.of_int32 !chr
      
let utf8_decode_character (c:t) =
  let
    len = String.length c and
    idx = ref 0 in
    utf8_decode_character_bytestream
      (fun k -> 
	 if !idx >= len then
	   raise Bad_encoding;
	 let chr = String.get c !idx in
	   idx := !idx + 1;
	   k chr)

let to_bytes c = (c:string)
let of_bytes s = s      
let of_char c =
  if (Char.code c) > 127 then raise Bad_encoding;
  String.make 1 c
let to_U_char (c:t) = utf8_decode_character c
let size c = String.length (to_bytes c)
