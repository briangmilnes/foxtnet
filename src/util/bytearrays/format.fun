(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Format

		iii.	RCS Log
	
$Log: format.fun,v $
Revision 1.15  1996/03/12  22:27:47  esb
adapted to new FOXWORD.

Revision 1.14  1996/01/19  23:08:14  esb
adapted to the new wordarray signature.

Revision 1.13  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.12  1995/11/12  16:42:11  esb
adapted to new Word_Array.

Revision 1.11  1995/10/17  21:44:58  esb
changed the default headers.

Revision 1.10  1995/09/18  19:31:55  esb
changed to use wordarrays instead of bytearrays.

Revision 1.9  1995/03/10  03:51:29  esb
adapted to new vendor.sig.

Revision 1.8  1995/02/04  21:47:07  robby
updated to 107

Revision 1.7  1994/11/07  21:37:46  cline
use V.String

Revision 1.6  1994/08/26  21:58:09  robby
added format_bytearray and makestring_bytearray back in for the old protocols.

Revision 1.5  1994/08/02  19:32:28  esb
uncurried format_bytearray and renamed it bytearray.

Revision 1.4  1994/07/01  02:39:05  danwang
Removed bytearray type.

Revision 1.4  1994/07/01  02:36:25  danwang
Moved control structures into Fox_Basis.

Revision 1.3  1993/09/17  16:46:23  milnes
Changed default parameters and simplified the name.

Revision 1.2  1993/09/13  22:07:53  cline
deleted '#'s from RCS log

Revision 1.1  1993/09/02  15:40:05  esb
Initial revision


	1.	functor Format
*)

functor Format (structure V: VENDOR): FORMAT = 
 struct
  fun local_print s = V.Print.print ("format.fun: " ^ s ^ "\n")

  datatype base = Hex | Binary | Decimal
	    
  datatype format = String of string
	          | Bytes of int
		  | Function of (Word_Array.T -> string) * int

  exception Bad_Format

  fun default_header Hex = "x"
    | default_header Binary = "b"
    | default_header Decimal = ""

  val byte_base = ref Hex
  val byte_header = ref default_header
  val byte_separator = ref #"."
  val char_delimiter = ref #"\""

  fun reset () = (byte_base := Hex;
		  byte_header := default_header;
		  byte_separator := #".";
		  char_delimiter := #"\"")

  local

   val length8 = Word.toInt o Word_Array.W8.U_Big.F.length
   val next8 = Word_Array.W8.U_Big.F.next
   val seek8 = Word_Array.W8.U_Big.F.seek
   val to8 = Word_Array.to8
   val from8 = Word_Array.from8

   fun byte_in_hex byte =
	(! byte_header Hex) ^ (Word8.fmt StringCvt.HEX byte)

   fun byte_in_decimal byte =
        (! byte_header Decimal) ^ (Word8.fmt StringCvt.DEC byte)

   fun byte_in_binary byte =
        (! byte_header Binary) ^ (Word8.fmt StringCvt.BIN byte)

   fun formatted_byte byte =
        case (! byte_base) of
           Hex => byte_in_hex byte
         | Decimal => byte_in_decimal byte
         | Binary => byte_in_binary byte
		
   fun formatted_bytes (0, p) = ""
     | formatted_bytes (_, NONE) = raise Bad_Format
     | formatted_bytes (1, SOME (head, _)) = formatted_byte head
     | formatted_bytes (count, SOME (head, rest)) =
        (formatted_byte head ^ String.str (! byte_separator) ^
	 formatted_bytes (count - 1, next8 rest))

   fun check_range (count, bytes) =
        let val length = length8 (to8 bytes)
        in if count > length orelse count < 0 then
	    (local_print ("check_range (" ^ Integer.toString count ^ ", " ^
			  Integer.toString length ^
			  "), raising Bad_Format");
	     raise Bad_Format)
	   else ()
	end

   fun format_element (String s, p) = (s, p)
     | format_element (Bytes count, p) =
        (check_range (count, p);
	 (formatted_bytes (count, next8 (to8 p)),
	  from8 (seek8 (to8 p, Word.fromInt count))))
     | format_element (Function (f, count), p) =
	(check_range (count, p);
	 (f p, from8 (seek8 (to8 p, Word.fromInt count))))
  in (* local *)

   fun wordarray (list, array) =
        let fun accumulator_format ([], _, acc) = rev acc
	      | accumulator_format (format :: formats, b, acc) =
	         let val (s, new) = format_element (format, b)
	         in accumulator_format (formats, new, (s :: acc))
	         end
        in accumulator_format (list, array, [])
        end

   fun format_wordarray l p =
    let fun accumulator_format [] _ acc = rev acc
	  | accumulator_format (f :: fs) b acc =
	    let val (s, new) = format_element (f, b)
	    in accumulator_format fs new (s :: acc)
	    end
    in accumulator_format l p []
    end

   fun makestring b =
        hd (wordarray ([Bytes (length8 (to8 b))], b))

  end (* local *)

  val makestring_wordarray = makestring

 end (* struct *)
