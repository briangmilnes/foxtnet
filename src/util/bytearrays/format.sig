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
	1.	signature FORMAT
	2.	functions format_wordarray and wordarray
	3.	other functions

		iii.	RCS Log
	
$Log: format.sig,v $
Revision 1.8  1995/09/18  19:31:55  esb
changed to use wordarrays instead of bytearrays.

Revision 1.7  1995/02/04  21:47:09  robby
updated to 107

Revision 1.6  1994/08/26  21:59:12  robby
added format_bytearray and makestring_bytearray back in for the old protocols.

Revision 1.5  1994/08/02  19:32:28  esb
uncurried format_bytearray and renamed it bytearray.

Revision 1.4  1994/07/01  02:39:29  danwang
Removed bytearray type.

Revision 1.3  1993/09/17  16:46:38  milnes
Changed default parameters and simplified the name.

Revision 1.2  1993/09/13  22:07:52  cline
deleted '#'s from RCS log

Revision 1.1  1993/09/02  15:40:05  esb
Initial revision


	1.	signature FORMAT
*)

signature FORMAT =
 sig

  datatype base = Hex | Binary | Decimal

  datatype format = String of string
                  | Bytes of int
(*
		  | Chars of int
*)
		  | Function of (Word_Array.T -> string) * int

  exception Bad_Format
	    
  val byte_base: base ref
  val byte_header: (base -> string) ref
  val byte_separator: char ref
(*
  val char_delimiter: char ref
*)

(*
	2.	functions format_wordarray and wordarray

   format_wordarray fs b returns a list of strings,
   one for each format specification in `fs', obtained by traversing the
   wordarray `b', starting with the first byte. Each
   format specification converts some number of bytes into a string, as
   follows:

   - String s converts 0 bytes into the string s

   - Bytes n converts n bytes into a string containing a numeric
     representation for each byte, separated by (! byte_separator),
     represented in base (! byte_base). The representation of a byte
     in a given base is the byte header ((! byte_header) base) followed
     by the value of the byte converted into the given base.

(*
   - Chars n converts n bytes into their string equivalent, with 
     (! char_delimiter) prepended and appended.
*)

   - Function (f, n) converts n bytes into the string given by applying
     f to b.  This is intended for (for instance) converting
     multi-byte (or sub-byte) values, or for converting specific values
     into symbolic names. Note that this can also be used to change the
     values of the formatting refs (byte_base, byte_header, byte_separator,
     char_delimiter), or to ignore sequences of bytes, or even to step
     backwards through the wordarray.

   Bad_Range is raised if the format specifies leaving the range of the
   wordarray.

   The formatting refs are initially set as follows:

	val byte_base = ref Hex
	val byte_header = ref (fn Hex => "1ux"
                                | Binary => "1ub"
				| Decimal => "")
	val byte_separator = ref #"."
(*
	val char_delimiter = ref #"\""
*)

*)

  val format_wordarray: format list -> Word_Array.T -> string list
  val wordarray: format list * Word_Array.T -> string list

(*
	3.	other functions
*)

  val makestring: Word_Array.T -> string
  val makestring_wordarray: Word_Array.T -> string
  val reset: unit -> unit

 end (* sig *)
