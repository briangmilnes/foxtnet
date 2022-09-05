(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

        WORD signature for Fox (probably temporary).


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature FOXWORD

		iii.	RCS Log
	
$Log: word.sig,v $
Revision 1.7  1996/12/18  22:19:00  esb
added fmt, scan, and wordSize, which are now standard.

Revision 1.6  1996/03/22  22:49:34  necula
Added vector access functions. Fixed a bug in sub_little.

Revision 1.5  1996/03/12  22:21:13  esb
removed signature FOXWORD_OPERATIONS, tried to optimizie Word48.sub/update.

Revision 1.4  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.3  1995/04/05  15:09:15  esb
added update_big, update_little, sub_big, sub_little.

Revision 1.2  1994/09/30  17:16:48  esb
added foxnet header.


		1.	signature FOXWORD
*)

signature FOXWORD =
 sig
  eqtype word
  val + : word * word -> word
  val - : word * word -> word
  val * : word * word -> word
  val div: word * word -> word
  val mod: word * word -> word
  val andb: word * word -> word
  val orb: word * word -> word
  val xorb: word * word -> word
  val notb: word -> word
  val min: word * word -> word
  val max: word * word -> word
  val << : word * Word.word -> word
  val >> : word * Word.word -> word
  val ~>> : word * Word.word -> word
  val < : word * word -> bool
  val <= : word * word -> bool
  val > : word * word -> bool
  val >= : word * word -> bool

  val fromInt: int -> word
  val fromLargeWord : Word32.word -> word
  val toInt: word -> int
  val toIntX: word -> int
  val toLargeWord : word -> Word32.word
  val toLargeWordX : word -> Word32.word
  val fromString : string -> word option
  val toString : word -> string
  val fmt: StringCvt.radix -> word -> string
  val scan: StringCvt.radix
          -> (char,'a) StringCvt.reader -> (word,'a) StringCvt.reader

  val bigEndian: bool
  val wordSize: int

  val sub: Word8Array.array * int -> word
  val sub_big: Word8Array.array * int -> word
  val sub_little: Word8Array.array * int -> word

  val subv: Word8Vector.vector * int -> word
  val subv_big: Word8Vector.vector * int -> word
  val subv_little: Word8Vector.vector * int -> word

  val update: Word8Array.array * int * word -> unit
  val update_big: Word8Array.array * int * word -> unit
  val update_little: Word8Array.array * int * word -> unit
 end
