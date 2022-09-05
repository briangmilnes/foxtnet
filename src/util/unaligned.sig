(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

Signature for unaligned byteN arithmetic.  This is just UBYTES with
a type aligned and conversion functions align and unalign.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	structure unaligned

		iii.	RCS Log
	
$Log: unaligned.sig,v $
Revision 1.4  1996/03/11  20:06:57  cline
updated for 109.6

Revision 1.3  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.2  1994/09/30  16:28:13  esb
changed to use FOXWORD instead of UBYTES.

Revision 1.1  1993/11/05  20:29:21  cline
Initial revision


		1.	structure unaligned
*)
(*
signature UNALIGNED_UBYTES =
 sig
  eqtype aligned
  include FOXWORD
  include FOXWORD_OPERATIONS
  val align: word -> aligned
  val unalign: aligned -> word
 end (* sig *)
*)

signature UNALIGNED_UBYTES =
 sig
  eqtype aligned
    eqtype word
    val + : word * word -> word
    val - : word * word -> word
    val * : word * word -> word
    val div : word * word -> word
    val mod : word * word -> word
    val andb : word * word -> word
    val orb : word * word -> word
    val xorb : word * word -> word
    val notb : word -> word
    val << : word * Word.word -> word
    val >> : word * Word.word -> word
    val ~>> : word * Word.word -> word
    val < : word * word -> bool
    val <= : word * word -> bool
    val > : word * word -> bool
    val >= : word * word -> bool
    val fromInt : int -> word
    val fromLargeWord : Word32.word -> word
    val toInt : word -> int
    val toIntX : word -> int
    val toLargeWord : word -> Word32.word
    val toLargeWordX : word -> Word32.word
    val fromString : string -> word option
    val toString : word -> string
    val bigEndian : bool
    val min : word * word -> word
    val max : word * word -> word
    val sub : Word8Array.array * int -> word
    val sub_big : Word8Array.array * int -> word
    val sub_little : Word8Array.array * int -> word
    val update : Word8Array.array * int * word -> unit
    val update_big : Word8Array.array * int * word -> unit
    val update_little : Word8Array.array * int * word -> unit
  val align: word -> aligned
  val unalign: aligned -> word
 end (* sig *)
