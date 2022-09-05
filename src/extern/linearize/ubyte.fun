(*

	FoxNet:  The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This is three structures which marshall and unmarshall
	to and from the types:  int8, int16 and int32.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Ubyte1
	2.	functor Ubyte2
	3.	functor Ubyte4

		iii.	RCS Log
	
$Log: ubyte.fun,v $
Revision 1.10  1996/01/19  23:09:46  esb
adapted to the new wordarray signature.

Revision 1.9  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.8  1995/06/27  18:58:30  cline
adapted to new extern.sig

Revision 1.7  1995/06/20  17:46:52  esb
minor fix.

Revision 1.6  1995/03/10  03:53:53  esb
adapted to new vendor.sig.

Revision 1.5  1994/11/09  20:49:09  esb
adapted to new extern.sig.

Revision 1.4  1994/09/30  16:55:23  esb
replaced ubytes by fox words.

Revision 1.3  1994/09/12  18: 30: 37  milnes
Added prints to handle _'s.

Revision 1.2  1994/08/25  23: 47: 43  robby
updated for new basis

Revision 1.1  1994/07/14  20: 28: 50  robby
Initial revision

Revision 1.1  94/07/13  18: 49: 14  robby
Initial revision


		1.	functor Ubyte1
*)

functor Ubyte1 (): UBYTE1_LINEARIZE = 
 struct
  type T = Word8.word
  type extern_in = Word8Array.array
  type extern_out = Word8Array.array
  type cursor = Word.word

  exception Extern

  fun size _ = 0w1

  fun marshal (array, value) pos = 
	(Word8Array.update (array, Word.toInt pos, value);
	 pos + 0w1)
	handle Subscript => raise Extern
    
  fun unmarshal (array, pos) = 
       let val result = ((Word8Array.sub (array, Word.toInt pos))
	                 handle Subscript => raise Extern)
       in (result, pos + 0w1)
       end
			      
 end

(*
	2.	functor Ubyte2
*)

functor Ubyte2 (val swap: bool
		structure Order: BYTE_ORDER
		sharing type Order.T = Word16.word): UBYTE2_LINEARIZE = 
 struct
  type T = Word16.word
  type extern_in = Word8Array.array
  type extern_out = Word8Array.array
  type cursor = Word.word

  exception Extern

  fun size _ = 0w2

  val swap_bytes = if swap then Order.invert else (fn x => x)

  fun marshal (array, value) pos = 
       (Unaligned.Byte2u.update (array, Word.toInt pos, 
				 Unaligned.Byte2u.unalign (swap_bytes value));
	pos + 0w2)
         handle Subscript => raise Extern
    
  fun unmarshal (array, pos) = 
       let val result = ((swap_bytes (Unaligned.Byte2u.align 
				      (Unaligned.Byte2u.sub (array,
							     Word.toInt pos))))
			 handle Subscript => raise Extern)
       in (result, pos + 0w2)
       end
 end

(*
	3.	functor Ubyte4
*)

functor Ubyte4 (val swap: bool
		structure V: VENDOR
		structure Order: BYTE_ORDER
		sharing type Order.T = Word32.word): UBYTE4_LINEARIZE = 
 struct
  fun local_print s = V.Print.print ("ubyte.fun: " ^ s ^ "\n")

  type T = Word32.word
  type extern_in = Word8Array.array
  type extern_out = Word8Array.array
  type cursor = Word.word

  exception Extern

  fun size _ = 0w4

  val swap_bytes = if swap then Order.invert else (fn x =>x)

  fun marshal (array, value) pos = 
       (Unaligned.Byte4u.update (array, Word.toInt pos, 
				 Unaligned.Byte4u.unalign (swap_bytes value));
	pos + 0w4)
         handle x => 
	         (local_print ("exception " ^ V.Control.exnName x ^ 
			       "raised in marshal, raising Extern.");
		  raise Extern)
    
  fun unmarshal (array, pos) = 
       let val result = ((swap_bytes (Unaligned.Byte4u.align
				      (Unaligned.Byte4u.sub (array,
							     Word.toInt pos))))
			 handle x =>
			         (local_print ("in unmarshal, exception " ^
					       V.Control.exnName x ^ 
					       " raising Extern.");
				  raise Extern))
       in (result, pos + 0w4)
       end
 end
