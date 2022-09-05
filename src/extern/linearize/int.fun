(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
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

	This provides the ability to marshall and unmarshall
	signed four byte integers.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Int

		iii.	RCS Log
	
$Log: int.fun,v $
Revision 1.7  1996/01/19  23:09:46  esb
adapted to the new wordarray signature.

Revision 1.6  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.5  1995/06/27  18:57:53  cline
adapted to new extern.sig

Revision 1.4  1994/11/09  20:49:09  esb
adapted to new extern.sig.

Revision 1.3  1994/09/30  16:55:23  esb
replaced ubytes by fox words.

Revision 1.2  1994/08/16  13: 03: 43  robby
added an exception handle

Revision 1.1  94/07/14  20: 28: 33  robby
Initial revision

Revision 1.1  94/07/13  18: 44: 20  robby
Initial revision


		1.	functor Int
*)

functor Int (structure Ubyte4: UBYTE4_LINEARIZE): INT_LINEARIZE = 
 struct
  signature EXCEPTION_CAPTURE = sig exception Extern end
  structure X: EXCEPTION_CAPTURE = Ubyte4
  open X

  type T = int
  type extern_in = Word8Array.array
  type extern_out = Word8Array.array
  type cursor = Word.word

  fun size _ = 0w4

  fun marshal (array, i) = Ubyte4.marshal (array, Word32.fromInt i)

  fun unmarshal (array, pos) =
       let val (unsigned, new_pos) = Ubyte4.unmarshal (array, pos)
       in (Word32.toInt unsigned, new_pos)
       end

end

