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

	This functor provides a pairing operation on
	structures that match LINEARIZE.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Pair

		iii.	RCS Log
	
$Log: pair.fun,v $
Revision 1.5  1996/01/19  23:09:46  esb
adapted to the new wordarray signature.

Revision 1.4  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.3  1995/06/27  18:58:21  cline
adapted to new extern.sig

Revision 1.2  1994/11/09  20:49:09  esb
adapted to new extern.sig.

Revision 1.1  1994/07/14  20:28:44  robby
Initial revision

Revision 1.1  94/07/13  18:49:05  robby
Initial revision


		1.	functor Pair
*)

functor Pair (structure P1: LINEARIZE
	      structure P2: LINEARIZE): PAIR_LINEARIZE =
 struct

  structure P1 = P1
  structure P2 = P2

  type T = P1.T * P2.T
  exception Extern
  type extern_in = Word8Array.array
  type extern_out = Word8Array.array
  type cursor = Word.word

  fun pair (x, y) = (x, y)
  fun first (a, b) = a
  fun second (a, b) = b

  fun size (a, b) = P1.size a + P2.size b
    
  fun marshal (array, (a, b)) cursor =
       ((P2.marshal (array, b) o P1.marshal (array, a)) cursor)
        handle P1.Extern => raise Extern | P2.Extern => raise Extern

  fun unmarshal (array, start) =
       (let val (a, mid) = P1.unmarshal (array, start)
	    val (b, stop) = P2.unmarshal (array, mid)
        in ((a, b), stop)
        end)
	 handle P1.Extern => raise Extern | P2.Extern => raise Extern

 end
