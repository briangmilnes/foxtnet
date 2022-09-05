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

	Byte swapping functions.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Swap_Bytes

		iii.	RCS Log
	
$Log: swap.fun,v $
Revision 1.3  1995/02/11  04:23:43  esb
minor bug fix.

Revision 1.2  1994/09/30  16:55:23  esb
replaced ubytes by fox words.

Revision 1.1  1994/07/14  22:59:28  robby
Initial revision

Revision 1.1  1994/07/14  20:29:49  robby
Initial revision

Revision 1.1  94/07/13  18:49:11  robby
Initial revision

Revision 1.1  1994/05/20  02:25:06  robby
Initial revision



		1.	functor Swap_Bytes
*)

functor Swap_Bytes (structure Order: BYTE_ORDERS): SWAP_BYTES =
 struct

  val swap_ubyte4 = Order.B4.invert
  val swap_ubyte2 = Order.B2.invert

  val do_swap = {swap_ubyte4 = swap_ubyte4, swap_ubyte2 = swap_ubyte2}

  fun id x = x

  val no_swap = {swap_ubyte4 = id, swap_ubyte2 = id}
 end
