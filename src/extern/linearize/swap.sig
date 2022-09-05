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

	Two sets (records, actually) of functions are exported
	from SWAP_BYTES:
		do_swap contains functions that perform
		byte swapping on the indicated types.

                no_swap contains identity operations.

	Code to swap bytes iff host byte order <> network byte
	order might look like:

		local
		  val {swap_ubyte4, ...} =
		    if host_byte_order <> net_byte_order then
		      Swap_Bytes.do_swap
		    else
		      Swap_Bytes.no_swap
		in
		  ... (swap_ubyte4 x) ...
		end


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature SWAP_BYTES

		iii.	RCS Log
	
$Log: swap.sig,v $
Revision 1.3  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.2  1994/09/30  16:55:23  esb
replaced ubytes by fox words.

Revision 1.1  1994/07/14  22:59:30  robby
Initial revision

Revision 1.1  1994/07/14  20:29:53  robby
Initial revision

Revision 1.1  94/07/13  18:49:12  robby
Initial revision

Revision 1.1  1994/05/20  02:25:06  robby
Initial revision



		1.	signature SWAP_BYTES
*)

signature SWAP_BYTES =
 sig
  val do_swap: {swap_ubyte4: Word32.word -> Word32.word,
		swap_ubyte2: Word16.word -> Word16.word}

  val no_swap: {swap_ubyte4: Word32.word -> Word32.word,
		swap_ubyte2: Word16.word -> Word16.word}
 end (* sig *)
