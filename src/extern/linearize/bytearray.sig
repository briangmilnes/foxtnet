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

	This specializes the LINEARIZE signature for bytearrays

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature BYTEARRAY_LINEARIZE

		iii.	RCS Log
	
$Log: bytearray.sig,v $
Revision 1.3  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.2  1994/08/08  15:11:24  danwang
Made changes to get zephyr authentication working.

Revision 1.1  1994/07/14  20:28:27  robby
Initial revision

Revision 1.1  94/07/13  18:44:06  robby
Initial revision


		1.	signature BYTEARRAY_LINEARIZE
*)

signature BYTEARRAY_LINEARIZE =
sig
  include LINEARIZE
  sharing type T=Word8Array.array
end

signature BYTEARRAY3_LINEARIZE =
sig
  include LINEARIZE
  datatype array_pair = Pair of  Word8Array.array * Word8Array.array
  sharing type T = array_pair
end
