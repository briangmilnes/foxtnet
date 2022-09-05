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

	These signatures specialize LINEARIZE to marshall and
	unmarshall from ubyte1, ubyte2, and ubyte4.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature UBYTE1_LINEARIZE
	2.	signature UBYTE2_LINEARIZE
	3.	signature UBYTE4_LINEARIZE

		iii.	RCS Log
	
$Log: ubyte.sig,v $
Revision 1.3  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.2  1994/09/30  16:55:23  esb
replaced ubytes by fox words.

Revision 1.1  1994/07/14  20:28:52  robby
Initial revision

Revision 1.1  94/07/13  18:49:16  robby
Initial revision


		1.	signature UBYTE1_LINEARIZE
*)

signature UBYTE1_LINEARIZE =
 sig
  include LINEARIZE
  sharing type T = Word8.word
 end

(*
		2.	signature UBYTE2_LINEARIZE
*)

signature UBYTE2_LINEARIZE  = 
 sig
  include LINEARIZE
  sharing type T = Word16.word
 end

(*
		3.	signature UBYTE4_LINEARIZE
*)

signature UBYTE4_LINEARIZE  = 
 sig
  include LINEARIZE
  sharing type T = Word32.word
 end
