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

	A signature for byte1, byte2, byte4 dynamic locatives.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DYN_LOCS 

		iii.	RCS Log
	
$Log: dynlocs.sig,v $
Revision 1.2  1994/09/30  16:51:58  esb
replaced ubytes with foxwords.

Revision 1.1  1994/08/12  14:26:45  milnes
Initial revision


		1.	signature DYN_LOCS
*)

signature DYN_LOCS = 
 sig
  structure Byte1 : LOCATIVE
   sharing type Byte1.object = FoxWord8.word
  structure Byte2 : LOCATIVE
   sharing type Byte2.object = FoxWord16.word
  structure Byte4 : LOCATIVE 
   sharing type Byte4.object = FoxWord32.word
 end


