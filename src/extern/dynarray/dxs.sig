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
	
	A set of structures for linearizing words into dynarrays.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DXS

		iii.	RCS Log
	
$Log: dxs.sig,v $
Revision 1.2  1994/09/30  16:56:04  esb
changed DXS to BYTE_EXTERN.

Revision 1.1  1994/08/12  14:26:45  milnes
Initial revision


		1.	signature BYTE_EXTERN
*)

signature BYTE_EXTERN =
 sig
  structure Byte1: EXTERN
    sharing type Byte1.T = FoxWord8.word
  structure Byte2: EXTERN
    sharing type Byte2.T = FoxWord16.word
  structure Byte4: EXTERN 
    sharing type Byte4.T = FoxWord32.word
 end

