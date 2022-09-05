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

	This is a signature for an ethernet device.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature BUILD_ETH_DEV

		iii.	RCS Log
	
$Log: buildethdev.sig,v $
Revision 1.3  1995/09/18  19:30:07  esb
first running version for alpha.

Revision 1.2  1994/08/02  20:25:36  esb
adapted to new protocol signature.

Revision 1.1  1994/06/07  16:31:38  robby
Initial revision


		1.	signature BUILD_ETH_DEV
*)

signature BUILD_ETH_DEV =
  sig
    structure Raw: RAW_DEVICE
    structure Dev: DEVICE_PROTOCOL
  end 
