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

This is a signature for the ethernet protocol.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature BUILDETH

		iii.	RCS Log
	
$Log: buildeth.sig,v $
Revision 1.4  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.3  1994/07/01  02:25:32  danwang
Moved control structures into Fox_Basis.

Revision 1.2  1994/06/07  16:27:31  robby
changed name of signature
.

Revision 1.1  94/06/05  19:55:52  robby
Initial revision


		1.	signature BUILDETH
*)

signature BUILDETH=
  sig
    structure Device : DEVICE_PROTOCOL
    structure Eth : ETHERNET_PROTOCOL
  end
