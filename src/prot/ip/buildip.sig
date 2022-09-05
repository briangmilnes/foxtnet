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

A signature for the BuildIp functor

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature IP_STACK

		iii.	RCS Log
	
$Log: buildip.sig,v $
Revision 1.10  1996/03/04  21:25:13  esb
Ip now satisfies IP_PROTOCOL rather than the less specific NETWORK_PROTOCOL.

Revision 1.9  1995/07/03  23:23:06  esb
adapted to ill-identified compiler bug.

Revision 1.8  1995/06/27  19:01:04  cline
adapted to new extern.sig

Revision 1.7  1994/09/12  18:12:09  milnes
No changes.

Revision 1.6  1994/08/29  17:43:26  robby
added Ip_Mux

Revision 1.5  1994/08/28  21:39:41  milnes
Changed the name of the extended ip.

Revision 1.4  1994/08/02  20:32:38  esb
adapted to new protocol signature.

Revision 1.3  1994/07/01  02:26:59  danwang
Moved control structures into Fox_Basis.

Revision 1.2  1994/06/16  16:39:20  danwang
Updated to use functorized Fox_Basis

Revision 1.1  1994/06/07  16:33:14  robby
Initial revision


		1.	signature IP_STACK
*)

signature IP_STACK =
 sig
  structure Device: DEVICE_PROTOCOL 
  structure Eth: ETHERNET_PROTOCOL 
  structure Arp: ADDRESS_RESOLUTION_PROTOCOL
  structure Ip_Mux: IP_MULTIPLEXER
  structure Ip: IP_PROTOCOL
 end  (* sig *)
