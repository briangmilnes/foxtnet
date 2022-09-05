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

This is a signature for TCP.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_STACK

		iii.	RCS Log
	
$Log: buildtcp.sig,v $
Revision 1.11  1996/03/04  21:26:10  esb
Ip now satisfies IP_PROTOCOL rather than the less specific NETWORK_PROTOCOL.

Revision 1.10  1995/08/25  19:14:43  cline
updated to use NETWORK_PROTOCOL, etc.

Revision 1.9  1995/03/07  23:58:11  esb
renamed signature.

Revision 1.8  1994/10/19  23:21:13  milnes
added Icmp.

Revision 1.7  1994/09/12  18:18:04  milnes
Added Ip_No_Icmp.

Revision 1.6  1994/08/29  17:42:14  robby
added Ip_Mux

Revision 1.5  1994/08/29  17:26:32  robby
added Arp

Revision 1.4  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.3  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.2  1994/07/01  02:29:06  danwang
Moved control structures into Fox_Basis.

Revision 1.1  1994/06/07  16:34:26  robby
Initial revision


		1.	signature TCP_STACK
*)

signature TCP_STACK =
 sig
  structure Device: DEVICE_PROTOCOL
  structure Eth: ETHERNET_PROTOCOL
  structure Arp: ADDRESS_RESOLUTION_PROTOCOL
  structure Ip_Mux: IP_MULTIPLEXER
  structure Ip: IP_PROTOCOL
  structure Tcp: TCP_PROTOCOL
 end (* sig *)
