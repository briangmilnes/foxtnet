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

This is a signature for the BuildUdp functor.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature UDP_STACK

		iii.	RCS Log
	
$Log: buildudp.sig,v $
Revision 1.13  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.12  96/03/04  21:26:29  esb
Ip now satisfies IP_PROTOCOL rather than the less specific NETWORK_PROTOCOL.

Revision 1.11  1996/01/16  22:38:59  cline
*** empty log message ***

Revision 1.10  1995/07/03  23:32:11  esb
adapted to an ill-defined compiler bug.

Revision 1.9  1995/06/28  20:07:38  cline
*** empty log message ***

Revision 1.8  1995/03/07  20:37:11  esb
updated tracing.

Revision 1.7  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.6  1994/08/29  17:40:42  robby
added Ip_Mux

Revision 1.5  1994/08/29  17:25:33  robby
added Arp

Revision 1.4  1994/08/16  00:45:52  esb
stub implementation, does not pass tests.

Revision 1.3  1994/07/01  02:33:25  danwang
Moved control structures into Fox_Basis.

Revision 1.2  1994/06/16  16:43:23  danwang
Updated to use functorized Fox_Basis

Revision 1.1  1994/06/07  16:36:37  robby
Initial revision


		1.	signature UDP_STACK
*)

signature UDP_STACK =
 sig
  structure Device: DEVICE_PROTOCOL
  structure Eth: ETHERNET_PROTOCOL
  structure Arp: ADDRESS_RESOLUTION_PROTOCOL
  structure Ip_Mux: IP_MULTIPLEXER
  structure Ip: IP_PROTOCOL where type Host_Id.T = Word32.word
  structure Udp: UDP_PROTOCOL where type host_id = Word32.word
 end (* sig *)
