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
	Pittsburgh, Pa 15213-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature BUILD_UDP_TCP

		iii.	RCS Log
	
$Log: buildudptcp.sig,v $
Revision 1.2  1994/08/29  17:45:33  robby
added Ip_Mux

Revision 1.1  1994/08/29  16:57:04  robby
Initial revision


		1.	signature BUILD_UDP_TCP

*)
signature BUILD_UDP_TCP=
sig
  structure Device:DEVICE_PROTOCOL
  structure Eth:ETHERNET_PROTOCOL
  structure Arp:ADDRESS_RESOLUTION
  structure Ip:IP_PROTOCOL
  structure Ip_Mux:IP_MULTIPLEXER
  structure Udp:UDP_PROTOCOL
  structure Tcp:TCP_PROTOCOL
end
