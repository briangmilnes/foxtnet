(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

This is a signature for DNS/TCP/UDP.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_STACK

		iii.	RCS Log
	
$Log: builddns.sig,v $
Revision 1.1  1996/03/04  21:27:39  esb
Initial revision


		1.	signature DNS_STACK
*)

signature DNS_STACK =
 sig
  structure Device: DEVICE_PROTOCOL
  structure Eth: ETHERNET_PROTOCOL
  structure Arp: ADDRESS_RESOLUTION_PROTOCOL
  structure Ip_Mux: IP_MULTIPLEXER
  structure Ip: IP_PROTOCOL
  structure Tcp: TCP_PROTOCOL
  structure Udp: UDP_PROTOCOL
  structure Dns: DNS_PROTOCOL
  structure Dns_Lookup: DNS_LOOKUP
   sharing type Tcp.Host_Id.T = Udp.Host_Id.T = Dns_Lookup.host_id
 end (* sig *)
