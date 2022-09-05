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

	A signature defining a stack with for IP and below with and
    without Icmp.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature Ip_Icmp

		iii.	RCS Log
	
$Log: buildipicmp.sig,v $
Revision 1.3  1995/03/07  23:43:54  esb
updated tracing.

Revision 1.2  1994/09/12  18:29:07  milnes
Added ip_icmp_stack.

Revision 1.1  1994/09/08  19:22:34  milnes
Initial revision


		1.	signature IP_ICMP_STACK
*)

signature IP_ICMP_STACK =
 sig
  structure Device: DEVICE_PROTOCOL 
  structure Eth: ETHERNET_PROTOCOL
  structure Arp: ADDRESS_RESOLUTION
  structure Ip_Mux: IP_MULTIPLEXER
  structure Ip_No_Icmp: EXTENDED_IP
  structure Icmp : ICMP_PROTOCOL
  structure Ip: IP_PROTOCOL
 end
