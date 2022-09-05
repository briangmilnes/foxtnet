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

	A signature for fields of addresses used to specify 
   the parent protocol for dispatch.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature PROTOCOL_NUMBERS

		iii.	RCS Log
	
$Log: protocolnumbers.sig,v $
Revision 1.2  1994/10/19  23:19:53  milnes
updated to FoxWord.

Revision 1.1  1994/10/10  18:31:44  milnes
Initial revision


		1.	signature PROTOCOL_NUMBERS

*)

signature PROTOCOL_NUMBERS =
 sig
  (* Ethernet Protocol Numbers *)
   val ip_over_eth  : FoxWord16.word
   val arp_over_eth : FoxWord16.word
  (* IP Protocol Numbers *)
   val icmp_over_ip : FoxWord8.word
   val tcp_over_ip  : FoxWord8.word
   val udp_over_ip  : FoxWord8.word
 end 


