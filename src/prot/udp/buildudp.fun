(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A functor for building udp over ip over eth over ethdev.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Build_Udp

		iii.	RCS Log
	
$Log: buildudp.fun,v $
Revision 1.27  1996/02/23  21:34:11  esb
re-instated signature.

Revision 1.26  1996/02/15  19:02:18  esb
eliminated icmp_debug_level.

Revision 1.25  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.24  1995/11/12  16:35:57  esb
adapted to new buildip.fun.

Revision 1.23  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.22  1995/08/29  14:13:37  esb
adapted to new IP+ICMP.

Revision 1.21  1995/07/04  00:12:06  esb
renamed structure lower to Lower.

Revision 1.20  1995/06/28  20:07:29  cline
*** empty log message ***

Revision 1.19  1995/03/07  20:37:11  esb
updated tracing.

Revision 1.18  1995/02/04  20:40:34  robby
updated to 107

Revision 1.17  1994/11/22  13:58:03  milnes
Removed addressing functor arguments.

Revision 1.16  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.15  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.14  1994/09/12  18:18:36  milnes
Added default_gateway.

Revision 1.13  1994/08/28  21:42:44  milnes
Added default gateways.

Revision 1.12  1994/08/16  00:45:52  esb
stub implementation, does not pass tests.

Revision 1.11  1994/07/27  19:46:08  danwang
Fixed so udp layer prints out debug info correctly.

Revision 1.10  1994/07/01  02:33:18  danwang
Moved control structures into Fox_Basis.

Revision 1.9  1994/06/16  16:43:23  danwang
Updated to use functorized Fox_Basis

Revision 1.8  1994/06/07  16:36:32  robby
 Added a signature

Revision 1.7  94/01/18  04:21:37  esb
minor changes.

Revision 1.6  1993/12/20  19:41:59  cline
Added high_priority_filter as functor parameter.

Revision 1.5  1993/12/20  16:35:54  cline
added high_priority_filter

Revision 1.4  1993/12/17  02:35:19  esb
added the hide_received_header functor parameter for IP.

Revision 1.3  1993/11/11  05:01:26  esb
functor parameter changes.

Revision 1.2  1993/11/09  22:09:47  milnes
Shared the event queue.

Revision 1.1  1993/10/25  17:36:03  milnes
Initial revision


		1.	functor Build_Udp
*)

functor Build_Udp (structure Device: DEVICE_PROTOCOL
		   sharing type Device.Incoming.T = Device.Outgoing.T
		   structure B: FOX_BASIS
		   val udp_over_ip: Word8.word
		   val eth_debug_level: int ref option
		   val arp_debug_level: int ref option
		   val ip_debug_level: int ref option
		   val udp_debug_level: int ref option): UDP_STACK =
 struct

  local
   val ip_protocol = Word16.fromInt 0x800
   val icmp_protocol = 0wx1 : Word8.word

   structure Lower = Build_Ip (structure Device = Device
			       structure B = B
			       val icmp_protocol = icmp_protocol
			       val ip_protocol = ip_protocol
			       val eth_debug_level = eth_debug_level
			       val arp_debug_level = arp_debug_level
			       val ip_debug_level = ip_debug_level)
  in 
   open Lower
  end

  structure Udp = Udp (structure Lower = Ip
		       structure B = B
		       val udp_over_ip = udp_over_ip
		       val compute_checksums = false
		       val debug_level = udp_debug_level)

 end (* struct *)
