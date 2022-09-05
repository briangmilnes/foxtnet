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
	
       A functor to build ip on top of eth on top of ethdev.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	BuildIp

		iii.	RCS Log
	
$Log: buildip.fun,v $
Revision 1.42  1996/05/14  01:20:19  esb
adapted to new ipmux.fun.

Revision 1.41  1996/03/12  22:23:41  esb
changed debug_level to be externally set

Revision 1.40  1996/03/12  22:21:51  esb
added loopback.

Revision 1.39  1996/03/04  21:24:58  esb
added Host_id, cleaned up.

Revision 1.38  1996/02/23  21:11:38  esb
restored signature, added IP functor parameter.

Revision 1.37  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.36  1995/11/12  16:34:27  esb
adapted to new Ip_Mux functor.

Revision 1.35  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.34  1995/09/13  15:25:17  esb
experimental changes.

Revision 1.33  1995/08/29  14:14:54  esb
version that includes IP and ICMP.

Revision 1.32  1995/07/03  23:24:42  esb
adapted to new buildip.sig.

Revision 1.31  1995/06/27  19:00:57  cline
adapted to new extern.sig

Revision 1.30  1995/03/07  20:33:53  esb
updated tracing.

Revision 1.29  1994/11/22  13:58:03  milnes
Removed addressing functor arguments.

Revision 1.28  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.27  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.26  1994/08/28  21:39:14  milnes
Added default_gateway.

Revision 1.25  1994/08/02  20:32:38  esb
adapted to new protocol signature.

Revision 1.24  1994/07/25  17:27:30  milnes
Changed initialize so that if it can't find the ip address it raises an error
and prints a nice warning message.

Revision 1.23  1994/07/01  02:26:48  danwang
Moved control structures into Fox_Basis.

Revision 1.22  1994/06/16  16:39:20  danwang
Updated to use functorized Fox_Basis

Revision 1.21  1994/06/15  20:46:53  milnes
Small changes for ip routing.

Revision 1.20  1994/06/13  22:37:54  esb
removed the hide_received_header parameter, which was no longer needed.

Revision 1.19  1994/06/07  16:33:01  robby
 Added a signature

Revision 1.18  94/06/05  18:40:44  milnes
Added an ip host specific arp filter.

Revision 1.17  1994/05/10  07:51:53  esb
adapted to new ipoptions.fun.

Revision 1.16  94/05/04  19:20:36  milnes
Made Ip_Eth visible in the result of the functor so that
 applications can resolve names.

Revision 1.15  1994/05/03  20:55:11  milnes
Added ip option handling.

Revision 1.14  1994/04/06  23:10:52  esb
removed an unnecessary relic.

Revision 1.13  94/01/18  16:54:28  esb
fixed the initialization for the case when ARP is not used.

Revision 1.12  1994/01/18  04:19:21  esb
added the use_arp switch as a functor parameter.

Revision 1.11  1994/01/17  19:51:04  milnes
Changes for ip fragmentation.

Revision 1.10  94/01/13  15:05:32  cline
Added IP fragmentation.

Revision 1.9  1993/12/20  16:34:34  cline
added high_priority_filter

Revision 1.8  1993/12/17  02:32:18  esb
added ARP support.

Revision 1.7  1993/12/14  20:22:09  esb
added IP functor parameter

Revision 1.6  1993/12/13  20:40:00  cline
fixed kerberos-2 ip address

Revision 1.5  1993/11/11  04:57:10  esb
functor parameter changes.

Revision 1.4  1993/11/09  22:47:53  cline
added kerberos.src.cs.cmu.edu addresses to ip_resolve

Revision 1.3  1993/11/09  22:08:14  milnes
Shared the event queue.

Revision 1.2  1993/10/25  19:33:06  cline
removed .U from Byte[421].U

Revision 1.1  1993/10/25  17:33:44  milnes
Initial revision


		1.	BuildIp
*)

functor Build_Ip (structure Device: DEVICE_PROTOCOL
		  structure B: FOX_BASIS
		  sharing type Device.Outgoing.T = Device.Incoming.T
		  val icmp_protocol: Word8.word
		  val ip_protocol: Word16.word
		  val eth_debug_level: int ref option
		  val arp_debug_level: int ref option
		  val ip_debug_level: int ref option): IP_STACK =
 struct

  val arp_protocol_number = Word16.fromInt 0x806

  structure Device = Device

  structure Eth = Ethernet (structure Device = Device
			    structure B = B
			    val debug_level = eth_debug_level)

  structure Arp = Arp_Eth (structure Eth = Eth
			   val arp_protocol_number = arp_protocol_number
			   structure B = B
			   val debug_level = arp_debug_level)

  structure Loop = Ip_Loop (structure Setup = Arp.Setup
			    structure Count = Arp.Count
			    structure External = Arp.Incoming
			    structure X = Arp.X
			    structure B = B
			    val debug_level = arp_debug_level)

  val loopback = "lo0"
  fun is_loopback interface = B.V.String.caseless_equal (interface, loopback)
  val not_loopback = not o is_loopback

  structure Ip_Mux = Ip_Mux2 (structure Arp1 = Arp
			      structure Arp2 = Loop
			      val is_interface1 = not_loopback
			      val is_interface2 = is_loopback
			      val ip_protocol_number1 = Word16.fromInt 0x800
			      val ip_protocol_number2 = Word16.fromInt 0
			      structure B = B
			      val debug_level = ip_debug_level)
(*
  structure Ip_Mux = Ip_Mux1 (structure Arp = Arp
			      structure Arp_Setup = Device.Setup
			      structure B = B
			      val ip_protocol_number = ip_protocol)
*)

  structure Ip = Ip (structure Lower = Ip_Mux
		     structure Host_Id = Ip_Host_Id
		     structure B = B
		     val icmp_protocol = icmp_protocol
		     val gateway = false
		     val debug_level = ip_debug_level)

 end
