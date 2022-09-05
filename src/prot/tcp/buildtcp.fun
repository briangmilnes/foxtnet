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

	A build functor for Tcp.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Build_Tcp

		iii.	RCS Log
	
$Log: buildtcp.fun,v $
Revision 1.30  1996/05/14  01:26:37  esb
made the user timeout into a word32.

Revision 1.29  1996/03/04  21:26:10  esb
Ip now satisfies IP_PROTOCOL rather than the less specific NETWORK_PROTOCOL.

Revision 1.28  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.27  1995/11/12  16:35:38  esb
adapted to new buildip.fun.

Revision 1.26  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.25  1995/09/26  16:22:13  cline
*** empty log message ***

Revision 1.24  1995/09/14  21:10:23  cline
work around for representation bug

Revision 1.23  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.22  1995/02/04  20:40:26  robby
updated to 107

Revision 1.21  1994/11/22  13:58:03  milnes
Removed addressing functor arguments.

Revision 1.20  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.19  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.18  1994/09/12  18:15:06  milnes
Added default_gateway.

Revision 1.17  1994/08/28  21:42:04  milnes
Added default gateway.

Revision 1.16  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.15  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.14  1994/07/11  17:54:35  esb
removed do_traces.

Revision 1.13  1994/07/01  02:28:23  danwang
Moved control structures into Fox_Basis.

Revision 1.12  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.11  1994/06/07  17:19:48  esb
changed abort_unknown_connections back to true to avoid problems with
half-open connections on pop.

Revision 1.10  94/06/07  16:34:22  robby
 Added a signature

Revision 1.9  94/01/30  20:54:56  esb
added a user_timeout functor parameter.

Revision 1.8  1994/01/28  01:18:19  esb
minor change.

Revision 1.7  1994/01/19  21:07:40  esb
changed interfaces.

Revision 1.6  1994/01/11  20:23:48  esb
changed active_open_timeout to user_timeout.

Revision 1.5  1994/01/09  03:26:19  esb
removed the structure Fsm, added "abort_unknown_connnections = false".

Revision 1.4  1993/12/20  16:44:33  cline
added high_priority_filter

Revision 1.3  1993/12/17  02:36:43  esb
added the hide_received_header functor parameter for IP.

Revision 1.2  1993/11/11  04:58:13  esb
functor parameter changes.

Revision 1.1  1993/11/09  22:26:27  milnes
Initial revision

		1.	functor Build_Tcp

*)

functor Build_Tcp (structure Device: DEVICE_PROTOCOL
		     sharing type Device.Incoming.T = Device.Outgoing.T
		   structure B: FOX_BASIS
		   val tcp_over_ip: Word8.word
		   val initial_window_size: int
		   val user_timeout: Word32.word (* in milliseconds *)
		   val compute_checksums: bool
		   val eth_debug_level: int ref option
		   val arp_debug_level: int ref option
		   val ip_debug_level: int ref option
		   val tcp_debug_level: int ref option): TCP_STACK =
 struct

  local
   val ip_protocol = Word16.fromInt 0x800
   val icmp_protocol = Word8.fromInt 1

   structure Lower = Build_Ip (structure Device = Device
			       structure B = B
			       val ip_protocol = ip_protocol
			       val icmp_protocol = icmp_protocol
			       val log_icmp_echos = false
			       val eth_debug_level = eth_debug_level
			       val arp_debug_level = arp_debug_level
			       val ip_debug_level = ip_debug_level)
  in
   open Lower
  end

  structure Tcp = Tcp (structure Lower = Ip
		       structure B = B
		       val tcp_protocol = tcp_over_ip
		       val initial_window = initial_window_size
		       val compute_checksums = compute_checksums
		       val user_timeout = user_timeout
		       val debug_level = tcp_debug_level)

 end (* struct *)
 
