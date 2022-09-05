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

	A build functor for Dns/Tcp/Udp.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Build_Tcp

		iii.	RCS Log
	
$Log: builddns.fun,v $
Revision 1.4  1997/04/22  11:27:42  esb
added dns_timeout.

Revision 1.3  96/10/03  18:39:00  esb
made computation of UDP checksums a functor parameter.

Revision 1.2  1996/05/14  01:27:10  esb
adapted to new tcpmain.fun.

Revision 1.1  1996/03/04  21:27:39  esb
Initial revision


		1.	functor Build_Dns
*)

functor Build_Dns (structure Device: DEVICE_PROTOCOL
		     sharing type Device.Incoming.T = Device.Outgoing.T
		   structure B: FOX_BASIS
		   val tcp_window_size: int
		   val tcp_user_timeout: Word32.word (* in milliseconds *)
		   val udp_checksum: bool
		   val dns_timeout: int
		   val eth_debug_level: int ref option
		   val arp_debug_level: int ref option
		   val ip_debug_level: int ref option
		   val tcp_debug_level: int ref option
		   val udp_debug_level: int ref option
		   val dns_debug_level: int ref option): DNS_STACK =
 struct

  local
   val tcp_protocol = Word8.fromInt  6
   val udp_protocol = Word8.fromInt  17
   val dns_port	    = Word16.fromInt 53
   structure Lower = Build_Tcp (structure Device = Device
				structure B = B
				val tcp_over_ip = tcp_protocol
				val initial_window_size = tcp_window_size
				val user_timeout = tcp_user_timeout
				val compute_checksums = true
				val eth_debug_level = eth_debug_level
				val arp_debug_level = arp_debug_level
				val ip_debug_level = ip_debug_level
				val tcp_debug_level = tcp_debug_level)
  in
   open Lower

   structure Udp = Udp (structure Lower = Lower.Ip
			structure B = B
			val udp_over_ip = udp_protocol
			val compute_checksums = udp_checksum
			val debug_level = udp_debug_level)

  structure Dns = Dns_Protocol (structure B = B
				structure Lower = Udp
				val dns_port = dns_port
				val debug_level = dns_debug_level)

  structure Dns_Lookup = Dns_Lookup (structure B = B
				     structure Host_Id = Udp.Host_Id
				     structure Dns = Dns
				     val timeout = dns_timeout
				     val debug_level = dns_debug_level)
  end

 end (* struct *)
 
