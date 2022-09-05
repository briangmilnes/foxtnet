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

                i.      Abstract

        A building functor for icmp.


                ii.     Table of Contents

        i.      Abstract
        ii.     Table of Contents
        iii.    RCS Log
        1.      BuildIcmp

                iii.    RCS Log
        
$Log: buildicmp.fun,v $
Revision 1.9  1995/03/07  23:43:54  esb
updated tracing.

Revision 1.8  1994/11/22  13:59:01  milnes
Removed addressing functor arguments.

Revision 1.7  1994/11/10  16:12:20  milnes
Updated for tcpipeth/addressing and debug_trace structure.

Revision 1.6  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.5  1994/08/28  21:46:16  milnes
Updated to the new signatures, reimplemented.

Revision 1.4  1994/07/01  02:35:56  danwang
Moved control structures into Fox_Basis.

Revision 1.3  1994/06/16  16:47:18  danwang
Updated to use functorized Fox_Basis

Revision 1.2  1994/06/05  18:45:12  milnes
Added logging of icmp echos.

Revision 1.1  1994/05/23  14:09:03  milnes
Initial revision


                1.      functor Build_Icmp
*)

functor Build_Icmp (structure Device: DEVICE_PROTOCOL
		    structure B: FOX_BASIS
		    sharing type Device.incoming = B.Dyn_Array.T
                        and type Device.outgoing = B.Dyn_Array.T
                    val ip_over_eth: FoxWord16.word
                    val serve_echos_and_address_masks: bool
                    val log_icmp_echos: bool
		    val eth_debug_level: int ref option
		    val arp_debug_level: int ref option
		    val ip_debug_level: int ref option
		    val icmp_debug_level: int ref option) =
 struct

 local
  structure IpBlock = Build_Ip (structure Device = Device
				structure B = B
				val hide_received_header = false
				val ip_protocol = ip_over_eth
				val eth_debug_level = eth_debug_level
				val arp_debug_level = arp_debug_level
				val ip_debug_level = ip_debug_level)
 in
  open IpBlock
 end

  structure Icmp = Icmp (structure B = B
			 structure Ip = Ip
			 val serve_echos_and_address_masks =
			     serve_echos_and_address_masks
                         val log_icmp_echos = log_icmp_echos
			 val debug_level = icmp_debug_level)
 end

