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

        A building functor for ip on top of icmp.


                ii.     Table of Contents

        i.      Abstract
        ii.     Table of Contents
        iii.    RCS Log
        1.      BuildIcmp

                iii.    RCS Log
        
$Log: buildipicmp.fun,v $
Revision 1.8  1995/03/07  23:43:54  esb
updated tracing.

Revision 1.7  1994/11/22  13:59:01  milnes
Removed addressing functor arguments.

Revision 1.6  1994/11/10  16:12:20  milnes
Updated for tcpipeth/addressing and debug_trace structure.

Revision 1.5  1994/10/20  19:55:34  esb
adapted to new ipicmp.fun

Revision 1.4  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.3  1994/09/12  18:28:55  milnes
Updated for the new icmp.

Revision 1.2  1994/08/28  21:46:32  milnes
Added default gateways.

Revision 1.1  1994/08/25  23:42:43  milnes
Initial revision


                1.      functor Build_Icmp
*)

functor Build_Ip_Icmp (structure Device: DEVICE_PROTOCOL
                       structure B: FOX_BASIS
                       sharing type Device.incoming = B.Dyn_Array.T
                           and type Device.outgoing = B.Dyn_Array.T
                       val ip_over_eth: FoxWord16.word
                       val log_icmp_echos: bool
		       val eth_debug_level: int ref option
		       val arp_debug_level: int ref option
		       val ip_debug_level: int ref option
		       val icmp_debug_level: int ref option): IP_ICMP_STACK =
 struct

  local
   structure Icmp_Block = 
     Build_Icmp (structure Device = Device
		 structure B = B 
		 val ip_over_eth = ip_over_eth
		 val serve_echos_and_address_masks = true
		 val log_icmp_echos = log_icmp_echos
		 val eth_debug_level = eth_debug_level
		 val arp_debug_level = arp_debug_level
		 val ip_debug_level = ip_debug_level
		 val icmp_debug_level = icmp_debug_level)
  in
   open Icmp_Block
   structure Ip_No_Icmp = Ip
  end

  structure Ip = Ip_With_Icmp (structure B = B 
			       structure Ip = Ip_No_Icmp
			       structure Icmp = Icmp
			       val log_icmp_echos = log_icmp_echos
			       val debug_level = icmp_debug_level)
 end

