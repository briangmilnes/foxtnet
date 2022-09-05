(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Kuo Chiang Chiang (kcchiang@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract
		
	This builds the SUN RPC protocol on top of UDP for the server.

	ii.	Table of Contents

	1.      functor Build_SunRPC_UDP

	iii.	RCS Log

$Log: build_sunrpc_udp.fun,v $
Revision 1.3  1995/05/02  23:41:29  cstone
no change

Revision 1.2  1995/05/02  23:39:49  cstone
no change

Revision 1.1  1994/10/14  11:59:57  kcchiang
Initial revision

*)

(*
	1.      functor Build_SunRPC_UDP
*)
functor Build_SunRPC_UDP (structure B                  : FOX_BASIS
			  structure Device             : DEVICE_PROTOCOL
			  val local_port               : ubyte2
			  val ip_over_eth              : ubyte2
			  val arp_over_eth             : ubyte2
			  val do_prints                : bool
			  val use_arp                  : bool
			  sharing type Device.incoming_message 
			               = B.Receive_Packet.T
			      and type Device.outgoing_message 
				       = B.Send_Packet.T 
			  ): BUILD_SUNRPC_PROT =
struct
    val udp_over_ip     = 1u17
    val use_arp         = use_arp
    val prot_header_size = 8		(* udp header size *)

    local
     structure Udp_Stack = 
	 BuildUdp(structure Device = Device
		  structure B = B
		  val ip_over_eth = ip_over_eth
		  val udp_over_ip = udp_over_ip
		  val use_arp = use_arp
		  val do_prints = do_prints)
    in
	structure Prot = Udp_Stack.Udp
	structure Ip = Udp_Stack.Ip

	val address_pattern = 
	    Udp_Stack.Udp.Pattern {local_port = local_port,
				   remote     = NONE}
    end

end
