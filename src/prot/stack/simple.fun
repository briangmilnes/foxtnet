(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken_Cline@cs.cmu.edu)
	Brian Milnes (milnes@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	simple.fun: A collection of stack-building functors with as
	few as possible functor parameters.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	structures Device and Initialize
	2.	functor Dev_Meter_Protocol
	3.	functor Dev_Meter_Protocol
	4.	functor Eth_Protocol
	5.	functor Arp_Protocol
	6.	functor Ip_Protocol
	7.	functor Udp_Protocol
	8.	functor Hostname_Lookup
	9.	functor Tcp_Lookup_Protocol
	10.	functor Dns_Layer
	11.	functor Tcp_Protocol

	iii.	RCS Log

$Log: simple.fun,v $
Revision 1.14  1996/04/18  21:32:07  cline
converted hash from int to word

Revision 1.13  1996/03/11  20:06:57  cline
updated for 109.6

Revision 1.12  1996/03/05  20:16:54  cline
added Hostname_Lookup DNS interface

Revision 1.11  1996/03/05  14:59:05  esb
working version.

Revision 1.10  1996/03/04  21:47:03  esb
will compile, DNS not yet fully supported.

Revision 1.9  1996/02/27  16:42:37  cline
cleaned up initialization code

Revision 1.8  1996/02/15  19:09:57  esb
adapted to new structures.

Revision 1.7  1996/02/07  19:20:25  cline
added domain_name_list for dnslookup and turned on udp checksums

Revision 1.6  1996/01/16  21:56:24  cline
*** empty log message ***

Revision 1.5  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.4  1995/11/12  16:41:34  esb
adapted to new device setup type (string), adapted to new Word_Array.

Revision 1.3  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.2  1995/10/03  19:50:28  cline
*** empty log message ***

Revision 1.1  1995/06/20  17:20:09  esb
Initial revision

Revision 1.7  1995/03/07  23:52:25  esb
updated to new Addressing.

Revision 1.6  1995/01/18  21:05:04  esb
added structure Addressing now needed by the Pseudo_Ip functor.

Revision 1.5  1994/11/07  21:35:29  cline
use V.String

Revision 1.4  1994/10/31  20:47:47  cline
*** empty log message ***

Revision 1.3  1994/10/27  20:28:23  cline
added SML/NJ 105 compatibility

Revision 1.2  1994/10/27  15:25:21  esb
fixed a bug.

Revision 1.1  1994/10/26  13:31:43  esb
Initial revision

*)

(*
	1.	structures Device and Initialize
*)

structure Device = Build_Eth_Dev (structure B = Fox_Basis
				  val interface_number = 0
				  val debug_level = NONE)

structure Initialize = Initialize (structure Device = Device.Raw
				   structure V = Fox_Basis.V
				   val hostname_to_ip =
				         Hostname_Ip.hostname_to_ip
				   val interface_number = 0)

(*
	2.	functor Dev_Meter_Protocol
*)

functor Dev_Protocol () = Device.Dev

(*
	3.	functor Dev_Meter_Protocol
*)

functor Dev_Meter_Protocol (structure Dev: DEVICE_PROTOCOL
			    val label: string)(*: DEVICE_PROTOCOL*) =
struct
  local
    structure Dev_Meter = Device_Meter (structure Lower = Dev
					val label = label
					val xmeter_pathname =
					      Constants.xmeter_pathname
					structure B = Fox_Basis
					val debug_level = NONE)
  in
    open Dev_Meter
  end
end

(*
	4.	functor Eth_Protocol

	This functor takes a device protocol and instantiates an
	ethernet protocol.
*)

functor Eth_Protocol (structure Dev: DEVICE_PROTOCOL
		      sharing type Dev.Incoming.T = Dev.Outgoing.T)
		    : ETHERNET_PROTOCOL =
  struct
    local
      structure Eth = Ethernet (structure Device = Dev
				structure B = Fox_Basis
				val debug_level = NONE)
    in
      open Eth

      (* cater to  compiler structure sharing limitation *)
      structure Pattern = Eth_Pattern
      structure Address = Eth_Address
      structure Connection_Key = Eth_Address
    end
  end (* struct *)

(*
	5.	functor Arp_Protocol
*)

functor Arp_Protocol (structure Eth: ETHERNET_PROTOCOL
		      sharing type Eth.Outgoing.T = Eth.Incoming.T)
		    : ADDRESS_RESOLUTION_PROTOCOL =
 struct
   local
     structure Arp = Arp_Eth (structure Eth = Eth
			      val arp_protocol_number = Constants.arp_protocol
			      structure B = Fox_Basis
			      val debug_level = NONE)
   in
     open Arp

     (* cater to  compiler structure sharing limitation *)
     structure Pattern = Arp_Pattern
     structure Address = Arp_Address
   end
 end

(*
	6.	functor Ip_Protocol
*)

functor Ip_Protocol (structure Arp: ADDRESS_RESOLUTION_PROTOCOL
		     sharing type Arp.Setup.T = string
		         and type Word_Array.T = Arp.Host.T
			 and type Arp.Protocol.T = Word16.word)
		   : NETWORK_PROTOCOL =
  struct
    local
      structure Ip_Mux = Ip_Mux1 (structure Arp = Arp
				  structure B = Fox_Basis
				  val ip_protocol_number =
				        Constants.ip_protocol)

      structure Ip = Ip (structure Lower = Ip_Mux
			 structure Host_Id = Ip_Host_Id
			 structure B = Fox_Basis
			 val icmp_protocol = Constants.icmp_protocol
			 val gateway = false
			 val debug_level = NONE)

    in
      open Ip
    end
  end

functor Pseudo_Ip_Protocol (structure Eth: ETHERNET_PROTOCOL
			    sharing Eth.Outgoing = Eth.Incoming
			        and type Eth.Setup.T = string)
			  : NETWORK_PROTOCOL =
  struct
    local
      structure Pseudo_Ip = Pseudo_Ip (structure Lower = Eth
				       structure B = Fox_Basis)
    in
      open Pseudo_Ip

      (* cater to  compiler structure sharing limitation *)
      structure Status = Network_Status
      structure Incoming = Network_Incoming
      structure Pattern = Network_Pattern
      structure Address = Network_Address
      structure Setup = Network_Setup
      structure Connection_Key = Network_Address
    end
  end

(*
	7.	functor Udp_Protocol
*)

functor Udp_Protocol (structure Lower: NETWORK_PROTOCOL
		      val protocol_number: Lower.Protocol_Id.T)
		    : UDP_PROTOCOL =
  struct
    local
      structure Udp = Udp (structure Lower = Lower
			   structure B = Fox_Basis
			   val udp_over_ip = protocol_number
			   val compute_checksums = true
			   val debug_level = NONE)
    in
      open Udp

      (* cater to  compiler structure sharing limitation *)
      structure Connection_Key = Transport_Key
      structure Address = Transport_Address
      structure Pattern = Transport_Pattern
    end
  end

(*
	8.	functor Hostname_Lookup
*)

functor Hostname_Lookup (structure Udp: UDP_PROTOCOL
			 sharing type Udp.Host_Id.T = Word32.word) =
 struct
   local
     structure Dns = Dns_Protocol (structure B = Fox_Basis
				   structure Lower = Udp
				   val dns_port = Constants.dns_port
				   val debug_level = NONE)

     structure Dns_Lookup = Dns_Lookup (structure B = Fox_Basis
					structure Host_Id = Udp.Host_Id
					structure Dns = Dns
					val debug_level = NONE)

     val udp_setup = Udp.Transport_Setup.Setup (Initialize.Udp.setup ())
     val dns_setup = Initialize.Dns.setup udp_setup
   in
     val lookup = Dns_Lookup.lookup dns_setup
   end
 end

(*
	9.	functor Tcp_Lookup_Protocol
*)

functor Tcp_Lookup_Protocol (structure Tcp: TCP_PROTOCOL
			     val lookup: string -> Tcp.Host_Id.T option)
        : TCP_PROTOCOL =
struct
  open Tcp
  structure Host_Id =
    struct
      open Host_Id
      fun parse s =
	case lookup s of
	  SOME t => SOME t
	| NONE => Host_Id.parse s
    end
end

(*
	10.	functor Dns_Layer
*)

functor Dns_Layer (structure Lower: TRANSPORT_PROTOCOL
		   structure Udp: UDP_PROTOCOL
		     sharing type Udp.Host_Id.T = Lower.Host_Id.T
		                = Word32.word): PROTOCOL =
 struct
  val dns_port = Word16.fromInt 53
  structure Dns = Dns_Protocol (structure B = Fox_Basis
				structure Lower = Udp
				val dns_port = dns_port
				val debug_level = NONE)

  structure Dns_Lookup = Dns_Lookup (structure B = Fox_Basis
				     structure Host_Id = Udp.Host_Id
				     structure Dns = Dns
				     val debug_level = NONE)

  open Lower

  structure Setup =
   struct
    type T = unit
    fun makestring _ = "DNS_Lookup setup"
    fun equal _ = true
    fun hash _ = 0w0
   end

  structure Address =
   struct
    type T = string * Lower.Transport_Address.port
    fun makestring (s, port) = s ^ "@" ^ Word16.toString port
    fun equal ((s1, p1), (s2: string, p2: Lower.Transport_Address.port)) =
         s1 = s2 andalso p1 = p2
    fun hash (s, p) = Word.fromLargeWord (Word16.toLargeWord p)
   end

  datatype session = S of {connect: Address.T * handler -> unit,
			   listen: Pattern.T * handler * Count.T -> listen,
			   extension: session_extension}


  fun session ((), handler) =
       let val transport = Initialize.Udp.setup ()
	   val udp = Udp.Transport_Setup.Setup transport
	   val dns = Initialize.Dns.setup udp
	   val lower_setup = Lower.Transport_Setup.Setup transport
	   val resolve = Dns_Lookup.lookup dns
	   fun connect lower_connect ((host_name, port), handlers) =
	        case resolve host_name of
		   NONE =>
		    raise (X.Connection "DNS unable to resolve")
		 | SOME lower_address =>
		    lower_connect (Lower.Transport_Address.Remote_Specified
				   {peer = lower_address, remote_port = port},
				   handlers)
	   fun wrap_handler (Lower.S {connect = lower_connect,
				      listen = lower_listen, extension}) =
	        handler (S {connect = connect lower_connect,
			    listen = lower_listen, extension = extension})
       in Lower.session (lower_setup, wrap_handler)
       end

 end

(*
	11.	functor Tcp_Protocol
*)

functor Tcp_Protocol (structure Lower: NETWORK_PROTOCOL
		      val compute_checksums: bool
		      val protocol_number: Lower.Protocol_Id.T)
		    : TCP_PROTOCOL =
  struct
    local
      val initial_window_size = 4096
      val user_timeout = 10000

      structure Tcp = Tcp (structure Lower = Lower
			   structure B = Fox_Basis
			   val tcp_protocol = protocol_number
			   val initial_window = initial_window_size
			   val compute_checksums = compute_checksums
			   val user_timeout = user_timeout
			   val debug_level = NONE)
    in
      open Tcp

      (* cater to  compiler structure sharing limitation *)
      structure Status = Tcp_Status
      structure Pattern = Transport_Pattern
      structure Address = Transport_Address
      structure Connection_Key = Transport_Key
    end
  end
