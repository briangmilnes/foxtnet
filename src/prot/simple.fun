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
	1.	functor Dev_Protocol
	2.	functor Eth_Protocol
	3.	functor Arp_Protocol
	4.	functor Ip_Protocol
	5.	functor Icmp_Protocol
	6.	functor Pseudo_Ip_Protocol
	7.	functor Length_Protocol
	8.	functor Udp_Protocol
	9.	functor Tcp_Protocol

	iii.	RCS Log

$Log: simple.fun,v $
Revision 1.11  1995/04/12  16:41:26  cstone
Immediately removed high_priority_filter again.

Revision 1.10  1995/04/12  16:17:34  cstone
Added high_priority_filter argument to Build_Eth_Dev for mach.

Revision 1.9  1995/04/06  21:13:43  petel
moved initialization from icmp to ip

Revision 1.8  1995/03/29  18:23:24  cstone
Corrected initialiation.

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

structure Simple_Trace =
 struct
  val stack_level = ref 0
  val dns_level = ref 0
  val tcp_level = ref 0
  val udp_level = ref 0
  val ip_level = ref 0
  val icmp_level = ref 0
  val arp_level = ref 0
  val eth_level = ref 0
  val dev_level = ref 0

  val stack = SOME stack_level
  val dns = SOME dns_level
  val tcp = SOME tcp_level
  val udp = SOME udp_level
  val ip = SOME ip_level
  val icmp = SOME icmp_level
  val arp = SOME arp_level
  val eth = SOME eth_level
  val dev = SOME dev_level
 end (* struct *)

local
 
structure Default_Protocol_Numbers: TCP_IP_ETH_PROTOCOL_NUMBERS =
 struct
   (* Ethernet Protocol Numbers *)
  val ip_over_eth  = SW.n16"0x800"
  val arp_over_eth = SW.n16"0x806"

   (* IP Protocol Numbers *)
  val icmp_over_ip = SW.n8"1"  (* [RFC 0790, page 6] *)
  val tcp_over_ip  = SW.n8"6"  (* [RFC 0790, page 6] *)
  val udp_over_ip  = SW.n8"17" (* [RFC 0790, page 6] *)
 end

structure Addressing_Types =
 struct
  datatype gateway = Gateway of {name : string, ip : FoxWord32.word}
  datatype interface = 
      Interface of {name : string, ip : FoxWord32.word, 
		    mask : FoxWord32.word option,
		    gateways : gateway list}
  datatype host = Host of {name : string, interfaces : interface list}
  datatype alias = Alias of {name : string option, host : host}
  exception Bad_Initialization of string
 end (* struct *)

structure Parser = Link_Parser (structure V = Fox_Basis.V)
structure Os_Addressing = Os_Addressing (structure B = Fox_Basis
			                 structure AT = Addressing_Types
					 structure Link_Parser = Parser)
structure Addressing = Addressing (structure B = Fox_Basis
				   structure Addressing_Types =
				             Addressing_Types
				   structure Os_Addressing = Os_Addressing
				   structure Link_Parser = Parser
				   val debug_level = Simple_Trace.stack)

exception Addressing_Uninitialized of string
fun get_interfaces () =
   (case (Addressing.fetch ()) of
      NONE => raise (Addressing_Uninitialized "")
    | SOME (Addressing.AT.Host {name, interfaces}) => interfaces)

in (* local *)

(*
	1.	functor Dev_Protocol

	This functor instantiates the ethernet device.
*)

functor Dev_Protocol (): DEVICE_PROTOCOL =
 struct
  local
   structure Build_Eth_Dev =
     Build_Eth_Dev (structure B = Fox_Basis
		    val debug_level = Simple_Trace.dev)
  in
   open Build_Eth_Dev.Eth_Dev
  end
 end (* struct *)

(*
	2.	functor Eth_Protocol

	This functor takes a device protocol and instantiates an
	ethernet protocol.
*)

functor Eth_Protocol (structure Dev: DEVICE_PROTOCOL
		       sharing type Dev.incoming = Fox_Basis.Dyn_Array.T
		           and type Dev.outgoing = Fox_Basis.Dyn_Array.T)
                    : ETHERNET_PROTOCOL =
 struct
  local
   structure Eth = Ethernet (structure Dev = Dev
			     structure B = Fox_Basis
			     val debug_level = Simple_Trace.eth)
  in
   open Eth
   fun initialize () =
      (((get_interfaces (); ())
         handle _ => Addressing.initialize (!Addressing.default));
       Eth.initialize ())
  end
 end (* struct *)

(*
	3.	functor Arp_Protocol
*)

functor Arp_Protocol (structure Eth: ETHERNET_PROTOCOL
		       sharing type Eth.incoming = Fox_Basis.Dyn_Array.T
		           and type Eth.outgoing = Fox_Basis.Dyn_Array.T)
                    : ADDRESS_RESOLUTION =
 struct
  local
   structure Arp = Arp_Eth (structure Eth = Eth
			    val arp_protocol_number = 
                                Default_Protocol_Numbers.arp_over_eth
			    structure B = Fox_Basis
			    val debug_level = Simple_Trace.arp)
  in
   open Arp
  end
 end

(*
	4.	functor Ip_Protocol
*)

functor Ip_Protocol (structure Arp: ADDRESS_RESOLUTION
		       sharing type Arp.incoming = Fox_Basis.Dyn_Array.T
		           and type Arp.outgoing = Fox_Basis.Dyn_Array.T)
                   : EXTENDED_IP =
 struct
  local
   structure Mux = Ip_Loop (structure Arp = Arp
			    structure B = Fox_Basis
			    val loopback_name = "lo0"
			    val ip_protocol_number = 
                               Default_Protocol_Numbers.ip_over_eth)

   fun set_ipmux_interfaces [] = ()
     | set_ipmux_interfaces ((Addressing.AT.Interface{name,ip,mask,gateways}) 
                             :: rest) =
        (Mux.add (name, ip);
         set_ipmux_interfaces rest)

   structure Ip = Ip (structure Lower = Mux
		      structure B = Fox_Basis
		      val debug_level = Simple_Trace.ip)

  fun set_ip_interfaces [] = ()
    | set_ip_interfaces ((Addressing.AT.Interface {name, ip, mask, gateways}) 
                         :: rest) =
        (Ip.set_interface_address (name,ip);
	 set_ip_interfaces rest)

   fun set_subnet_masks [] = ()
     | set_subnet_masks ((Addressing.AT.Interface {name, ip, mask = NONE,
				       gateways}) :: rest) =
         set_subnet_masks rest
     | set_subnet_masks ((Addressing.AT.Interface {name, ip, mask = SOME m,
				       gateways}) :: rest) =
	(Ip.set_subnet_mask (name, SOME m);
	 set_subnet_masks rest)

   fun set_the_default_gateway [] = ()
     | set_the_default_gateway ((Addressing.AT.Interface {gateways =
					  ((Addressing.AT.Gateway {name, ip})
					   :: others), ...}) :: rest) =
	Ip.set_default_gateway ip
     | set_the_default_gateway (i :: rest) = set_the_default_gateway rest

   val simple_count = ref 0

  in (*local*)
    open Ip

    fun initialize () =
      if (!simple_count = 0) then 
         let val init_val = Ip.initialize ()
             val interfaces = get_interfaces ()
         in
            set_ipmux_interfaces interfaces;
            set_subnet_masks interfaces;
            set_the_default_gateway interfaces;
	    set_ip_interfaces interfaces;
            simple_count := !simple_count + 1;
            init_val
         end
      else
         Ip.initialize ()

    fun finalize () =
      (if (!simple_count > 0) then simple_count := !simple_count - 1 else ();
       Ip.finalize ())
        
  end (*local*)
 end (*functor*)

(*
	5.	functor Icmp_Protocol
*)

functor Icmp_Protocol (structure Ip: EXTENDED_IP
			sharing type Ip.incoming_data = Ip.outgoing
			           = Fox_Basis.Dyn_Array.T): IP_PROTOCOL =
 struct
  local
  structure Icmp = Icmp (structure B = Fox_Basis
			 structure Ip = Ip
			 val serve_echos_and_address_masks = true
			 val log_icmp_echos = false
			 val debug_level = Simple_Trace.icmp)

  structure New_Ip = Ip_With_Icmp (structure Ip = Ip
				   structure Icmp = Icmp
				   structure B = Fox_Basis
				   val log_icmp_echos = false
				   val debug_level = Simple_Trace.icmp)

  in
   open New_Ip
  end
 end

(*
	6.	functor Pseudo_Ip_Protocol
*)

functor Pseudo_Ip_Protocol (structure Arp: ADDRESS_RESOLUTION
			     sharing type Arp.incoming = Fox_Basis.Dyn_Array.T
				 and type Arp.outgoing = Fox_Basis.Dyn_Array.T)
                   : IP_PROTOCOL =
 struct
  local
				      
   structure Pseudo_Ip = Pseudo_Ip (structure Lower = Arp
				    structure Addressing = Addressing
				    val local_ip_number = FoxWord32.intToWord 0
				    structure B = Fox_Basis)
  in
   open Pseudo_Ip
  end
 end

(*
	7.	functor Length_Protocol
*)

functor Length_Protocol (structure Arp: ADDRESS_RESOLUTION
			  sharing type Arp.incoming = Arp.outgoing
			                = Fox_Basis.Dyn_Array.T
			      and type Arp.allocation = int)
                       : ADDRESS_RESOLUTION =
 struct
  local
   structure Length = Min_Length (structure Lower = Arp
				  val lower_min_size = Arp.minimum_packet_size
				  structure B = Fox_Basis)
  in
   open Arp
   fun maximum_packet_size () = Arp.maximum_packet_size () - Length.length_size
   fun minimum_packet_size () = 0
   open Length
  end
 end

(*
	8.	functor Udp_Protocol
*)

functor Udp_Protocol (structure Lower: IP_PROTOCOL
		       sharing type Lower.incoming_data = Lower.outgoing
			             = Fox_Basis.Dyn_Array.T
			   and type Lower.ip_number = FoxWord32.word
			   and type Lower.ip_protocol = FoxWord8.word
		      val checksum: bool): UDP_PROTOCOL =
 struct
  local
   val ip_equal: FoxWord32.word * FoxWord32.word -> bool = op=
   fun ip_checksum ip_number =
        let val buffer = Fox_Basis.Create.create 4
	in FoxWord32.update (buffer, 0, Fox_Basis.Order.B4.to_big ip_number);
	   Fox_Basis.Checksum.checksum (buffer, 0, 4)
	end
   val protocol_number = Default_Protocol_Numbers.udp_over_ip
   val protocol_checksum =
         FoxWord16.intToWord (FoxWord8.wordToInt protocol_number)

   structure Udp = Udp (structure Lower = Lower
			structure B = Fox_Basis
			val ip_equal = ip_equal
			val ip_checksum = ip_checksum
			val protocol_checksum = protocol_checksum
			val udp_protocol = protocol_number
			val compute_checksums = checksum
			val debug_level = Simple_Trace.udp)
  in
   open Udp
  end
 end

(*
	9.	functor Tcp_Protocol
*)

functor Tcp_Protocol (structure Lower: IP_PROTOCOL
		       sharing type Lower.incoming_data = Lower.outgoing
			             = Fox_Basis.Dyn_Array.T
			   and type Lower.ip_number = FoxWord32.word
			   and type Lower.ip_protocol = FoxWord8.word
		      val checksum: bool): TCP_PROTOCOL =
 struct
  local
   val ip_equal: FoxWord32.word * FoxWord32.word -> bool = op=
   fun ip_checksum ip_number =
        let val buffer = Fox_Basis.Create.create 4
	in FoxWord32.update (buffer, 0, Fox_Basis.Order.B4.to_big ip_number);
	   Fox_Basis.Checksum.checksum (buffer, 0, 4)
	end
   val protocol_number = Default_Protocol_Numbers.tcp_over_ip
   val protocol_checksum =
         FoxWord16.intToWord (FoxWord8.wordToInt protocol_number)

   structure Tcp = Tcp (structure Lower = Lower
			structure B = Fox_Basis
			val ip_equal = ip_equal
			val ip_checksum = ip_checksum
			val protocol_checksum = protocol_checksum
			val tcp_protocol = protocol_number
			val compute_checksums = checksum
			val initial_window = 4096
			val abort_unknown_connections = false
			val user_timeout = 60000
			val debug_level = Simple_Trace.tcp)
  in
   open Tcp
  end
 end

end (*local*)
