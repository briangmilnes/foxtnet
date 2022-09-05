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
	
	A functor to build all of TCP/IP over ethernet.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Build_TCPIP_ETH

		iii.	RCS Log
	
$Log: buildtcpipeth.fun,v $
Revision 1.1  1995/06/20  17:20:09  esb
Initial revision

Revision 1.15  1995/03/21  17:17:52  cstone
Added address parameter to functor.

Revision 1.14  1995/03/12  17:58:17  esb
adapted to new trace.sig.

Revision 1.13  1995/03/07  23:52:00  esb
updated tracing.

Revision 1.12  1995/02/04  20:39:19  robby
updated to 107

Revision 1.11  1995/01/16  23:47:53  esb
commented out more unused filter code which wouldn't compile.

Revision 1.10  1995/01/14  02:27:32  esb
adapted to new filter interface.

Revision 1.9  1995/01/12  21:21:28  esb
turned off high_priority_filter, which is no longer needed.

Revision 1.8  1995/01/06  17:03:37  esb
temporarily eliminated setting the packet filter.

Revision 1.7  1995/01/03  18:11:45  esb
eliminated filter that is not yet implemented.

Revision 1.6  1994/12/21  20:36:49  milnes
Updated for new shadowed addressing, but it produces terrible performance
problems when the duplicate filters are installed.

Revision 1.5  1994/11/22  13:58:38  milnes
Removed addressing functor arguments.

Revision 1.4  1994/11/10  16:13:13  milnes
Updated for debug_trace.

Revision 1.3  1994/10/20  21:33:08  milnes
Checked in compiling on the alph, but it won't load on the mips
until this buffer sharing constraint is fixed.

Revision 1.2  1994/10/19  23:20:29  milnes
updated to use FoxWord.

Revision 1.1  1994/10/10  18:27:40  milnes
Initial revision


		1.	functor Build_Tcp_Ip_Eth
*)

functor Build_Tcp_Ip_Eth (structure B: FOX_BASIS
			  structure Protocol_Numbers:
                                        TCP_IP_ETH_PROTOCOL_NUMBERS
			  structure Stack_Trace: TCP_IP_ETH_TRACE
			  structure Addressing: ADDRESSING
                          val stack_address: Addressing.addressing_path
                         ): TCP_IP_ETH =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = Stack_Trace.stack
			   val module_name = "buildtcpipeth.fun")
  val trace_print = Trace.trace_print

  fun start_timing () =
       if Trace.trace_on () then B.Time.time_of_day ()
       else B.Time.zero_time

  fun end_timing (start) = 
       if Trace.trace_on () then 
        B.Time.makestring(B.Time.-(B.Time.time_of_day (),start))
       else ""

  structure Addressing = Addressing
  structure A = Addressing

  local
   structure Dev =
      Build_Eth_Dev (structure B = B
                     val high_priority_filter = false
                     val debug_level = Stack_Trace.dev)
  in
   structure Dev = Dev.Eth_Dev
  end

  local
   structure Tcp = Build_Tcp (structure Device = Dev
			      structure B = B
			      val ip_over_eth = Protocol_Numbers.ip_over_eth
			      val tcp_over_ip = Protocol_Numbers.tcp_over_ip
			      val initial_window_size = 4096
			      val user_timeout = 240000 (* 4 minutes *)
			      val eth_debug_level = Stack_Trace.eth
			      val arp_debug_level = Stack_Trace.arp
			      val ip_debug_level = Stack_Trace.ip
			      val icmp_debug_level = Stack_Trace.icmp
			      val tcp_debug_level = Stack_Trace.tcp)
  in
   open Tcp
  end

  fun ip_checksum ip =
       let val buffer = B.Create.create 4
       in FoxWord32.update (buffer, 0, B.Order.B4.to_big ip);
	  B.Checksum.checksum (buffer, 0, 4)
       end
  val udp_proto = Protocol_Numbers.udp_over_ip
  val protocol_checksum = FoxWord16.intToWord (FoxWord8.wordToInt udp_proto)

  structure Udp = Udp (structure Lower = Ip
		       structure B = B
		       fun ip_equal (a, b: Ip.ip_number) = a = b
		       val ip_checksum = ip_checksum
		       val udp_protocol = udp_proto
		       val protocol_checksum = protocol_checksum
		       val compute_checksums = false
		       val debug_level = Stack_Trace.udp)

  val dns_port = SW.n16 "53"
  fun make_address (ip, port) =
       Udp.Address (Udp.Key {peer = ip, local_port = dns_port,
			     remote_port = dns_port})

  structure Dns = Dns (structure B = B
		       structure Tcp_Or_Udp = Udp
		       datatype Tcp_Or_Udp = Tcp | Udp
		       val Tcp_Or_Udp = Udp
		       val make_address = make_address
		       val debug_level = Stack_Trace.dns)

  exception Initialization_Failed of string

  val ref_count = ref 0

  fun initialize () =
       let fun set_ipmux_interfaces [] = ()
	     | set_ipmux_interfaces ((A.AT.Interface {name, ip, mask,
						      gateways}) :: rest) =
	        (Ip_Mux.add (name, ip);
		 set_ipmux_interfaces rest)
	   fun set_ip_interfaces [] = ()
	     | set_ip_interfaces ((A.AT.Interface {name, ip, mask,
						   gateways}) :: rest) =
	        (Ip.set_interface_address (name,ip);
		 set_ip_interfaces rest)
	   fun set_subnet_masks [] = ()
	     | set_subnet_masks ((A.AT.Interface {name, ip, mask = NONE,
						  gateways}) :: rest) =
	        set_subnet_masks rest
	     | set_subnet_masks ((A.AT.Interface {name, ip, mask = SOME m,
						  gateways}) :: rest) =
	        (Ip_No_Icmp.set_subnet_mask (name, SOME m);
		 set_subnet_masks rest)
  (* This is wrong, the host needs a concept of a default gateway. *)
	   fun set_default_gateway [] = ()
	     | set_default_gateway ((A.AT.Interface {gateways =
						     ((A.AT.Gateway {name, ip})
						      :: others), ...}) ::
				    rest) =
		Ip_No_Icmp.set_default_gateway ip
	     | set_default_gateway (i :: rest) = set_default_gateway rest
(*
	    fun make_arp_filter ip = 
	         let val b = ByteArray.array (4,0)
		 in FoxWord32.update (b, 0, B.Order.B4.to_big ip);
		    {self = b, protocol = Default_Protocol_Numbers.ip_over_eth}
		 end
	    fun make_packet_filter [] = []
	      | make_packet_filter ((A.AT.Interface {name, ip,
		                     mask, gateways}) :: rest) =
		 (Ip_No_Icmp.filter ip) ::
		 (Arp.filter (make_arp_filter ip)) :: 
		 (make_packet_filter rest)
	    fun set_packet_filter interfaces =
	         (Dev.set_filter (make_packet_filter interfaces))
*)
	    val start = start_timing ()
	    val return =
	         if ! ref_count = 0 then
		  (A.initialize (! A.default);
		   trace_print (fn _ =>
				"Addressing.initialize " ^ end_timing start);
		   (case A.fetch () of
		       NONE => raise (Initialization_Failed "")
		     | SOME (A.AT.Host {name, interfaces}) =>
			(let val set_start = start_timing ()
			 in set_ipmux_interfaces interfaces;
			    Tcp.initialize ();
			    Udp.initialize ();
			    Dns.initialize ();
			    set_ip_interfaces interfaces;
			    set_subnet_masks interfaces;
			    set_default_gateway interfaces;
			    (* set_packet_filter interfaces; *)
			    trace_print (fn _ => "initialize set time " ^
					 end_timing set_start)
			 end;
			 ref_count := 1;
			 ! ref_count)))
		 else
		  (ref_count := (! ref_count) + 1;
		   ! ref_count)
       in trace_print (fn _ => "initialize total time " ^ end_timing start);
	  return 
       end 

  fun finalize () =
       (case ! ref_count of
           0 => 0
	 | 1 => 
	    (Tcp.finalize();
	     Udp.finalize();
	     Dns.finalize();
	     ref_count := 0;
	     ! ref_count)
	 | _ => 
	    (ref_count := (!ref_count) - 1;
	     !ref_count))

  structure Numbers = Protocol_Numbers
  structure Set = Stack_Trace

 end

