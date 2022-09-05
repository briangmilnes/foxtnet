(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	ip.tst: low-level test module for IP


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Ip
	2.	structure Test_Ip

	iii.	RCS Log

$Log: ip.tst,v $
Revision 1.48  1996/05/08  02:10:22  esb
adapted to new ip.fun.

Revision 1.47  1996/02/23  21:10:43  esb
adapted to new IP functor parameter (gateway).

Revision 1.46  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.45  1995/11/12  16:34:52  esb
adapted to new Word_Array.

Revision 1.44  1995/10/02  21:23:08  esb
adapted to new simulator functor with xmeter_pathname.

Revision 1.43  1995/09/15  16:40:39  cline
work around for representation analysis bug

Revision 1.42  1995/09/13  15:26:23  esb
uncommented 3 of 4 tests, removed obsolete code

Revision 1.41  1995/08/30  19:37:07  esb
made test programs work.

Revision 1.40  1995/06/26  17:29:44  esb
passes test.

Revision 1.39  1995/03/12  17:50:04  esb
adapted to new trace.sig.

Revision 1.38  1995/03/07  20:34:01  esb
updated tracing.

Revision 1.37  1995/02/21  15:45:37  esb
changed parameters to ipfrag to improve chances of passing test.

Revision 1.36  1995/02/13  23:28:04  esb
cleaned up.

Revision 1.35  1995/02/09  19:53:01  esb
changed to compile under 1.07

Revision 1.34  1995/01/18  21:02:13  esb
adapted to new coro.sig.

Revision 1.33  1995/01/06  16:54:55  esb
adapted to new Test_Addresses.

Revision 1.32  1994/11/22  13:56:09  milnes
Updated to initialize its own addressing so that the tests would run.

Revision 1.31  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.30  1994/09/12  18:14:16  milnes
Turned off do_prints.

Revision 1.29  1994/08/28  21:40:31  milnes
Added default gateway.

Revision 1.28  1994/08/02  20:32:38  esb
adapted to new protocol signature.

Revision 1.27  1994/07/04  21:32:56  esb
adapted to Copy/Create split.

Revision 1.26  1994/07/01  02:27:27  danwang
Moved control structures into Fox_Basis.

Revision 1.25  1994/06/16  16:39:20  danwang
Updated to use functorized Fox_Basis

Revision 1.24  1994/06/09  18:37:41  esb
adapted to new incoming_message type for IP.

Revision 1.23  1994/05/23  14:02:30  milnes
Added print function tests.

Revision 1.22  1994/04/26  18:00:06  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.21  94/02/20  13:31:03  esb
got rid of the "Sly" structure.

Revision 1.20  94/02/09  18:02:00  esb
fixed a minor problem.

Revision 1.19  94/01/18  15:54:21  esb
restructured.

Revision 1.18  1994/01/17  19:51:04  milnes
Changes for ip fragmentation.

Revision 1.17  94/01/13  14:58:24  cline
Added IP fragemntation test.

Revision 1.16  1993/12/15  14:23:58  cline
added initial_interfaces to IP functor invocations.

Revision 1.15  1993/12/04  21:01:17  esb
now provide a handler with a parameter of type connection.

Revision 1.14  1993/10/25  19:32:34  cline
removed .U from Byte[421].U

Revision 1.13  1993/10/21  16:17:07  esb
set do_prints to false (was mistakenly true).

Revision 1.12  1993/10/21  16:15:01  esb
Removed the header check.

Revision 1.11  1993/10/08  05:23:57  esb
updated to latest release of ip.fun.

Revision 1.10  1993/09/22  19:31:56  milnes
Removed "-----------"s.

Revision 1.9  1993/09/18  21:52:38  esb
minor changes.

Revision 1.8  1993/09/17  16:42:04  milnes
Changed default parameters.

Revision 1.7  1993/09/13  22:06:53  cline
deleted '#'s from RCS log

Revision 1.6  1993/09/02  22:20:22  esb
adapted to changes in the PROTOCOL signature.

Revision 1.5  1993/07/30  14:24:06  esb
added functor parameter "hide_received_header" for IP.

Revision 1.4  1993/07/16  18:45:04  esb
substantial rewrite to go along with the rewrite of IP.

Revision 1.3  1993/07/14  18:30:21  esb
fixed a bug whereby the passively opened connection was not
getting closed; this would keep the code from working the second time around.

Revision 1.2  1993/06/19  01:49:52  esb
shortened the name of the test

Revision 1.1  1993/06/19  01:40:02  esb
Initial revision


	1.	functor Test_Ip
*)

functor Test_Ip (structure B: FOX_BASIS
		 val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "eth.tst"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print
  val debug_constant_string = Trace.debug_constant_string
  val arp_protocol = Word16.fromInt 0x806
  val ip_protocol = Word16.fromInt 0x800
  val interface = "SE0"

  structure Sim = Build_Simulators (structure B = B
				    val xmeter_pathname = "/dev/null"
				    val debug_level = debug_level)

  structure Sly_Eth = Ethernet (structure Device = Sim.Sly
				structure B = B
				val debug_level = debug_level)

  structure Sly_Arp = Arp_Eth (structure Eth = Sly_Eth
			       val arp_protocol_number = arp_protocol
			       structure B = B
			       val debug_level = debug_level)

  structure Sly_Mux = Ip_Mux1 (structure Arp = Sly_Arp
			       val interface_to_arp_setup = fn _ => ()
			       val ip_protocol_number = ip_protocol
			       structure B = B)

  structure Sly_Ip = Ip (structure Lower = Sly_Mux
			 structure B = B
			 structure Host_Id = Ip_Host_Id
			 val icmp_protocol = 0w1:Word8.word
			 val gateway = false
			 val debug_level = debug_level)

  structure Snow_Eth = Ethernet (structure Device = Sim.Snow
				 structure B = B
				 val debug_level = debug_level)

  structure Snow_Arp = Arp_Eth (structure Eth = Snow_Eth
				val arp_protocol_number = arp_protocol
				structure B = B
				val debug_level = debug_level)

  structure Snow_Mux = Ip_Mux1 (structure Arp = Snow_Arp
				val arp_setup = ()
				val interface_to_arp_setup = fn _ => ()
				val ip_protocol_number = ip_protocol
				structure B = B)

  structure Snow_Ip = Ip (structure Lower = Snow_Mux
			  structure B = B
			  structure Host_Id = Ip_Host_Id
			  val icmp_protocol = 0w1:Word8.word
			  val gateway = false
			  val debug_level = debug_level)

  val sly_setup = Sly_Ip.Network_Setup.Setup
		    [{local_id = Test_Addresses.get_ip "sly",
		      interface = interface,
		      gateways = [], mask = NONE, mtu = NONE}]
  val snow_setup = Snow_Ip.Network_Setup.Setup
		     [{local_id = Test_Addresses.get_ip "snow",
		       interface = interface,
		       gateways = [], mask = NONE, mtu = NONE}]

  val test_protocol = Word8.fromInt 0x11
  fun make_address (ip_number, protocol) =
       {peer = ip_number, proto = protocol}

  val sly = make_address (Test_Addresses.get_ip "sly", test_protocol)
  val snow = make_address (Test_Addresses.get_ip "snow", test_protocol)

  val sly = Snow_Ip.Network_Address.Address
	       {peer = Test_Addresses.get_ip "sly", proto = test_protocol}
  val snow = Sly_Ip.Network_Address.Address
	       {peer = Test_Addresses.get_ip "snow", proto = test_protocol}
  val snow_pat = Snow_Ip.Network_Pattern.Complete
		   {peer = Test_Addresses.get_ip "sly", proto = test_protocol}
  val snow_pat1 = Snow_Ip.Network_Pattern.Partial {proto = test_protocol}
  val sly_pat = Sly_Ip.Network_Pattern.Partial {proto = test_protocol}

  structure Test1 = Test_Segment (structure Sender = Sly_Ip
				  val sender_setup = sly_setup
				  val receiver_address = snow
				  structure Receiver = Snow_Ip
				  val receiver_setup = snow_setup
				  val sender_pattern = snow_pat
				  val segment_size = 1
				  structure B = Fox_Basis
				  val equal_packet = NONE
				  val debug_level = debug_level
				  val test_name = "ip short")

  structure Test2 = Test_Segment (structure Sender = Snow_Ip
				  val sender_setup = snow_setup
				  val receiver_address = sly
				  structure Receiver = Sly_Ip
				  val receiver_setup = sly_setup
				  val sender_pattern = sly_pat
				  val segment_size = 1480
				  structure B = Fox_Basis
				  val equal_packet = NONE
				  val debug_level = debug_level
				  val test_name = "ip long")

  structure Test3 = Test_Segment (structure Sender = Sly_Ip
				  val sender_setup = sly_setup
				  val receiver_address = snow
				  structure Receiver = Snow_Ip
				  val receiver_setup = snow_setup
				  val sender_pattern = snow_pat
				  val segment_size = 1500
				  structure B = Fox_Basis
				  val equal_packet = NONE
				  val debug_level = debug_level
				  val test_name = "ip frag short")

  structure Test4 = Test_Segment (structure Sender = Snow_Ip
				  val sender_setup = snow_setup
				  val receiver_address = sly
				  structure Receiver = Sly_Ip
				  val receiver_setup = sly_setup
				  val sender_pattern = sly_pat
				  val segment_size = 10000
				  structure B = Fox_Basis
				  val equal_packet = NONE
				  val debug_level = debug_level
				  val test_name = "ip frag long")

  val run = Test4.run o Test3.run o Test2.run o Test1.run

 end

(*
		2.	structure Test_Ip
*)

structure Test_Ip = Test_Ip (structure B = Fox_Basis
			     val debug_level = NONE)



