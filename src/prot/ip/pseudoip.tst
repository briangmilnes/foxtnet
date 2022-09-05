(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken.Cline@cs.cmu.edu)
        Nick Haines (nickh@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	pseudoip.tst: testing the pseudo-ip.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	structure Test_Pseudo_Ip

	iii.	RCS Log

$Log: pseudoip.tst,v $
Revision 1.6  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.5  1995/09/15  16:40:39  cline
work around for representation analysis bug

Revision 1.4  1995/07/05  23:09:40  esb
minor fix.

Revision 1.3  1995/06/29  18:20:09  esb
adapted to new wordarray.

Revision 1.2  1995/06/23  17:55:30  cline
updated sender_setup and receiver_setup

Revision 1.1  1995/06/20  17:05:46  esb
Initial revision


	1.	structure Test_Pseudo_Ip
*)

structure Test_Pseudo_Ip =
 struct

  local

   val debug_level = NONE

   structure Wire = Wire (structure B = Fox_Basis
			  val debug_level = debug_level)

   val arp_protocol_number = Word16.fromInt 0x806
   val ip_protocol_number = Word16.fromInt 0x800
   local
     open Word48
   in
     val eth1 = fromInt 0x010203 * fromInt 0x1000000 + fromInt 0x040506
     val eth2 = fromInt 0x090807 * fromInt 0x1000000 + fromInt 0x060504
   end
   structure Dev1 = Ethernet_Device_Simulator (val local_address = eth1
					       structure Wire = Wire
					       structure B = Fox_Basis
					       val debug_level = debug_level
					       val name = "012345")
   structure Sim1 = Device_To_Protocol (structure Device = Dev1
					structure B = Fox_Basis
					val debug_level = debug_level)
   structure Eth1 = Ethernet (structure Device = Sim1
			      structure B = Fox_Basis
			      val debug_level = debug_level)
   structure Pip1 = Pseudo_Ip (structure Lower = Eth1
			       structure B = Fox_Basis)

   structure Dev2 = Ethernet_Device_Simulator (val local_address = eth2
					       structure Wire = Wire
					       structure B = Fox_Basis
					       val debug_level = debug_level
					       val name = "987654")
   structure Sim2 = Device_To_Protocol (structure Device = Dev2
					structure B = Fox_Basis
					val debug_level = debug_level)
   structure Eth2 = Ethernet (structure Device = Sim2
			      structure B = Fox_Basis
			      val debug_level = debug_level)
   structure Pip2 = Pseudo_Ip (structure Lower = Eth2
			       structure B = Fox_Basis)

   val receiver_address = Pip1.Network_Address.Address
			    {peer = eth2, proto = ip_protocol_number}
   val sender_pattern =
         Pip2.Network_Pattern.Partial {proto = ip_protocol_number}

   val sender_setup = Pip1.Network_Setup.Setup
			[{local_id = eth2, interface = "SE0",
			  gateways = [], mask = NONE, mtu=NONE}]
   val receiver_setup = Pip2.Network_Setup.Setup
			  [{local_id = eth1, interface = "SE0",
			    gateways = [], mask = NONE, mtu=NONE}]

   structure Test1 = Test_Segment (structure Sender = Pip1
				   val sender_setup = sender_setup
				   val receiver_address = receiver_address
				   structure Receiver = Pip2
				   val receiver_setup = receiver_setup
				   val sender_pattern = sender_pattern
				   val segment_size = 300
				   structure B = Fox_Basis
				   val equal_packet = NONE
				   val debug_level = debug_level
				   val test_name = "pseudo-ip")

   structure Test2 = Test_Segment (structure Sender = Pip1
				   val sender_setup = sender_setup
				   val receiver_address = receiver_address
				   structure Receiver = Pip2
				   val receiver_setup = receiver_setup
				   val sender_pattern = sender_pattern
				   val segment_size = 1
				   structure B = Fox_Basis
				   val equal_packet = NONE
				   val debug_level = debug_level
				   val test_name = "pseudo-ip (short)")

  in (* local *)

   fun run () =
        (Test1.run ();
	 Test2.run ())

  end (* local *)

 end (* struct *)
