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

	ethall.tst: testing standard ethernet.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	structure Test_Eth_All

	iii.	RCS Log

$Log: ethall.tst,v $
Revision 1.2  1995/07/05  23:32:54  esb
adapted to new wordarray signature.

Revision 1.1  1995/06/21  14:04:23  esb
Initial revision


	1.	structure Test_Eth_All
*)

structure Test_Eth_All =
 struct

  local

   val debug_level = NONE

   structure Wire = Wire (structure B = Fox_Basis
			  val debug_level = debug_level)

   val addr1 = SW.n48 "0x0102030405"
   structure Dev1 = Ethernet_Device_Simulator (val local_address = addr1
					       structure Wire = Wire
					       structure B = Fox_Basis
					       val debug_level = debug_level
					       val name = "012345")
   structure Sim1 = Device_To_Protocol (structure Device = Dev1
					structure B = Fox_Basis
					val debug_level = debug_level)
   structure Eth1 = Eth_All (structure Lower = Sim1
			     structure B = Fox_Basis)

   val addr2 = SW.n48 "0x090807060504"
   structure Dev2 = Ethernet_Device_Simulator (val local_address = addr2
					       structure Wire = Wire
					       structure B = Fox_Basis
					       val debug_level = debug_level
					       val name = "987654")
   structure Sim2 = Device_To_Protocol (structure Device = Dev2
					structure B = Fox_Basis
					val debug_level = debug_level)
   structure Eth2 = Eth_All (structure Lower = Sim2
			     structure B = Fox_Basis)

   val protocol_number = SW.n16 "0x333"
   val receiver_address = (protocol_number,
			   ({self = addr1, peer = addr2}, ()))
   val sender_pattern = (NONE, (NONE, ()))

   structure Test1 = Test_Segment (structure Sender = Eth1
				   val sender_setup = ()
				   val receiver_address = receiver_address
				   structure Receiver = Eth2
				   val receiver_setup = ()
				   val sender_pattern = sender_pattern
				   val segment_size = 300
				   structure B = Fox_Basis
				   val equal_packet = NONE
				   val debug_level = debug_level
				   val test_name = "block eth")

   fun equal_packet (sent, received) =
        let val size = Word_Array.W8.length sent
	    fun generate (0, _) = NONE
	      | generate (count, array) =
		 case Word_Array.W8.next array of
		    NONE => NONE
		  | SOME (value, new_array) =>
		     SOME (value, (count - 1, new_array))
	    val short_received = Word_Array.W8.new generate (size, received)
	in if Word_Array.W8.equal (short_received, sent) then true
	   else
	    (Fox_Basis.V.Print.print ("received wrong data, " ^
				      Fox_Basis.Compare.compare
				        (sent, short_received));
	     false)
	end

   structure Test2 = Test_Segment (structure Sender = Eth1
				   val sender_setup = ()
				   val receiver_address = receiver_address
				   structure Receiver = Eth2
				   val receiver_setup = ()
				   val sender_pattern = sender_pattern
				   val segment_size = 1
				   structure B = Fox_Basis
				   val equal_packet = SOME equal_packet
				   val debug_level = debug_level
				   val test_name = "block eth (short)")
  in (* local *)

   val run = Test2.run o Test1.run

  end (* local *)

 end (* struct *)
