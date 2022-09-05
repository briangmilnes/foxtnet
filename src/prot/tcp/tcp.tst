(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (ken.cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcp.tst: simple correctness tests for TCP.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log

---------------------------------------------------------------------
	iii.	RCS Log

$Log: tcp.tst,v $
Revision 1.42  1996/07/19  19:49:47  cline
*** empty log message ***

Revision 1.41  1996/03/15  22:46:22  esb
made the test use "ln0" rather than "SE0".

Revision 1.40  1996/02/06  23:40:28  esb
adapted to new WORD_ARRAY signature (using words instead of ints).

Revision 1.39  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.38  1995/09/26  16:30:38  esb
functorized the test.

Revision 1.37  1995/09/25  16:47:24  cline
added packetmeter and increased data size

Revision 1.36  1995/09/20  19:57:58  cline
Rewritten for the new version of the Fox Net

Revision 1.26  1995/06/29  18:46:41  cline
Simple test for tcp based on Test_Segment

Revision 1.25  1995/03/12  17:52:40  esb
adapted to new trace.sig.

Revision 1.24  1995/03/10  03:48:00  esb
adapted to new vendor.sig.

Revision 1.23  1995/03/07  20:37:11  esb
updated tracing.

Revision 1.22  1995/02/21  13:06:10  esb
upgraded for SML/NJ 1.07.

Revision 1.21  1995/02/13  23:29:17  esb
adapted to the new dynarray interface.

Revision 1.20  1995/01/20  17:46:37  esb
adapted to new COROUTINE signature.

Revision 1.19  1995/01/06  17:02:58  esb
adapted to new Test_Addresses.

Revision 1.18  1994/12/05  22:03:24  esb
adapted to new event.sig.

Revision 1.17  1994/11/22  13:56:55  milnes
Updated to initialize its own addressing so that the tests would run.

Revision 1.16  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.15  1994/08/28  20:09:03  milnes
Added default gateway.

Revision 1.14  1994/08/24  22:23:47  esb
got to work with new protocol signature.

Revision 1.13  1994/07/01  02:33:49  danwang
Moved control structures into Fox_Basis.

Revision 1.12  1994/06/16  16:43:23  danwang
Updated to use functorized Fox_Basis

Revision 1.11  1994/05/23  14:03:59  milnes
Added print function tests.

Revision 1.10  1994/04/26  17:58:43  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.9  94/03/09  03:29:16  esb
made one of the tests 8000 bytes to test fragmentation.

Revision 1.8  94/03/02  21:22:37  esb
minor fix in an error message.

Revision 1.7  1994/01/18  17:05:41  esb
restructured and made faster.

Revision 1.6  1994/01/15  16:24:05  esb
added functors for ip fragmentation

Revision 1.5  1993/12/15  14:30:11  cline
added initial_interfaces to ip functor invocations

Revision 1.4  1993/12/04  20:58:09  esb
now provide a handler with a parameter of type connection.

Revision 1.3  1993/10/25  19:35:17  cline
removed .U from Byte[421].U

Revision 1.2  1993/10/25  18:19:07  cline
turned off do_prints

Revision 1.1  1993/10/18  20:03:26  cline
Initial revision

*)

functor Test_Tcp (structure B: FOX_BASIS
		  val window_size: int
		  val user_timeout: word32
		  val data_bytes: int) =
 struct

  local

   val quiet = NONE
   val show_some = SOME (ref 1)
   val show_all = SOME (ref 2)

   structure Debug =
    struct
      val sim = quiet
      val eth = quiet
      val arp = quiet
      val ip = quiet
      val icmp = quiet
      val tcp = show_all
      val segment = quiet
    end

   structure Sim = Build_Simulators (structure B = Fox_Basis
				     val debug_level = Debug.sim)

   val tcp_protocol = Word8.fromInt 6
   val port1 = Word16.fromInt 0x1111
   val port2 = Word16.fromInt 0x2222

   structure Sly_Stack = Build_Tcp (structure Device = Sim.Sly
				    structure B = Fox_Basis
				    val initial_window_size = window_size
				    val user_timeout = user_timeout
				    val compute_checksums = true
				    val tcp_over_ip = tcp_protocol
				    val eth_debug_level = Debug.eth
				    val arp_debug_level = Debug.arp
				    val ip_debug_level = Debug.ip
				    val icmp_debug_level = Debug.icmp
				    val tcp_debug_level = Debug.tcp)

   structure Snow_Stack = Build_Tcp (structure Device = Sim.Snow
				     structure B = Fox_Basis
				     val initial_window_size = window_size
				     val user_timeout = user_timeout
				     val compute_checksums = true
				     val tcp_over_ip = tcp_protocol
				     val eth_debug_level = Debug.eth
				     val arp_debug_level = Debug.arp
				     val ip_debug_level = Debug.ip
				     val icmp_debug_level = Debug.icmp
				     val tcp_debug_level = Debug.tcp)

   val sly_ip = Test_Addresses.get_ip "sly"
   val snow_ip = Test_Addresses.get_ip "snow"

   val receiver_address =
	 Sly_Stack.Tcp.Transport_Address.Complete
	   {peer = snow_ip, local_port = port1, remote_port = port2}
   val sender_pattern =
         Snow_Stack.Tcp.Transport_Pattern.Local_Specified
	   {local_port = port2}

   val sender_setup = Sly_Stack.Ip.Network_Setup.Setup
			[{local_id = sly_ip, interface = "ln0",
			  gateways = [], mask = NONE, mtu=NONE}]
   val receiver_setup = Snow_Stack.Ip.Network_Setup.Setup
			  [{local_id = snow_ip, interface = "ln0",
			    gateways = [], mask = NONE, mtu=NONE}]

   structure Test1 = Test_Stream (structure Sender = Sly_Stack.Tcp
				  val sender_setup = sender_setup
				  val receiver_address = receiver_address
				  structure Receiver = Snow_Stack.Tcp
				  val receiver_setup = receiver_setup
				  val sender_pattern = sender_pattern
				  val data_bytes = Word.fromInt data_bytes
				  structure B = Fox_Basis
				  val equal = NONE
				  val timeout_time = 2 * Word32.toInt user_timeout
				  val debug_level = Debug.segment
				  val test_name = "tcp")


  in (* local *)

   fun run () = Test1.run ()

  end (* local *)

 end (* struct *)

structure Test_Tcp = Test_Tcp (structure B = Fox_Basis
			       val window_size = 50000
			       val user_timeout = 0w60000:word32
			       val data_bytes = 200000)

