(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Test the ethernet protocol.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Ethernet
	2.	Test_Ethernet

		iii.	RCS Log
	
$Log: eth.tst,v $
Revision 1.41  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.40  1995/11/10  23:33:26  esb
adapted to new setup type.

Revision 1.39  1995/10/02  20:59:58  esb
added the xmeter_pathname to the device functor.

Revision 1.38  1995/09/15  16:40:39  cline
work around for representation analysis bug

Revision 1.37  1995/06/21  21:51:00  cline
added test2.run to run.

Revision 1.36  1995/06/20  16:56:38  esb
adapted to new protocol signature.

Revision 1.35  1995/03/12  16:23:25  esb
adapted to new Trace.sig.

Revision 1.34  1995/03/07  20:32:16  esb
updated tracing.

Revision 1.33  1995/02/13  23:06:01  esb
adapted to new dynarray interface.

Revision 1.32  1995/02/09  19:53:46  esb
made work under sml/nj 1.07

Revision 1.31  1995/01/18  20:59:20  esb
renamed datatype address to be eth_address.

Revision 1.30  1995/01/06  16:54:08  esb
adapted to new Test_Addresses.

Revision 1.29  1994/10/25  17:09:16  esb
eliminated an undesirable print statement in makestring_incoming.

Revision 1.28  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.27  1994/09/12  18:11:57  milnes
Updated to hex address prints.

Revision 1.26  1994/08/24  22:21:12  esb
we now allow for incoming packets longer than what we sent.

Revision 1.25  1994/08/02  20:26:48  esb
adapted to new protocol signature.

Revision 1.24  1994/07/01  02:26:40  danwang
Moved control structures into Fox_Basis.

Revision 1.23  1994/06/16  16:37:34  danwang
Updated to use functorized Fox_Basis

Revision 1.22  1994/05/23  13:59:10  milnes
Added print function tests.

Revision 1.21  1994/04/26  20:10:15  esb
change in a timing constant.

Revision 1.20  94/04/26  18:01:29  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.19  94/03/02  21:23:31  esb
removed headers from comparison.

Revision 1.18  94/01/18  15:25:10  esb
restructured.

Revision 1.17  1993/12/04  21:02:20  esb
now provide a handler with a parameter of type connection.

Revision 1.16  1993/10/25  19:31:57  cline
removed .U from Byte[421].U

Revision 1.15  1993/10/14  22:36:54  esb
fixed a minor bug (excess paren).

Revision 1.14  1993/10/14  18:35:08  milnes
Used implicit sequencing in let bodies.

Revision 1.13  1993/10/06  17:39:46  esb
fixed a minor problem and adapted to changes in the ethernet functor args.

Revision 1.12  1993/09/17  16:41:50  milnes
Changed default parameter stuff.

Revision 1.11  1993/09/02  15:56:26  esb
major clean-up

Revision 1.10  1993/07/16  18:27:41  esb
now correctly closes passively-opened connection; added do_if_debug.

Revision 1.9  1993/06/19  01:49:39  esb
shortened the name of the test

Revision 1.8  1993/06/17  02:37:20  esb
changed the type from TEST_FUNCTOR to TEST_ETHERNET (which I also declared).

Revision 1.7  1993/06/16  19:58:27  esb
cleaned up a little

Revision 1.6  1993/06/16  09:01:00  esb
changed the definition of address_pattern to only include protocol type

Revision 1.5  1993/06/15  23:44:52  esb
adapted to new passive_open which returns handler option

Revision 1.4  1993/06/15  17:33:38  esb
minor changes: formatting, do_if_debug

Revision 1.3  1993/06/14  18:50:33  esb
added protocol number to address patterns

Revision 1.2  1993/06/11  22:29:40  esb
made the world safe for byte1's again...

Revision 1.1  1993/06/10  23:06:53  milnes
Initial revision

		1.	functor Test_Ethernet
*)

functor Test_Ethernet (structure B: FOX_BASIS
                       val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "eth.tst"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print
  val debug_constant_string = Trace.debug_constant_string

  structure Sim = Build_Simulators (structure B = B
				    val xmeter_pathname = "/dev/null"
				    val debug_level = debug_level)

  structure Sly = Ethernet (structure Device = Sim.Sly
			    structure B = B
			    val debug_level = debug_level)

  structure Snow = Ethernet (structure Device = Sim.Snow
			     structure B = B
			     val debug_level = debug_level)

  val test_protocol = Word16.fromInt 0x888

  val sly = Snow.Eth_Address.Address
               {eth = Test_Addresses.name_eth "sly", proto = test_protocol}
  val snow = Sly.Eth_Address.Address
               {eth = Test_Addresses.name_eth "snow", proto = test_protocol}
  val snow_pat = Snow.Eth_Pattern.Complete
		   {eth = Test_Addresses.name_eth "sly", proto = test_protocol}
  val snow_pat1 = Snow.Eth_Pattern.Partial {proto = test_protocol}
  val sly_pat = Sly.Eth_Pattern.Partial {proto = test_protocol}

  structure Test1 = Test_Segment (structure Sender = Sly
				  val sender_setup = "SE0"
				  val receiver_address = snow
				  structure Receiver = Snow
				  val receiver_setup = "SE0"
				  val sender_pattern = snow_pat
				  val segment_size = 1500
				  structure B = Fox_Basis
				  val equal_packet = NONE
				  val debug_level = debug_level
				  val test_name = "eth short")

  structure Test2 = Test_Segment (structure Sender = Snow
				  val sender_setup = "SE0"
				  val receiver_address = sly
				  structure Receiver = Sly
				  val receiver_setup = "SE0"
				  val sender_pattern = sly_pat
				  val segment_size = 1500
				  structure B = Fox_Basis
				  val equal_packet = NONE
				  val debug_level = debug_level
				  val test_name = "eth long")

  val run = Test2.run o Test1.run

 end

(*
		2.	Test_Ethernet
*)

structure Test_Ethernet = Test_Ethernet (structure B = Fox_Basis
					 val debug_level = NONE)



