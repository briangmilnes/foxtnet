(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Nick Haines (nickh@cs.cmu.edu)
	Brian Milnes (milnes@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	eth.fun: An ethernet protocol functor, and a test Functor.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor types
	2.	structure Eth_Number
	3.	structure Eth_Protocol
	4.	structure Eth_Address
	5.	structure Eth_Pattern
	6.	other sub-structures
	7.	extension types
	8.	structure Eth_Header
	9.	functions for structure Connection
	10.	structure Connection
	11.	export objects declared in signature

	iii.	RCS Log

$Log: eth.fun,v $
Revision 1.85  1997/03/04  17:36:56  esb
to improve performance, made identify reject all packets from self.

Revision 1.84  1996/04/18  21:01:15  cline
converted hash from into to word

Revision 1.83  1996/01/19  23:01:26  esb
adapted to the new wordarray signature.

Revision 1.82  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.81  1995/11/10  23:31:35  esb
adapted to new word_array

Revision 1.80  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.79  1995/10/17  21:46:15  esb
installed the latest connection functor.

Revision 1.78  1995/10/04  18:56:36  esb
added new conn functor.

Revision 1.77  1995/10/02  20:59:35  esb
added the latest connection functor.

Revision 1.76  1995/09/19  18:50:24  esb
changes in some print statements.

Revision 1.75  1995/09/14  21:07:14  cline
Work around for representation bug

Revision 1.74  1995/09/13  15:24:19  esb
made protocol state into data type, changed identify to match new conn.fun.

Revision 1.73  1995/08/08  18:17:15  esb
adapted to new ethheader.

Revision 1.72  1995/07/21  12:50:44  esb
adapted to new conn.fun (again).

Revision 1.71  1995/07/20  21:33:02  esb
adapted to changed conn.fun

Revision 1.70  1995/07/03  23:30:51  esb
adapted to new conn.fun.

Revision 1.69  1995/06/29  18:19:37  esb
adapted to new wordarray.

Revision 1.68  1995/06/27  16:55:31  esb
adapted to new conn.fun

Revision 1.67  1995/06/26  17:28:29  esb
worked around a compiler bug.

Revision 1.66  1995/06/23  19:54:43  esb
adapted to a slightly modified conn.fun

Revision 1.65  1995/06/20  16:56:38  esb
adapted to new protocol signature.

Revision 1.64  1995/03/24  01:38:44  esb
ci -u eth.fun
minor fix.

Revision 1.63  1995/03/12  16:23:25  esb
adapted to new Trace.sig.

Revision 1.62  1995/03/10  03:46:01  esb
adapted to new vendor.sig.

Revision 1.61  1995/03/07  20:32:16  esb
updated tracing.

Revision 1.60  1995/02/21  15:45:18  esb
made packet counters into 64-bit quantities.

Revision 1.59  1995/02/04  20:39:52  robby
updated to 107

Revision 1.58  1995/01/18  20:59:20  esb
renamed datatype address to be eth_address.

Revision 1.57  1995/01/06  01:33:17  esb
removed set_arp_filter and clear_arp_filter.

Revision 1.56  1994/11/11  18:09:59  esb
changed the functor parameters for Debug_Trace_Printer.

Revision 1.55  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.54  1994/11/07  21:32:20  cline
use V.Print

Revision 1.53  1994/10/25  16:32:03  esb
added handles around all calls to Conn.functions.

Revision 1.52  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.51  1994/09/30  16:58:44  esb
now closes lower connection from passive open only if no other references

Revision 1.50  1994/09/12  18:11:22  milnes
Added tracing and made addresses print in hex.

Revision 1.49  1994/08/26  14:22:57  milnes
Added some debug prints.

Revision 1.48  1994/08/16  20:13:41  esb
added some prints when raising exceptions

Revision 1.47  1994/08/12  06:20:16  esb
added type allocation and set/clear_arp_filter.

Revision 1.46  1994/08/02  20:26:48  esb
adapted to new protocol signature.

Revision 1.45  1994/07/04  21:32:24  esb
adapted to Copy/Create split.

Revision 1.44  1994/07/01  02:25:56  danwang
Moved control structures into Fox_Basis.

Revision 1.43  1994/06/16  16:37:34  danwang
Updated to use functorized Fox_Basis

Revision 1.42  1994/05/23  13:58:22  milnes
Added printing functions.

Revision 1.41  1994/05/10  07:46:30  esb
adapted to new store.sig, added many minor optimizations.

Revision 1.40  94/04/26  18:00:50  esb
fixed a race condition connected with passive open.

Revision 1.39  94/04/06  23:09:46  esb
adapted to new receive_packet interface.

Revision 1.38  94/03/29  17:44:40  milnes
Moved to using Receive_Packet size.

Revision 1.37  1994/03/10  19:41:30  esb
used Copy.create.

Revision 1.36  94/03/02  21:25:13  esb
better handling of device open errors.

Revision 1.35  94/02/20  23:59:15  esb
modified send to adapt to new send_packet interface.

Revision 1.34  94/02/18  14:33:35  milnes
Extended the timing calls and made the fast path timing calls non-cumulative.

Revision 1.33  1994/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file move problems that confused rcs.

Revision 1.31  1994/02/08  14:16:11  esb
added a print statement when raising unknown exceptions in send.

Revision 1.30  1994/01/30  19:36:32  esb
minor bug.

Revision 1.29  1994/01/30  19:23:53  esb
minor change.

Revision 1.28  1994/01/19  21:41:07  esb
corrected the maximum payload size.

Revision 1.27  1994/01/19  21:07:00  esb
added printing error messages before raising packet size.

Revision 1.26  1993/12/04  21:02:09  esb
now provide a handler with a parameter of type connection.

Revision 1.25  1993/11/11  04:54:42  esb
Changed to explicitly handle Send_Packet.Illegal_Extend in send.

Revision 1.24  1993/11/09  22:07:04  milnes
Added some tracing.

Revision 1.23  1993/10/25  19:31:43  cline
removed .U from Byte[421].U

Revision 1.22  1993/10/14  20:57:12  milnes
Undid a typobug.

Revision 1.21  1993/10/14  18:34:07  milnes
Used implicit sequencing in let bodies.

Revision 1.20  1993/10/08  05:23:08  esb
fixed a bug in close.

Revision 1.19  1993/10/06  17:39:46  esb
adapted to changes in the dispatcher.

Revision 1.18  1993/10/06  12:11:28  milnes
Updated to centralize state and with changes from code review.

Revision 1.17  1993/09/17  16:41:22  milnes
Changed default parameter stuff.

Revision 1.16  1993/09/14  15:44:12  cline
added hash_table_size to Dispatcher

Revision 1.15  1993/09/10  11:37:04  esb
adapted to new event.sig.

Revision 1.14  93/09/02  15:56:07  esb
major clean-up.

Revision 1.13  93/08/27  20:39:01  milnes
Updated for timing.

Revision 1.12  1993/07/30  14:28:41  esb
fixed finalization and exception handling.

Revision 1.11  1993/07/16  18:26:34  esb
fixed a bug in finalize which was not closing the lower connection.
also rearranged debugging messages.

Revision 1.10  1993/07/12  20:38:49  esb
changed to work with the new dev.sig using Send_Packet and Receive_Packet

Revision 1.9  1993/06/24  00:25:37  esb
changed to do_if_debug, improved some debugging statements

Revision 1.8  1993/06/21  14:57:32  esb
changed definition of Connection_Died exception

Revision 1.7  1993/06/16  20:20:37  esb
we now correctly re-install the handler if the handler returns NONE

Revision 1.6  1993/06/16  19:58:27  esb
cleaned up a little

Revision 1.5  1993/06/16  09:00:35  esb
changed the definition of address_pattern to only include protocol type

Revision 1.4  1993/06/15  23:44:52  esb
adapted to new passive_open which returns handler option

Revision 1.3  1993/06/15  17:33:38  esb
minor changes: formatting, do_if_debug

Revision 1.2  1993/06/14  18:50:33  esb
added protocol number to address patterns

Revision 1.1  1993/06/10  23:06:53  milnes
Initial revision

	1.	functor types
*)

functor Ethernet (structure Device: DEVICE_PROTOCOL
		    sharing type Device.Incoming.T = Device.Outgoing.T
                  structure B: FOX_BASIS
	          val debug_level: int ref option): ETHERNET_PROTOCOL =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "eth.fun"
			   val makestring = Device.X.makestring)

(*
	2.	structure Eth_Number
*)

  structure Eth_Number: ETH_NUMBER =
   struct
    type T = Word48.word

    fun new n = n

    fun convert n = n

    fun equal (n1: T, n2: T) = n1 = n2

    fun hash n = Word.fromLargeWord (Word48.toLargeWord (Word48.>> (n, 0w20)))

    fun makestring address =
         let fun makestring_one byte = Word48.toString byte
	     val mask = Word48.fromInt 0xff
	     fun make_byte shift =
	          Word48.andb (Word48.>> (address, shift), mask)
	     val b0 = makestring_one (make_byte 0w40)
	     val b1 = makestring_one (make_byte 0w32)
	     val b2 = makestring_one (make_byte 0w24)
	     val b3 = makestring_one (make_byte 0w16)
	     val b4 = makestring_one (make_byte  0w8)
	     val b5 = makestring_one (make_byte  0w0)
	 in b0 ^ ":" ^ b1 ^ ":" ^ b2 ^ ":" ^ b3 ^ ":" ^ b4 ^ ":" ^ b5
	 end
   end

(*
	3.	structure Eth_Protocol
*)

  structure Eth_Protocol: KEY =
   struct
    type T = Word16.word
    val makestring = Integer.toString o Word16.toInt
    fun equal (a: T, b) = a = b
    val hash = Word.fromLargeWord o Word16.toLargeWord
   end

(*
	4.	structure Eth_Address
*)

  structure Eth_Address: ETH_ADDRESS =
   struct
    type eth_number = Eth_Number.T
    type eth_protocol = Eth_Protocol.T
    datatype address = Address of {eth: eth_number, proto: eth_protocol}
    type T = address
    fun makestring (Address {eth, proto}) =
         Eth_Number.makestring eth ^ "^" ^ Eth_Protocol.makestring proto
    fun hash (Address {eth, proto}) = Eth_Number.hash eth
    fun equal (a: T, b) = a = b
   end

(*
	5.	structure Eth_Pattern
*)

  structure Eth_Pattern: ETH_PATTERN =
   struct
    type eth_number = Eth_Number.T
    type eth_protocol = Eth_Protocol.T
    datatype pattern = Complete of {eth: eth_number, proto: eth_protocol}
                     | Partial of {proto: eth_protocol}
    type T = pattern
    fun makestring (Complete address) = Eth_Address.makestring
					  (Eth_Address.Address address)
      | makestring (Partial {proto}) = Integer.toString (Word16.toInt proto)
    fun hash (Complete address) = Eth_Address.hash
				    (Eth_Address.Address address)
      | hash (Partial {proto}) = Word.fromLargeWord (Word16.toLargeWord proto)
    fun equal (a: T, b) = a = b
   end

(*
	6.	other sub-structures
*)

   structure Unit =
    struct
     type T = unit
     fun makestring () = "()"
     fun hash () = 0
     fun equal ((), ()) = true
    end

(*
	7.	extension types
*)

  datatype eth_connection_extension = 
    Eth_Connection_Extension of
      {connection_address: Eth_Address.T}

  datatype eth_session_extension =
    Eth_Session_Extension of
      {local_address: Eth_Number.T,
       packets_sent: unit -> Word64.word,
       packets_received: unit -> Word64.word,
       failed_sends: unit -> Word64.word,
       packets_rejected: unit -> Word64.word,
       minimum_packet_size: Word.word,
       maximum_packet_size: Word.word}

(*
	8.	structure Eth_Header
*)

  structure Eth_Header = Eth_Header (structure In = Device.Incoming
				     structure Out = Device.Outgoing
				     structure B = B)

(*
	9.	functions for structure Connection
*)

  fun lower_setup setup = setup

  val min_eth_payload = 0w46
  val max_eth_payload = 0w1500

  datatype protocol_state =
      PS of Word48.word *
            Word64.word ref * Word64.word ref *
	    Word64.word ref * Word64.word ref

  fun init_proto (setup,
		  Device.S {extension = Device.Dev_Session_Extension
			                  {local_address = a, ...}, ...}, _) =
       let val a48 = Word48_Array.to a
	   val local_address = Word48_Array.U_Big.F.head a48
	   val zero64 = Word64.fromInt 0
	   val packets_sent = ref zero64
	   val packets_received = ref zero64
	   val failed_sends = ref zero64
	   val packets_rejected = ref zero64
	   val extension = Eth_Session_Extension
	                   {local_address = local_address,
			    packets_sent = fn _ => ! packets_sent,
			    packets_received = fn _ => ! packets_received,
			    failed_sends = fn _ => ! failed_sends,
			    packets_rejected = fn _ => ! packets_rejected,
			    minimum_packet_size = min_eth_payload,
			    maximum_packet_size = max_eth_payload}
	   val state = (local_address,
			packets_sent, packets_received,
			failed_sends, packets_rejected)
       in (PS state, extension)
       end

  fun fin_proto _ = ()

  fun resolve _ = SOME ()

  fun make_key (_, address, _, _) = address

  fun map_pattern (_, pattern, _) = SOME ((), ())

  fun match (_, Eth_Pattern.Complete address1, _, address2) =
       Eth_Address.equal
         (Eth_Address.Address address1, address2)
    | match (_, Eth_Pattern.Partial {proto = proto1}, _,
	     Eth_Address.Address {eth, proto = proto2}) =
       proto1 = proto2

  type connection_state = protocol_state

  fun init_connection (state, address, _) =
       (state, Eth_Connection_Extension {connection_address = address})

  fun fin_connection _ = ()

  val header_size = Eth_Header.size {self = Word48.fromInt 0,
				     peer = Word48.fromInt 0,
				     proto = Word16.fromInt 0}

  val one64 = Word64.fromInt 1

  fun send (Eth_Address.Address {eth, proto},
	    PS (local_address, packets_sent, _, failed_sends, _)) =
       let val header = {self = local_address, peer = eth, proto = proto}
	   val bytes = Device.Outgoing.uninitialized header_size
	   val _ = Eth_Header.marshal (bytes, header) 0w0
	   fun send_packet packet = 
	        let val packet_size = Device.Outgoing.size packet
		in if packet_size > max_eth_payload then
		    (failed_sends := Word64.+ (! failed_sends, one64);
		     Trace.print_raise (Device.X.Send "packet too large",
					SOME "send"))
		   else if packet_size < min_eth_payload then
		    (failed_sends := Word64.+ (! failed_sends, one64);
		     Trace.print_raise (Device.X.Send "packet too small",
					SOME "send"))
		   else
		    (packets_sent := Word64.+ (! packets_sent, one64);
		     [Device.Outgoing.join (bytes, packet)])
		end
       in
	  send_packet
       end

  val broadcast_eth = Word48.notb (Word48.fromInt 0)
  fun identify (_, PS (local_address, _, _, _, packets_rejected)) packet =
       (let val ({self, peer, proto}, _) = Eth_Header.unmarshal (packet, 0w0)
	in if peer = local_address then [] (* ignore packets from ourself *)
	   else [Eth_Address.Address {eth = peer, proto = proto},
		 Eth_Address.Address {eth = broadcast_eth, proto = proto}]
	end)
	 handle Eth_Header.Extern =>
	         (packets_rejected := Word64.+ (! packets_rejected, one64);
		  Trace.local_print "unable to unmarshal packet, discarding";
		  [])
	      | x =>
	         (packets_rejected := Word64.+ (! packets_rejected, one64);
		  Trace.print_raise_again (x, SOME "identify"))

  fun receive (_, PS (_, _, packets_received, _, _)) packet =
       let val (_, new_packet) = Device.Incoming.split (packet, header_size)
       in packets_received := Word64.+ (! packets_received, one64);
	  SOME new_packet
       end

  fun undelivered _ _ = ()

  fun lower_status (_, lower_key) status =
       Trace.local_print ("received status " ^ Device.Status.makestring status)

(*
	10.	structure Connection
*)

  structure Connection =
      Connection (structure Lower = Device
		  structure Setup = Device.Setup
		  structure Address = Eth_Address
		  structure Pattern = Eth_Pattern
		  structure Connection_Key = Eth_Address
		  structure Incoming = Device.Incoming
		  structure Outgoing = Device.Outgoing
		  structure Status = Unit
		  structure Count = Device.Count
		  structure X = Device.X
		  type connection_extension = eth_connection_extension
		  type listen_extension = unit
		  type session_extension = eth_session_extension
		  type connection_state = connection_state
		  type protocol_state = protocol_state
		  val lower_setup = lower_setup
		  val init_proto = init_proto
		  val fin_proto = fin_proto
		  val resolve = resolve
		  val make_key = make_key
		  val map_pattern = map_pattern
		  val match = match
		  val init_connection = init_connection
		  val fin_connection = fin_connection
		  val send = send
		  val identify = identify
		  val receive = receive
		  val undelivered = undelivered
		  val lower_status = lower_status
		  structure B = B
		  val module_name = "eth.fun"
		  val debug_level = debug_level)

(*
	11.	export objects declared in signature
*)

  open Connection

 end (* struct *)

