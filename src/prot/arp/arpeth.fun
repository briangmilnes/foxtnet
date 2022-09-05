(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract
	arpeth.fun: ARP for ethernet


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Arp_Eth
	2.	internal structures
	3.	internal types and values
	4.	structure Arp
	5.	exported types and values


	iii.	RCS Log

$Log: arpeth.fun,v $
Revision 1.30  1996/04/18  21:29:38  cline
converted hash from int to word

Revision 1.29  1996/02/07  19:15:48  cline
added null_hardware_address

Revision 1.28  1996/01/19  23:05:07  esb
adapted to the new wordarray signature.

Revision 1.27  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.26  1995/11/12  16:39:13  esb
adapted to new Word_Array.

Revision 1.25  1995/09/14  21:12:37  cline
work around for representation bug

Revision 1.24  1995/08/08  18:24:20  esb
separated in and out external structures.

Revision 1.23  1995/06/27  19:07:10  cline
adapted to new extern.sig

Revision 1.22  1995/06/23  15:36:48  cline
changed broadcast_pattern to use Partial instead of Complete

Revision 1.21  1995/06/20  16:57:52  esb
adapted to new protocol signature.

Revision 1.20  1995/03/07  23:51:23  esb
updated tracing.

Revision 1.19  1995/02/04  20:39:33  robby
updated to 107

Revision 1.18  1995/01/06  01:38:15  esb
added a longer timeout when debugging.

Revision 1.17  1994/11/11  18:18:52  esb
changed the functor parameters for Debug_Trace_Printer.

Revision 1.16  1994/11/10  16:12:20  milnes
Updated for tcpipeth/addressing and debug_trace structure.

Revision 1.15  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.14  1994/08/12  06:25:46  esb
added enable/disable_lower to arp's functor parameters.

Revision 1.13  1994/08/02  20:29:33  esb
adapted to new protocol signature.

Revision 1.12  1994/07/01  02:35:41  danwang
Moved control structures into Fox_Basis.

Revision 1.11  1994/06/16  16:46:40  danwang
Updated to use functorized Fox_Basis

Revision 1.10  1994/04/06  23:22:36  esb
adapted to new receive_packet interface.

Revision 1.9  94/02/21  00:03:56  esb
modified send to adapt to new send_packet interface.

Revision 1.8  93/10/27  01:19:26  esb
simplified

Revision 1.7  1993/10/26  22:44:49  esb
added a protocol number argument to bytes_to_lower, which was needed.

Revision 1.6  1993/10/25  19:36:38  cline
removed .U from Byte[421].U

Revision 1.5  1993/10/09  17:49:36  esb
consolidated the protocol state; we now use the new store interface.

Revision 1.4  93/09/22  19:31:56  milnes
Removed "-----------"s.

Revision 1.3  1993/09/13  22:07:45  cline
deleted '#'s from RCS log

Revision 1.2  1993/09/02  19:40:55  esb
adapted to new PROTOCOL signature.

Revision 1.1  1993/08/24  21:20:29  esb
Initial revision

	1.	functor Arp_Eth
*)

functor Arp_Eth (structure Eth: ETHERNET_PROTOCOL
		   sharing type Eth.Incoming.T = Eth.Outgoing.T
		 val arp_protocol_number: Word16.word
		 structure B: FOX_BASIS
	         val debug_level: int ref option):
                ADDRESS_RESOLUTION_PROTOCOL =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "arp.fun(eth)"
			   val makestring = Eth.X.makestring)

(*
	2.	internal structures
*)

  structure Hardware_Address =
      Protocol_Extern48_Big (structure In = Eth.Incoming
			     structure Out = Eth.Outgoing
			     structure B = B)

  structure Protocol_Address: ARP_PROTOCOL_EXTERN =
   struct
    type T = Word_Array.T
    type regular_extern_in = Eth.Incoming.T
    type protocol_extern_in = regular_extern_in * Word.word
    type extern_in = protocol_extern_in
    type extern_out = Eth.Outgoing.T
    type cursor = Word.word
    exception Extern
    val size = Word_Array.W8.U_Big.F.length o Word_Array.to8
    fun marshal (outgoing, data) cursor =
	 (Eth.Outgoing.update (outgoing, cursor, data);
	  size data + cursor)
    fun unmarshal ((incoming, length), position) =
         (Eth.Incoming.sub (incoming, {start = position, length = length}),
	  position + length)
    fun equal (array1, array2) =
         case (Word_Array.W8.U_Big.F.next (Word_Array.to8 array1),
	       Word_Array.W8.U_Big.F.next (Word_Array.to8 array2)) of
	    (NONE, NONE) => true
	  | (SOME (f1, r1), SOME (f2, r2)) =>
	     f1 = f2 andalso equal (Word_Array.from8 r1, Word_Array.from8 r2)
	  | _ => false
    fun hash array =
         case Word_Array.W8.U_Big.F.next (Word_Array.to8 array) of
	    NONE => 0w0
	  | SOME (first, rest) =>
	     Word.fromLargeWord (Word8.toLargeWord first) +
	     hash (Word_Array.from8 rest)
    fun makestring array =
         case Word_Array.W8.U_Big.F.next (Word_Array.to8 array) of
	    NONE => ""
	  | SOME (first, rest) =>
	     Word8.toString first ^ "." ^
	     makestring (Word_Array.from8 rest)
   end

(*
	3.	internal types and values
*)

  val null_hardware_address = Word48.fromInt 0

  fun local_hardware_address
    (Eth.Eth_Session_Extension {local_address, packets_sent, packets_received,
				failed_sends, packets_rejected,
				minimum_packet_size, maximum_packet_size}) =
       local_address

  fun min_max_packet
    (Eth.Eth_Session_Extension {local_address, packets_sent, packets_received,
				failed_sends, packets_rejected,
				minimum_packet_size, maximum_packet_size}) =
       (minimum_packet_size, maximum_packet_size)

  fun broadcast_address protocol =
    Eth.Eth_Address.Address {eth = Word48.notb (Word48.fromInt 0),
			     proto = protocol}

  fun broadcast_pattern protocol = 
    Eth.Eth_Pattern.Partial {proto = protocol}

  fun lower_address (eth_number, eth_protocol) =
    Eth.Eth_Address.Address {eth = eth_number, proto = eth_protocol}

  fun lower_pattern (eth_number, eth_protocol) =
    Eth.Eth_Pattern.Complete {eth = eth_number, proto = eth_protocol}

  val hardware_type = Word16.fromInt 1

  val arp_timeout = if Trace.debug_on () then 1000 else 100
  val arp_resend = 5

  val lower_header_size = 14

(*
	4.	structure Arp
*)

  structure Arp: ADDRESS_RESOLUTION_PROTOCOL =
       Arp (structure Lower = Eth
	    structure Hardware_Address = Hardware_Address
	    structure Protocol_Address = Protocol_Address
	    val null_hardware_address = null_hardware_address
	    val local_hardware_address = local_hardware_address
	    val arp_protocol_number = arp_protocol_number
	    val broadcast_address = broadcast_address
	    val broadcast_pattern = broadcast_pattern
	    val lower_address = lower_address
	    val lower_pattern = lower_pattern
	    val min_max_packet = min_max_packet
	    val hardware_type = hardware_type
	    val arp_timeout = arp_timeout
	    val arp_resend = arp_resend
	    structure B = B
	    structure Trace = Trace) 

(*
	5.	exported types and values
*)

  open Arp

 end (* struct *)
