(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This file, sim.fun, provides a simulated ethernet driver with
	a possibly large number of simulated hosts.  It requires
	wire.fun to work.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Ethernet_Device_Simulator

		iii.	RCS Log
	
$Log: sim.fun,v $
Revision 1.44  1996/04/18  21:26:59  cline
converted hash from int to word

Revision 1.43  1996/02/23  21:34:30  esb
changed debugging statement.

Revision 1.42  1996/01/19  23:04:53  esb
adapted to the new wordarray signature.

Revision 1.41  1996/01/15  20:01:03  cline
updated for 108.19 sml

Revision 1.40  1995/11/12  16:36:57  esb
made the setup type be string, adapted to new Word_Array.

Revision 1.39  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.38  1995/09/18  19:30:07  esb
adapted to new dev.sig.

Revision 1.37  1995/06/29  18:20:22  esb
adapted to new wordarray.

Revision 1.36  1995/06/20  16:48:58  esb
converted to new protocol signature.

Revision 1.35  1995/03/12  17:53:50  esb
adapted to new trace.sig.

Revision 1.34  1995/03/07  20:37:42  esb
updated tracing.

Revision 1.33  1995/02/04  20:39:39  robby
updated to 107

Revision 1.32  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.31  1994/11/07  21:34:12  cline
use V.Print

Revision 1.30  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.29  1994/09/12  18:20:13  milnes
Added prints to the handle _'s.

Revision 1.28  1994/08/28  21:42:54  milnes
 Added an io flush to try and show a gc bug.

Revision 1.27  1994/08/24  22:24:19  esb
now use fixed-sized packets for data, to more closely emulate ethdev

Revision 1.26  1994/08/12  06:18:18  esb
added hash_* , equal_*, and set_arp_filter

Revision 1.25  1994/08/02  20:25:36  esb
adapted to new protocol signature.

Revision 1.24  1994/07/04  21:34:16  esb
adapted to Copy/Create split.

Revision 1.23  1994/07/01  02:34:34  danwang
Moved control structures into Fox_Basis.

Revision 1.22  1994/06/16  16:45:05  danwang
Updated to use functorized Fox_Basis

Revision 1.21  1994/06/05  18:44:40  milnes
Added vacuous arp filter support.

Revision 1.20  1994/05/23  14:04:22  milnes
Added print function.

Revision 1.19  1994/04/06  23:21:50  esb
adapted to new receive_packet interface.

Revision 1.18  94/03/10  19:41:00  esb
used Copy.create.

Revision 1.17  94/03/07  13:11:54  esb
adapted to changed dev.sig.

Revision 1.16  94/03/02  21:21:47  esb
added a print statement before raising exceptions.

Revision 1.15  94/01/30  18:15:48  milnes
added statistics.

Revision 1.14  1994/01/18  04:16:52  esb
changed the format of the local_address functor parameter.

Revision 1.13  1993/12/06  19:11:04  esb
brought module up to date with dev.sig.

Revision 1.12  1993/12/04  20:56:05  esb
now provide a handler with a parameter of type connection.

Revision 1.11  1993/10/29  05:39:02  esb
got rid of the hand-coded copy function and replaced it with Copy.copy.

Revision 1.10  1993/10/25  19:35:47  cline
removed .U from Byte[421].U

Revision 1.9  1993/10/25  17:44:38  milnes
Exported safe_msg and zero_timeout to be consistent with the new ethernet device format.

Revision 1.8  1993/09/02  15:54:16  esb
added the Fox Basis B.

Revision 1.7  1993/07/22  18:28:24  nickh
Adjusted offsets for sub and update to use byte offsets.

Revision 1.6  1993/07/12  20:38:17  esb
changed to work with the new dev.sig using Send_Packet and Receive_Packet

Revision 1.5  1993/06/21  14:57:58  esb
changed definition of Connection_Died exception

Revision 1.4  1993/06/15  23:21:33  esb
integrated new passive_open in which the handler may fail

Revision 1.3  1993/06/15  17:16:04  esb
fixed a bug that copied byte 13 into byte 15

Revision 1.2  1993/06/15  13:14:06  esb
added exception packet_size

Revision 1.1  1993/06/10  23:05:01  milnes
Initial revision


		1.	functor Ethernet_Device_Simulator
*)

functor Ethernet_Device_Simulator (val local_address: Word48.word
				   structure Wire: WIRE
				   structure B: FOX_BASIS
				   val debug_level: int ref option
				   val name: string): RAW_DEVICE =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "sim.fun (" ^ name ^ ")"
			   val makestring = fn _ => NONE)

  structure External = Protocol_External (structure B = B
					  val debug_level = debug_level)

  fun send packet =
       (Trace.debug_print (fn _ => "sending packet of size " ^
			   Word.toString (External.size packet) ^
			   ", packet is (first 60 bytes) " ^
			   External.makestring_max (packet, 0w60));
	if External.size packet < 0w60 then
	 Trace.local_print ("packet " ^
			    External.makestring_max (packet, 0w60) ^
			    " in ethernet simulator has size " ^
			    Word.toString (External.size packet) ^
			    ", required minimum size is 60")
	else
	 let val size = External.size packet
	     val data = External.sub (packet, {start = 0w0, length = size})
	 in Trace.debug_print (fn _ => "sending packet of size " ^
			       Word.toString size ^
			       ", data size " ^
			       Word.toString
			          (Word_Array.W8.U_Big.F.length
				   (Word_Array.to8 data)));
	    Wire.send data
	 end)

  structure Setup =
   struct
    type T = string
    fun makestring s = s
    fun equal (a: T, b) = a = b
    fun hash _ = 0w0
   end (* struct *)

  type session =
        {send: External.T -> unit,
         local_address: Word_Array.T,
         packets_sent: unit -> Word64.word,
         packets_received: unit -> Word64.word,
         read_timeouts: unit -> Word64.word,
         failed_sends: unit -> Word64.word,
         packets_rejected: unit -> Word64.word}

  exception Session_Already_Open

  fun session (_, handler) =
       let val address = Word48_Array.from
	                    (Word48_Array.Big.F.create (local_address, 0w1))
	   fun zero64 () = Word64.fromInt 0
	   val session_value = {send = send,
				local_address = address,
				packets_sent = zero64,
				packets_received = zero64,
				read_timeouts = zero64,
				packets_rejected = zero64,
				failed_sends = zero64}
	   val (connection, receive) = handler session_value
       in ((Wire.register (address, receive o External.new))
	   handle x => (Trace.print_handled (x, NONE);
			Trace.print_raise (Session_Already_Open, NONE)));
	  ((connection ()
	    before Wire.unregister address)
	   handle x => (Wire.unregister address;
			Trace.print_raise_again (x, NONE)))
       end

 end (* struct *)
