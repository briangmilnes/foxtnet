(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Brian Milnes (milnes@cs.cmu.edu)
	Nick Haines (nickh@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	devreal.tst: a simple but real test of ethdev and the packet
	filter.  On the MIPS, must be run as root.  On every machine,
	it must be run from the console.


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	iv.	Overview
	1.	functor Dev_Real
	2.	constants
	3.	functions for definition of outgoing and incoming values
	4.	internal structure Dev
	5.	comparison function
	6.	timeout function
	7.	function send_arp_request
	8.	function checksum
	9.	function send_icmp_echo
	10.	structure Dev_Real


	iii.	RCS Log

$Log: devreal.tst,v $
Revision 1.13  1995/09/19  15:52:15  esb
working version.

Revision 1.12  1995/09/18  19:30:07  esb
first running version.

Revision 1.11  1995/03/12  17:53:50  esb
adapted to new trace.sig.

Revision 1.10  1995/03/07  20:37:42  esb
updated tracing.

Revision 1.9  1995/02/04  20:39:39  robby
updated to 107

Revision 1.8  1995/01/18  21:03:24  esb
adapted to new COROUTINE signature.

Revision 1.7  1995/01/16  23:48:16  esb
added a test of the device filters.

Revision 1.6  1995/01/14  02:30:25  esb
adapted to new filter interface.

Revision 1.5  1995/01/06  23:19:33  esb
adapted to new eth.sig.

Revision 1.4  1994/11/22  13:56:55  milnes
Updated to initialize its own addressing so that the tests would run.

Revision 1.3  1994/10/27  20:26:26  cline
changed Buffer.buf to Bytearray.bytearray

Revision 1.2  1994/10/20  14:37:05  cline
support SML/NJ 105b

Revision 1.1  1994/08/19  17:44:14  esb
Initial revision


	iv.	Overview

	Construct and send an ARP packet, and watch for it to come
	back.  Then construct and send an ICMP echo packet, and watch
	for it to come back.

	This should test sending, filtering, and receiving; right now,
	filtering is not tested.  On kitsune, about 1/2 the calls seem
	to complete successfully, the other half don't.

	1.	functor Dev_Real
*)

functor Dev_Real (structure B: FOX_BASIS
		  val debug_level: int ref option) =
 struct
  local
   structure Trace = Trace (structure V = B.V
			    val debug_level = debug_level
			    val module_name = "devreal.tst"
			    val makestring = fn _ => NONE)

(*
	2.	constants
*)

   val ethernet_ip = SW.n16 "0x800"
   val ethernet_arp = SW.n16 "0x806"
   val ip_icmp = SW.n8 "0x1"
   val arp_hw = SW.n16 "1"
   val ethernet_addr_size = SW.n8 "6"
   val ip_addr_size = SW.n8 "4"
   val arp_request = SW.n16 "1"
   val arp_reply = SW.n16 "2"
   val eth_zero = SW.n48 "0"
   val eth_bcast = SW.n48 "0xffffffffffff"

   val ip_ver_hlen = SW.n8 "0x45"
   val ip_tos = SW.n8 "0"
   val ip_ttl = SW.n8 "0xff"
   val ip_icmp = SW.n8 "1"
   val icmp_echo_request = SW.n8 "8"
   val icmp_echo_reply = SW.n8 "0"
   val icmp_echo_code = SW.n8 "0"

   val zero16 = SW.n16 "0"
   val zero8 = SW.n8 "0"
   val one8 = SW.n8 "1"
   val two8 = SW.n8 "2"
   val three8 = SW.n8 "3"
   val four8 = SW.n8 "4"
   val five8 = SW.n8 "5"
   val six8 = SW.n8 "6"
   val seven8 = SW.n8 "7"

(*
	3.	functions for definition of outgoing and incoming values
*)

   datatype header_data = W8 of FoxWord8.word |
                          W16 of FoxWord16.word |
                          W32 of FoxWord32.word |
                          W48 of FoxWord48.word |
                          W64 of FoxWord64.word

   fun split_bytes (word, create, constructor, convert) =
        let val wa = create (word, 1)
	    val ba = convert (constructor wa)
	    fun loop NONE = []
	      | loop (SOME (first, rest)) =
	         W8 first :: loop (Word_Array.W8.next rest)
	in loop (Word_Array.W8.next ba)
	end

   fun marshal [] = NONE
     | marshal (W8 word :: rest) = SOME (word, rest)
     | marshal (W16 word :: rest) =
        marshal (split_bytes (word, Word_Array.W16.Big.create,
			      Word_Array.Array16, Word_Array.convert8) @ rest)
     | marshal (W32 word :: rest) =
        marshal (split_bytes (word, Word_Array.W32.Big.create,
			      Word_Array.Array32, Word_Array.convert8) @ rest)
     | marshal (W48 word :: rest) =
        marshal (split_bytes (word, Word48_Array.Big.create,
			      fn a => a, Word48_Array.to8) @ rest)
     | marshal (W64 word :: rest) =
        marshal (split_bytes (word, Word_Array.W64.Big.create,
			      Word_Array.Array64, Word_Array.convert8) @ rest)

(*
	4.	internal structure Dev
*)

   local
    structure Base = Build_Eth_Dev (structure B = B
				    val high_priority_filter = true
				    val debug_level = debug_level)
   in
    structure Dev = Base.Raw
   end (* local *)

(*
	5.	comparison function
*)

  fun equal8 (a: FoxWord8.word, b) = a = b
  structure Compare = Compare_Seq (structure Seq = Word_Array.W8
				   val equal = equal8
				   val makestring = FoxMakestring.word8
				   structure V = B.V)

  fun compare (a, b) =
       let val len = Dev.External.size a
	   val wa = if len > 0 then
	             Dev.External.sub (a, {start = 0,
					   length = Word_Array.W8.length b})
		    else Word_Array.W8.new (fn _ => NONE) ()
       in Compare.compare (wa, b)
       end

(*
	6.	timeout function
*)

  fun send_timeout data () =
       (B.Scheduler.sleep 1000;
	B.Pipe.enqueue (data, Dev.External.uninitialized 0))

(*
	7.	function send_arp_request
*)

   fun send_arp_request (send, data, eth_local, eth_dest, ip_local, ip_dest) =
        let val arp_out = [W48 eth_bcast, W48 eth_local, W16 ethernet_arp,
			   W16 arp_hw, W16 ethernet_ip, W8 ethernet_addr_size,
			   W8 ip_addr_size, W16 arp_request, W48 eth_local,
			   W32 ip_local, W48 eth_zero,  W32 ip_dest]
	    val arp_in  = [W48 eth_local, W48 eth_dest,  W16 ethernet_arp,
			   W16 arp_hw, W16 ethernet_ip, W8 ethernet_addr_size,
			   W8 ip_addr_size, W16 arp_reply,   W48 eth_dest,
			   W32 ip_dest,  W48 eth_local, W32 ip_local]
	    val out_data = Word_Array.W8.new marshal arp_out
	    val in_data  = Word_Array.W8.new marshal arp_in
	in B.Scheduler.fork (send_timeout data);
	   send (Dev.External.new out_data);
	   case compare (B.Pipe.dequeue data, in_data) of
	      "" => B.Test.test ("ARP response", fn _ => true)
	    | s =>
	       (Trace.local_print ("difference between packet received " ^
				   "and expected is " ^ s);
		B.Test.test ("ARP response", fn _ => false))
	end

(*
	8.	function checksum
*)

   fun checksum (data, start, length, check_position) =
        let val wanted = Word_Array.W8.Rev.seek
	                    (Word_Array.W8.seek (data, start),
			     Word_Array.W8.length data - (start + length))
	    val check = B.Checksum.one_s_complement
	                   (B.Checksum.checksum wanted)
	    val write_pos = Word_Array.W8.seek (data, check_position)
	    val (write_pos16, _) = Word_Array.convert16 (Word_Array.Array8
							 write_pos)
	in case Word_Array.W16.Big.write write_pos16 of
	      NONE =>
	       (Trace.local_print ("error, checksum position " ^
				   FoxMakestring.int check_position ^
				   " not in data " ^
				   B.Format.makestring data);
		B.Test.test ("checksum", fn _ => false))
	    | SOME u =>
	       (Word_Array.W16.Big.update (u, check);
		())
	end

(*
	9.	function send_icmp_echo
*)

   fun send_icmp_echo (send, data_pipe,
		       eth_local, eth_dest, ip_local, ip_dest) =
        let val ip_len = 20 (* ip header *) + 8 (* icmp *) + 8 (* data *)
	    val ip_len16 = FoxWord16.intToWord ip_len
	    val arp_out = [W48 eth_dest, W48 eth_local, W16 ethernet_ip,
			   (* IP header *)
			   W8 ip_ver_hlen, W8 ip_tos, W16 ip_len16,
			   W16 zero16, W16 zero16, W8 ip_ttl, W8 ip_icmp,
			   W16 zero16, W32 ip_local, W32 ip_dest,
			   (* ICMP header *)
			   W8 icmp_echo_request, W8 icmp_echo_code,
			   W16 zero16, W16 zero16, W16 zero16, 
			   (* data *)
			   W8 zero8, W8 one8, W8 two8, W8 three8,
			   W8 four8, W8 five8, W8 six8, W8 seven8]
	    val arp_in  = [W48 eth_local, W48 eth_dest, W16 ethernet_ip,
			   (* IP header *)
			   W8 ip_ver_hlen, W8 ip_tos, W16 ip_len16,
			   W16 zero16, W16 zero16, W8 ip_ttl, W8 ip_icmp,
			   W16 zero16, W32 ip_dest, W32 ip_local,
			   (* ICMP header *)
			   W8 icmp_echo_reply, W8 icmp_echo_code,
			   W16 zero16, W16 zero16, W16 zero16, 
			   (* data *)
			   W8 zero8, W8 one8, W8 two8, W8 three8,
			   W8 four8, W8 five8, W8 six8, W8 seven8]
	    val out_data = Word_Array.W8.new marshal arp_out
	    val in_data  = Word_Array.W8.new marshal arp_in
	in checksum (out_data, 14, 20, 24);
	   checksum (out_data, 34, 16, 36);
	   B.Scheduler.fork (send_timeout data_pipe);
	   send (Dev.External.new out_data);
	   (* for some reason, we seem to get back our own packet.  Since
	      the filter is not in promiscuous mode, this should not happen.
	      Nonetheless, dequeueing our own packet before the response
	      lets the test work, and this should not substantially bother
	      the foxnet. *)
	   let val received0 = B.Pipe.dequeue data_pipe
	       val received = if compare (received0, out_data) = "" then
			       B.Pipe.dequeue data_pipe
			      else received0
	       fun update (NONE, _) = ()
		 | update (_, NONE) = ()
		 | update (SOME new, SOME (first, rest)) =
		    update (Word_Array.W8.update (new, first),
			    Word_Array.W8.next rest)
	       val data = ((Dev.External.sub (received,
					      {start = 18, length = 4}))
			   handle x =>
			           (Trace.local_print ("received packet " ^
						       Dev.External.makestring
						       received ^
						       ", not long enough");
				    B.Test.test ("ICMP response",
						 fn _ => false);
				    Word_Array.W8.create (SW.n8 "0", 100)))
			   
	   in update (Word_Array.W8.write (Word_Array.W8.seek (in_data, 18)),
		      Word_Array.W8.next data);
	      checksum (in_data, 14, 20, 24);
	      checksum (in_data, 34, 16, 36);
	      case compare (received, in_data) of
		 "" => B.Test.test ("ICMP response", fn _ => true)
	       | s =>
		 (Trace.local_print ("difference between packet received " ^
				     "and expected is " ^ s ^
				     "\nreceived: " ^
				     Dev.External.makestring_max
				        (received, ip_len + 14) ^
				     "\nexpected: " ^
				     B.Format.makestring in_data);
		  B.Test.test ("ICMP response", fn _ => false))
	   end
	end

   exception Local_Host_Not_In_Test_Addresses
   exception Remote_Host_Not_In_Test_Addresses
   exception Remote_Host_Eth_Not_In_Test_Addresses

   fun run_session (dest_name, local_address, send, data_pipe) () =
        let val eth_local = local_address
	    val ip_local = case Test_Addresses.eth_ip eth_local of
		              NONE =>
			       raise Local_Host_Not_In_Test_Addresses
			    | SOME ip => ip
	    val ip_dest = case Test_Addresses.name_ip dest_name of
	                     NONE =>
			      raise Remote_Host_Not_In_Test_Addresses
			   | SOME ip => ip
	    val eth_dest = case Test_Addresses.ip_eth ip_dest of
	                      NONE =>
			       raise Remote_Host_Eth_Not_In_Test_Addresses
			    | SOME eth => eth
	in send_arp_request (send, data_pipe,
			     eth_local, eth_dest, ip_local, ip_dest);
	   send_icmp_echo (send, data_pipe,
			      eth_local, eth_dest, ip_local, ip_dest)
	end

   fun receive data_pipe data =
        let val head = Dev.External.sub (data, {start = 0, length = 6})
	    val (head48, _) = Word48_Array.from8 head
	    val dest = Word48_Array.Big.head head48
	in if dest <> eth_bcast then B.Pipe.enqueue (data_pipe, data)
	   else ()
	end

   fun create_session dest_name
                      {send, local_address, packets_sent, packets_received,
		       read_timeouts, failed_sends, packets_rejected} =
	let val data_pipe = B.Pipe.new ()
	    val (local48, _) = Word48_Array.from8 local_address
	    val local_eth = Word48_Array.Big.head local48
	in (run_session (dest_name, local_eth, send, data_pipe),
	    receive data_pipe)
	end

  in (* local *)
   fun run dest_name =
        B.Test.tests ("DevReal", 2,
		      fn _ => Dev.session (create_session dest_name))
  end (* local *)

 end (* struct *)

(*
 ---------------------------------------------------------------------
	10.	structure Dev_Real
*)

structure Dev_Real = Dev_Real (structure B = Fox_Basis
			       val debug_level = NONE)
