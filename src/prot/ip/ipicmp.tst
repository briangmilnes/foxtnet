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
	iv.	Overview
	1.	functor Test_Ip_Icmp
	2.	protocol stack
	3.	protocol parameters
	4.	server
	5.	client
	6.	function run
	7.	structure Test_Ip_Icmp

	iii.	RCS Log

$Log: ipicmp.tst,v $
Revision 1.9  1996/05/08  02:03:35  esb
major cleanup

Revision 1.8  1996/04/30  20:27:38  esb
adapted to new ip.sig.

Revision 1.7  1996/02/06  23:39:33  esb
adapted to new WORD_ARRAY signature (using words instead of ints).

Revision 1.6  1995/11/29  22:24:29  esb
made it work with the new word-arrays.

Revision 1.5  1995/10/17  22:31:32  esb
changed some debugging statements.

Revision 1.4  1995/10/04  21:33:06  esb
many changes for reliability.

Revision 1.3  1995/09/26  16:31:06  esb
added a repetition of the test.

Revision 1.2  1995/09/19  18:50:01  esb
adapted to new signatures.

Revision 1.1  1995/09/16  17:45:00  esb
Initial revision


	iv.	Overview

This module tests some of the paths required of IP by RFC 1122, as
well as some optional stuff that has been implemented by Fox Net IP
(In what follows, "silently" means without generating any ICMP
messages.)

The complete list of requirements is:

 1. "silently" (i.e. without network action) discard version != 4
 2. verify IP checksum, silently discard bad datagram
 3. subnet addressing
 4. Src address must be host's own IP address
 5. Silently discard datagram with bad dest addr
 6. Silently discard datagram with bad src addr
 7. Reassemble fragments
 8. Do not send packet with TTL of 0
 9. Do not discard received packets with TTL < 2
10. Pass all IP options rcvd to higher layer
11. IP layer silently ignores unknown options
12. Silently ignore Stream Identifer option
13. Originate & terminate Source Route options
14. Datagram with completed SR passed up to TL
15. Build correct (non-redundant) return route
16. Do not send multiple SR options in one header
17. Silently discard ICMP msg with unknown type
18. Included octets in ICMP msg same as received
19. Demux ICMP Error to transport protocol
20. Do not send ICMP error message for:
   a ICMP error msg
   b IP b'cast or IP m'cast
   c Link-layer b'cast
   d Non-initial fragment
   e Datagram with non-unique src address
21. Return ICMP error msgs (when not prohibited)
22. Pass ICMP Dest Unreachable to higher layer
23. Update route cache when recv Redirect
24. Handle both Host and Net Redirects
25. Pass Source Quench to higher layer
26. Time Exceeded: pass to higher layer
27. Pass Parameter Problem to higher layer
28. Serve Echos Requests
29. Use specific-dest addr as Echo Reply src
30. Send same data in Echo Reply
31. Pass Echo Reply to higher layer
32. Reverse and reflect Source Route option
33. Use specific-dest addr as Timestamp Reply src
34. Reverse and reflect Source Route option
35. Pass Timestamp Reply to higher layer
36. Obey rules for "standard time value"
37. Addr Mask source configurable
38. Support static configuration of addr mask
39. Retransmit Addr Mask Req if no Reply
40. Update address mask from first Reply only
41. Explicitly configured to be address mask agent
42. Broadcast Addr Mask Reply when init.
43. Use address mask in local/remote decision
44. Operate with no gateways on conn network
45. Maintain "route cache" of next-hop gateways
46. If no cache entry, use default gateway
47. Support multiple default gateways
48. Able to detect failure of next-hop gateway
49. Ping gateway only when traffic being sent
50. Ping gateway only when no positive indication
51. Switch from failed default gateway to another
52. Manual method of entering config info
53. Able to reassemble incoming datagrams
54. At least 576 byte datagrams
55. EMTU_R configurable or indefinite
56. Transport layer able to learn MMS_R
57. Send ICMP Time Exceeded on reassembly timeout
58. Pass MMS_S (maximum send transport-message size) to higher layers
59. Allow application to choose local IP addr
60. Recognize all broadcast address formats
61. Use IP b'cast/m'cast addr in link-layer b'cast
62. Allow transport layer to use options, TOS, TTL
63. Transport layer can send Port Unreachable, Echo Request, Timestamp Request
64. Pass Unreachable, Quench, Echo/Timest Reply, Time Exceeded to transp layer

We test:
     Ability to send and receive echo
     Ability to send and receive timestamp
     Ability to generate response when receiving
          packet for unregistered protocol
     Ability to generate response when receiving packet that does
          not result in opening a connection.

	1.	functor Test_Ip_Icmp
*)

functor Test_Ip_Icmp (structure B: FOX_BASIS
		      val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = NONE
			   val module_name = "ipicmp.tst"
			   val makestring = fn _ => NONE)

(*
	2.	protocol stack
*)

  val arp_protocol = Word16.fromInt 0x806
  val ip_protocol = Word16.fromInt 0x800
  val interface = "ln0"

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
			       val arp_setup = ()
			       val interface_name = interface
			       val ip_protocol_number = ip_protocol
			       structure B = B)

  structure Sly_Ip = Ip (structure Lower = Sly_Mux
			 structure Host_Id = Ip_Host_Id
			 structure B = B
			 val icmp_protocol = Word8.fromInt 1
			 val gateway = false
			 val debug_level = NONE)

  structure Snow_Eth = Ethernet (structure Device = Sim.Snow
				 structure B = B
				 val debug_level = debug_level)

  structure Snow_Arp = Arp_Eth (structure Eth = Snow_Eth
				val arp_protocol_number = arp_protocol
				structure B = B
				val debug_level = debug_level)

  structure Snow_Mux = Ip_Mux1 (structure Arp = Snow_Arp
				val arp_setup = ()
				val interface_name = interface
				val ip_protocol_number = ip_protocol
				structure B = B)

  structure Snow_Ip = Ip (structure Lower = Snow_Mux
			  structure Host_Id = Ip_Host_Id
			  structure B = B
			  val icmp_protocol = Word8.fromInt 1
			  val gateway = false
			  val debug_level = NONE)

(*
	3.	protocol parameters
*)

  val sly_mask = Word32.<< (Word32.fromInt 0xffff80, 0w8)
  val sly_setup = Sly_Ip.Network_Setup.Setup
                   [{local_id = Test_Addresses.get_ip "sly",
		    interface = interface, gateways = [],
		    mask = SOME (sly_mask, {serve = true}),
		    mtu = NONE}]
  val snow_setup = Snow_Ip.Network_Setup.Setup
                    [{local_id = Test_Addresses.get_ip "snow",
		      interface = interface, gateways = [],
		      mask = NONE, mtu = NONE}]

  val test_protocol = Word8.fromInt 0x11
  fun make_address (ip_number, protocol) =
       {peer = ip_number, proto = protocol}

  val sly = make_address (Test_Addresses.get_ip "sly", test_protocol)
  val snow = make_address (Test_Addresses.get_ip "snow", test_protocol)
  val snow_pat = Snow_Ip.Network_Pattern.Complete sly
  val snow_pat1 = Snow_Ip.Network_Pattern.Partial {proto = test_protocol}
  val sly_pat = Sly_Ip.Network_Pattern.Partial {proto = test_protocol}

  fun equal_packet (packet1, packet2) =
       let val len1 = Sly_Ip.Incoming.size packet1
           val len2 = Sly_Ip.Incoming.size packet2
	   val w1 = Sly_Ip.Incoming.sub (packet1, {start = 0w0, length = len1})
	   val w2 = Sly_Ip.Incoming.sub (packet2, {start = 0w0, length = len2})
	   val result = Word_Array.W8.U_Big.F.equal (Word_Array.to8 w1,
						     Word_Array.to8 w2)
       in if result then result
	  else
	   (Trace.local_print ("received data " ^
			       Sly_Ip.Incoming.makestring packet1 ^
			       ", expected " ^
			       Sly_Ip.Incoming.makestring packet2);
	    result)
       end

  fun equal_status (Sly_Ip.Network_Status.Quench (packet1, ()),
		    Sly_Ip.Network_Status.Quench (packet2, ())) =
       equal_packet (packet1, packet2)
    | equal_status (Sly_Ip.Network_Status.Unreachable (kind1, packet1),
		    Sly_Ip.Network_Status.Unreachable (kind2, packet2)) =
       kind1 = kind2 andalso equal_packet (packet1, packet2)
    | equal_status (status1, status2) =	(* ignore statuses we aren't testing *)
       (Trace.local_print ("received status " ^
			   Sly_Ip.Status.makestring status1 ^
			   ", expected " ^
			   Sly_Ip.Status.makestring status2);
	false)

(*
	4.	server
*)

  structure Server_App =
   struct
    fun conn_fun (pipe, done_pipe)
                 (Sly_Ip.C {send, abort, extension}) =
         let val data = Word_Array.from8 (Word_Array.W8.U_Big.F.tabulate
					  (Word8.fromInt o Word.toInt, 0w16))
	     val packet = Sly_Ip.Incoming.new data
	     val status1 = Sly_Ip.Network_Status.Quench (packet, ())
	     val status2 = Sly_Ip.Network_Status.Unreachable
	                    (Sly_Ip.Network_Status.Parameter_Problem 1, packet)
	     val status3 = Sly_Ip.Network_Status.Unreachable
	                    (Sly_Ip.Network_Status.Missing_Option, packet)
	 in send (Sly_Ip.Outgoing.new data);
	    B.Pipe.enqueue (pipe, status1);
	    B.Pipe.dequeue done_pipe;
	    B.Pipe.enqueue (pipe, status2);
	    B.Pipe.dequeue done_pipe;
	    B.Pipe.enqueue (pipe, status3);
	    B.Pipe.dequeue done_pipe
	 end

    fun data_fun (_, data) = 
         (Trace.trace_print (fn _ => "sly received " ^
			     Sly_Ip.Incoming.makestring data);
	  B.Test.test ("received data on server", fn _ => false))

    fun status_fun (status_pipe, done_pipe) (_, status) =
         (B.Test.test ("status",
		       fn _ => equal_status (status,
					     B.Pipe.dequeue status_pipe));
	  B.Pipe.enqueue (done_pipe, ()))

    fun handler _ =
         let val status_pipe = B.Pipe.new ()
             val done_pipe = B.Pipe.new ()
	 in {connection_handler = conn_fun (status_pipe, done_pipe),
	     data_handler = data_fun,
	     status_handler = status_fun (status_pipe, done_pipe)}
	 end
    fun null_handler _ =
	 {connection_handler = fn _ => (), data_handler = fn _ => (),
	  status_handler = fn _ => ()}

    fun session conn_synch (Sly_Ip.S {connect, listen, extension}) =
	 (Trace.trace_print (fn _ => "server waiting for client");
	  B.Pipe.dequeue conn_synch;
	  B.Scheduler.sleep 300;
	  Trace.trace_print (fn _ => "scheduler sleep done, starting send");
	  connect (Sly_Ip.Network_Address.Address snow, Sly_Ip.H handler);
	  Trace.trace_print (fn _ => "closing IP server session"))

    fun run conn_synch = Sly_Ip.session (sly_setup, session conn_synch)
   end

(*
	5.	client
*)

  structure Client_App =
   struct
    fun conn_fun (pipe, remote_packet_pipe)
                 (Snow_Ip.Icmp.C {send, abort, extension}) =
         let fun build_array size =
	          Word_Array.from8 (Word_Array.W8.U_Big.F.tabulate
				    (Word8.fromInt o Word.toInt, size))
	     fun out_data_fun size () = Snow_Ip.Outgoing.new (build_array size)
	     fun in_data_fun size () = Snow_Ip.Incoming.new (build_array size)
	     fun build_echo data_fun =
	          {id = Word16.fromInt 0x99,
		   sequence = Word16.fromInt 0x13,
		   data = data_fun ()}
	     val send1 = Snow_Ip.Icmp.Echo_Request
	                     (build_echo (out_data_fun 0w99))
	     val rcv1 = Snow_Ip.Icmp.Echo_Reply (build_echo (in_data_fun 0w99))
	     val send2 = Snow_Ip.Icmp.Timestamp_Request
	                     {id = Word16.fromInt 0x91,
			      sequence = Word16.fromInt 0x19}
	     val zero32 = Word32.fromInt 0
	     val rcv2 = Snow_Ip.Icmp.Timestamp_Reply
	                     {id = Word16.fromInt 0x91,
			      sequence = Word16.fromInt 0x19,
			      originate = zero32, receive = zero32,
			      transmit = zero32, returned = zero32}
	     val packet = B.Pipe.dequeue remote_packet_pipe
	     val send3 = Snow_Ip.Icmp.Source_Quench packet
	     val send4 = Snow_Ip.Icmp.Parameter_Problem (packet, 21)
	     val send5 = Snow_Ip.Icmp.Missing_Required_Option packet
	 in B.Pipe.enqueue (pipe, rcv1);
	    send send1;
	    B.Scheduler.sleep 1000;
	    B.Pipe.enqueue (pipe, rcv2);
	    send send2;
	    B.Scheduler.sleep 1000;
	    send send3;
	    B.Scheduler.sleep 100;
	    send send4;
	    B.Scheduler.sleep 100;
	    send send5
	 end

    fun equal_packet (packet1, packet2) =
         Word_Array.W8.U_Big.F.equal
             (Word_Array.to8
	      (Snow_Ip.Incoming.sub
	       (packet1, {start = 0w0,
			  length = Snow_Ip.Incoming.size packet1})),
	      Word_Array.to8
	      (Snow_Ip.Incoming.sub
	       (packet2, {start = 0w0,
			  length = Snow_Ip.Incoming.size packet2})))

    fun equal_icmp (Snow_Ip.Icmp.Echo_Reply {id = id1, sequence = seq1,
					     data = d1},
		    Snow_Ip.Icmp.Echo_Reply {id = id2, sequence = seq2,
					     data = d2}) =
         id1 = id2 andalso seq1 = seq2 andalso equal_packet (d1, d2)
      | equal_icmp (Snow_Ip.Icmp.Timestamp_Reply
		     {id = id1, sequence = seq1, originate = o1,
		      receive = r1, transmit = t1, returned = ret1},
		    Snow_Ip.Icmp.Timestamp_Reply
		     {id = id2, sequence = seq2, originate = o2,
		      receive = r2, transmit = t2, returned = ret2}) =
	 (Trace.local_print ("times are " ^
			     Int.toString (Word32.toInt (Word32.- (r1, o1))) ^
			     " to remote, " ^
			     Int.toString (Word32.toInt (Word32.- (t1, r1))) ^
			     " for remote processing, " ^
			     Int.toString (Word32.toInt (Word32.- (ret1, t1)))
			     ^ " to return");
	  id1 = id2 andalso seq1 = seq2 andalso
	  Word32.<= (o1, r1) andalso Word32.<= (r1, t1) andalso
	  Word32.<= (t1, ret1))
      | equal_icmp (a, b) =
	 (Trace.local_print ("got " ^ Snow_Ip.Icmp.Incoming.makestring a ^
			     ", expected " ^
			     Snow_Ip.Icmp.Incoming.makestring b);
	  false)
	  

    fun data_fun pipe (_, data) =
         (Trace.trace_print (fn _ => "snow received " ^
			     Snow_Ip.Icmp.Incoming.makestring data);
	  B.Test.test ("ICMP standard reply",
		       fn _ => equal_icmp (data, B.Pipe.dequeue pipe)))

    fun status_fun (_, status) =
         (Trace.local_print ("snow status " ^
			     Snow_Ip.Icmp.Status.makestring status);
	  B.Test.test ("ICMP status", fn _ => false))

    fun handler (packet_pipe) _ =
         let val expected_pipe = B.Pipe.new ()
	 in {connection_handler = conn_fun (expected_pipe, packet_pipe),
	     data_handler = data_fun expected_pipe,
	     status_handler = status_fun}
	 end

    fun session data_pipe (Snow_Ip.Icmp.S {connect, listen, extension}) =
         (connect (Test_Addresses.get_ip "sly",
		   Snow_Ip.Icmp.H (handler data_pipe));
	  Trace.trace_print (fn _ => "closing Icmp session"))

    (* the IP session only receives a data packet and sends it off
       on the data pipe, where it can be used by ICMP to send off
       some ICMP messages. *)
    fun ip_conn (done_pipe, close_session) _ =
         (B.Pipe.dequeue done_pipe;
	  B.Pipe.enqueue (close_session, ());
          Trace.trace_print (fn _ => "IP client connection handler returning"))
    fun ip_data (done_pipe, data_pipe) (_, data) =
         (Trace.trace_print (fn _ => "received IP client data");
	  B.Pipe.enqueue (data_pipe, data);
          B.Pipe.enqueue (done_pipe, ()))
    fun ip_status _ = ()
    fun ip_handler (data_pipe, close_session) _ =
         let val done_pipe = B.Pipe.new ()
	 in {connection_handler = ip_conn (done_pipe, close_session),
	     data_handler = ip_data (done_pipe, data_pipe),
	     status_handler = ip_status}
	 end
    fun ip_session conn_synch (Snow_Ip.S {connect, listen, extension}) =
	 let val close_session = B.Pipe.new ()
	     val data_pipe = B.Pipe.new ()
	     val pattern = Snow_Ip.Network_Pattern.Partial
	                     {proto = test_protocol}
	 in Trace.trace_print (fn _ => "client listening");
	    listen (pattern,
		    Snow_Ip.H (ip_handler (data_pipe, close_session)),
		    Snow_Ip.Count.Unlimited);
	    B.Pipe.enqueue (conn_synch, ());
	    Trace.trace_print (fn _ => "IP client listen started");
	    Snow_Ip.Icmp.session (snow_setup, session data_pipe);
	    Trace.trace_print (fn _ =>
			       "ICMP session done, waiting on close_session");
	    B.Pipe.dequeue close_session;
	    Trace.trace_print (fn _ => "closing IP client session")
	 end

    fun run (session_synch, conn_synch) =
         (Snow_Ip.session (snow_setup, ip_session conn_synch);
	  Trace.trace_print
	     (fn _ => "IP client session closed, signaling main thread");
	  B.Pipe.enqueue (session_synch, ()))
   end (* struct *)

(*
	6.	function run
*)

  fun do_run _ =
       let val conn_synch = B.Pipe.new ()
	   val session_synch = B.Pipe.new ()
	   fun timeout () =
	        (B.Scheduler.sleep 20000;
		 if B.Pipe.size session_synch < 0 then
		  (Trace.local_print "timeout";
		   B.Pipe.enqueue (session_synch, ()))
		 else ())
       in B.Scheduler.fork (fn _ =>
			    Client_App.run (session_synch, conn_synch));
	  B.Scheduler.fork timeout;
	  B.Scheduler.fork (fn _ => Server_App.run conn_synch);
	  B.Pipe.dequeue session_synch
       end

  fun run _ =
       (B.Test.tests ("ip-icmp", 5, do_run);
	B.Scheduler.sleep 10000)

  val _ = if ! B.Debug.do_tests then run () else ()

 end

(*
		7.	structure Test_Ip_Icmp
*)

structure Test_Ip_Icmp = Test_Ip_Icmp (structure B = Fox_Basis
				       val debug_level = NONE)





