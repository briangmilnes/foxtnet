(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	arp.tst: low-level test module for ARP


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Arp
	2.	function run_test
	3.	structure Test_Arp

	iii.	RCS Log

$Log: arp.tst,v $
Revision 1.38  1996/04/30  20:24:17  esb
adapted to new seq.sig which uses words.

Revision 1.37  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.36  1995/11/12  16:39:29  esb
adapted to new lower setup type (string), adapted to new Word_Array.

Revision 1.35  1995/10/04  21:29:27  esb
added xmeter_pathname to device.

Revision 1.34  1995/07/05  23:32:40  esb
adapted to new wordarray signature.

Revision 1.33  1995/06/29  18:20:42  esb
fixed to make more reliable by retransmitting until received.

Revision 1.32  1995/06/28  10:20:53  esb
fixed some synchronization bugs, now seems to work reliably.

Revision 1.31  1995/06/23  17:54:27  cline
updated to use Address_Specific

Revision 1.30  1995/06/20  16:57:52  esb
adapted to new protocol signature.

Revision 1.29  1995/03/12  17:54:12  esb
adapted to new trace.sig.

Revision 1.28  1995/03/10  03:50:14  esb
adapted to new vendor.sig.

Revision 1.27  1995/03/07  23:51:23  esb
updated tracing.

Revision 1.26  1995/02/09  19:50:29  esb
made work with sml/nj 1.07

Revision 1.25  1995/01/18  21:05:52  esb
adapted to new COROUTINE signature.

Revision 1.24  1995/01/06  17:04:11  esb
adapted to new Test_Addresses.

Revision 1.23  1994/12/01  18:45:34  esb
renamed parameters to Event_Queue.{signal,wait}.

Revision 1.22  1994/11/08  00:03:21  esb
decided the ethernet error message was unnecessary, and made it disappear.

Revision 1.21  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.20  1994/09/12  18:21:37  milnes
Fixed a bug, it's still a bit strange in that it is doing a round
robin arping that includes X arping X.

Revision 1.19  1994/08/24  22:21:46  esb
we now allow for incoming packets longer than what we sent.

Revision 1.18  1994/08/12  06:26:18  esb
minor changes.

Revision 1.17  1994/08/02  20:29:33  esb
adapted to new protocol signature.

Revision 1.16  1994/07/01  02:35:35  danwang
Moved control structures into Fox_Basis.

Revision 1.15  1994/06/16  16:46:40  danwang
Updated to use functorized Fox_Basis

Revision 1.14  1994/04/26  18:05:01  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.13  94/01/18  15:54:07  esb
restructured.

Revision 1.12  1994/01/17  18:13:14  esb
interface changes.

Revision 1.11  1993/12/17  04:07:05  esb
improved the tests and made them work more reliably.

Revision 1.10  1993/11/01  16:37:18  esb
got it to compile and run again.

Revision 1.9  1993/10/25  19:36:44  cline
removed .U from Byte[421].U

Revision 1.8  1993/10/14  18:25:34  milnes
Used implicit sequencing in let bodies.

Revision 1.7  1993/10/09  17:49:36  esb
consolidated the protocol state; we now use the new store interface.

Revision 1.6  1993/09/22  19:31:56  milnes
Removed "-----------"s.

Revision 1.5  1993/09/18  21:56:41  esb
minor changes.

Revision 1.4  1993/09/17  16:45:01  milnes
Changed default parameters.

Revision 1.3  1993/09/13  22:07:46  cline
deleted '#'s from RCS log

Revision 1.2  1993/09/02  19:40:56  esb
adapted to new PROTOCOL signature.

Revision 1.1  1993/08/24  21:20:29  esb
Initial revision

*)

(*
	1.	functor Test_Arp
*)

functor Test_Arp (structure B: FOX_BASIS
		  val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "arp.tst"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  val arp_eth_protocol = Word16.fromInt 0x555
  val quick_connect_protocol = Word16.fromInt 0x999
  val snow_connect_protocol = Word16.fromInt 0x998
  val sly_connect_protocol = Word16.fromInt 0x997
  val inf = "SE0"

  structure Sim = Build_Simulators (structure B = B
				    val xmeter_pathname = "/dev/null"
				    val debug_level = debug_level)

  structure Quick_Build = Build_Eth (structure Device = Sim.Quick
				     structure B = B
				     val debug_level = debug_level)
  
  structure Snow_Build = Build_Eth (structure Device = Sim.Snow
				    structure B = B
				    val debug_level = debug_level)
  
  structure Sly_Build = Build_Eth (structure Device = Sim.Sly
				   structure B = B
				   val debug_level = debug_level)
  
  structure Quick_Eth = Quick_Build.Eth

  structure Snow_Eth = Snow_Build.Eth

  structure Sly_Eth = Sly_Build.Eth

  structure Quick = Arp_Eth (structure Eth = Quick_Eth
			     val arp_protocol_number = arp_eth_protocol
			     structure B = B
			     val debug_level = debug_level)

  structure Snow = Arp_Eth (structure Eth = Snow_Eth
			    val arp_protocol_number = arp_eth_protocol
			    structure B = B
			    val debug_level = debug_level)

  structure Sly = Arp_Eth (structure Eth = Sly_Eth
			   val arp_protocol_number = arp_eth_protocol
			   structure B = B
			   val debug_level = debug_level)

  fun make_ip_array ip =
       Word_Array.from32 (Word_Array.W32.unalign
			  (Word_Array.W32.Big.F.create (ip, 0w1)))

  val quick_ip = make_ip_array (Test_Addresses.get_ip "quick")
  val snow_ip = make_ip_array (Test_Addresses.get_ip "snow")
  val sly_ip = make_ip_array (Test_Addresses.get_ip "sly")

  val retries = 5
  val timeout = if Trace.debug_on () then 1000 else 100
  fun power (base, 0) = 1
    | power (base, exponent) = base * power (base, exponent - 1)
  val time_waited = timeout * power (2, retries)

  fun make_array (size, start) =
       let fun generate count =
	        if count = size then NONE
		else
		 let val new_count = count + 1
		     val new_value = Word8.fromInt (count mod 256)
		 in SOME (new_value, new_count)
		 end
       in Word_Array.from8 (Word_Array.W8.U_Big.F.new generate 0)
       end

  val num_tests = 12

  fun complete_test (queue, tests) =
       (tests := ! tests + 1;
	debug_print (fn _ => "completed test " ^
		     Integer.toString (! tests));
	if ! tests = num_tests then
	 (B.Event_Queue.signal {queue = queue, match = fn _ => true,
				value = ()};
	  ())
	else ())

(*
	2.	function run_test

	This test causes the named simulator to arp all three of sly,
	snow and quick.  It is very different from the usual style
        of testing (e.g. segment.tst) in that it checks multiple
        things and keeps counts for a total of 12 tests.
*)

  fun run_test (queue, tests, name, protocol_number, self_address,
		connect, send_fun, quick_listen, quick_max,
		snow_listen, snow_max, sly_listen, sly_max,
		h_con, new, sub, size, address_specific) =
       (let fun pipe_timeout pipe () =
                 (B.Scheduler.sleep (time_waited * 5);
		  B.Pipe.enqueue (pipe, ()))
            fun conn_handler pipe conn =
	          (B.Scheduler.fork (pipe_timeout pipe);
		   B.Pipe.dequeue pipe)
	    val receive_count = B.Pipe.new ()
	    fun data_handler (requested, expected, sub, size,
			      pipe, done) (_, packet) =
	         if ! done then ()
		 else
		  let val packet_data = sub (packet, {start = 0w0,
						      length = size packet})
		      fun same () =
		           Word_Array.W8.U_Big.F.equal
			      (Word_Array.to8 packet_data,
			       Word_Array.to8 expected)
		  in done := true;
		     B.Pipe.enqueue (pipe, ());
		     B.Test.test (requested ^ " receiving from " ^ name, same);
		     complete_test (queue, tests);
		     B.Pipe.enqueue (receive_count, ())
		  end
            fun status_handler requested status =
	         (B.Test.test (name ^ " unexpected status from " ^ requested,
			       fn _ => false);
		  complete_test (queue, tests))
	    fun handler (name, expected, sub, size, done) key =
	         let val pipe = B.Pipe.new ()
		 in {connection_handler = conn_handler pipe,
		     data_handler = data_handler (name, expected, sub,
						  size, pipe, done),
		     status_handler = status_handler name}
		 end
	    val quick_pattern = Quick.Arp_Pattern.Specific
				{self = quick_ip, protocol = protocol_number}
	    val snow_pattern = Snow.Arp_Pattern.Specific
			       {self = snow_ip, protocol = protocol_number}
	    val sly_pattern = Sly.Arp_Pattern.Specific
			      {self = sly_ip, protocol = protocol_number}
	    val quick_data = make_array (50, 33)
	    val snow_data = make_array (55, 44)
	    val sly_data = make_array (999, 10)
	    val self_data = case name of
                               "quick" => quick_data
			     | "snow" => snow_data
			     | _ => sly_data
	    val quick_done = ref false
	    val snow_done = ref false
	    val sly_done = ref false
	    val (self_data, self_done) =
	          case name of
		     "quick" => (quick_data, quick_done)
		   | "snow" => (snow_data, snow_done)
		   | _ => (sly_data, sly_done)
	    val Quick.L {stop = stop_quick, ...} =
	          quick_listen (quick_pattern,
				Quick.H (handler ("quick", quick_data,
						  Quick.Incoming.sub,
						  Quick.Incoming.size,
						  quick_done)),
				quick_max)
	    val Snow.L {stop = stop_snow, ...} =
	          snow_listen (snow_pattern,
			       Snow.H (handler ("snow", snow_data,
						Snow.Incoming.sub,
						Snow.Incoming.size,
						snow_done)),
				snow_max)
	    val Sly.L {stop = stop_sly, ...} =
	          sly_listen (sly_pattern,
			      Sly.H (handler ("sly", sly_data,
					      Sly.Incoming.sub,
					      Sly.Incoming.size,
					      sly_done)),
				sly_max)
	    fun yield () =
	         B.Scheduler.suspend (fn s => B.Scheduler.resume (s, ()))
	   (* send until we are informed that the packet has been received. *)
	    fun send_conn_handler (packet, received) conn =
		 (send_fun conn (new packet);
		  yield ();
		  if ! received then ()
		  else send_conn_handler (packet, received) conn)
	    fun send_handler (packet, name, expected, sub, size,
			      received) key =
	         let val pipe = B.Pipe.new ()
	         in {connection_handler = send_conn_handler (packet, received),
		     data_handler = data_handler (name, expected, sub,
						  size, pipe, self_done),
		     status_handler = status_handler name}
		 end
	    val quick_address = address_specific
				{self = self_address, peer = quick_ip,
				 protocol = protocol_number}
	    val quick_handler = h_con (send_handler (quick_data, name,
						     self_data, sub, size,
						     quick_done))
	    val snow_address = address_specific
			       {self = self_address, peer = snow_ip,
				protocol = protocol_number}
	    val snow_handler = h_con (send_handler (snow_data, name,
						    self_data, sub, size,
						    snow_done))
	    val sly_address = address_specific
			      {self = self_address, peer = sly_ip,
			       protocol = protocol_number}
	    val sly_handler = h_con (send_handler (sly_data, name,
						   self_data, sub, size,
						   sly_done))
	in connect (quick_address, quick_handler);
	   connect (snow_address, snow_handler);
	   connect (sly_address, sly_handler);
	   B.Pipe.dequeue receive_count; (* wait to receive packets *)
	   B.Pipe.dequeue receive_count;
	   B.Pipe.dequeue receive_count;
           stop_quick ();
           stop_snow ();
           stop_sly ()
	end)
	 handle x =>
	         (local_print ("exception " ^ B.V.Control.exnName x ^
			       " raised in " ^ name ^ " test");
		  B.Test.test (name, fn _ => false))

  fun quick_test (queue, tests) = 
       (Quick.session
	(inf,
	 fn (Quick.S {connect = quick_c, listen = quick_l, ...}) =>
	 Snow.session
	 (inf,
	  fn (Snow.S {connect = snow_c, listen = snow_l, ...}) =>
	  Sly.session (inf,
		       fn (Sly.S {connect = sly_c, listen = sly_l, ...}) =>
		       run_test (queue, tests, "quick",
				 quick_connect_protocol, quick_ip, quick_c,
				 (fn (Quick.C {send, ...}) => send),
				 quick_l, Quick.Count.Maximum 1,
				 snow_l, Snow.Count.Maximum 1,
				 sly_l, Sly.Count.Maximum 1,
				 Quick.H, Quick.Incoming.new,
				 Quick.Incoming.sub,
				 Quick.Incoming.size,
				 Quick.Arp_Address.Specific))));
	B.Test.test ("quick", fn _ => true);
	complete_test (queue, tests))
	

  fun snow_test (queue, tests) = 
       (Quick.session
        (inf,
	 fn (Quick.S {connect = quick_c, listen = quick_l, ...}) =>
	 Snow.session
	 (inf,
	  fn (Snow.S {connect = snow_c, listen = snow_l, ...}) =>
	  Sly.session
	  (inf,
	   fn (Sly.S {connect = sly_c, listen = sly_l, ...}) =>
	   run_test (queue, tests, "snow",
		     snow_connect_protocol, snow_ip, snow_c,
		     (fn (Snow.C {send, ...}) => send),
		     quick_l, Quick.Count.Maximum 1,
		     snow_l, Snow.Count.Maximum 1,
		     sly_l, Sly.Count.Maximum 1,
		     Snow.H, Snow.Incoming.new,
		     Snow.Incoming.sub,
		     Snow.Incoming.size,
		     Snow.Arp_Address.Specific))));
	B.Test.test ("snow", fn _ => true);
	complete_test (queue, tests))

  fun sly_test (queue, tests) = 
       (Quick.session
	(inf,
	 fn (Quick.S {connect = quick_c, listen = quick_l, ...}) =>
	 Snow.session
	 (inf,
	  fn (Snow.S {connect = snow_c, listen = snow_l, ...}) =>
	  Sly.session
	  (inf,
	   fn (Sly.S {connect = sly_c, listen = sly_l, ...}) =>
	   run_test (queue, tests, "sly",
		     sly_connect_protocol, sly_ip, sly_c,
		     (fn (Sly.C {send, ...}) => send),
		     quick_l, Quick.Count.Maximum 1,
		     snow_l, Snow.Count.Maximum 1,
		     sly_l, Sly.Count.Maximum 1,
		     Sly.H, Sly.Incoming.new,
		     Sly.Incoming.sub,
		     Sly.Incoming.size,
		     Sly.Arp_Address.Specific))));
	B.Test.test ("sly", fn _ => true);
	complete_test (queue, tests))

  fun signal_timeout queue () =
       (B.Scheduler.sleep (time_waited * 8);
	case B.Event_Queue.signal {queue = queue, match = fn _ => true,
				   value = ()} of
	   NONE => ()
	 | _ => B.Test.test ("timeout", fn _ => false))

  fun run_tests () =
       let val queue = B.Event_Queue.new ()
	   val tests = ref 0
	   val constant_args = (queue, tests)
	   fun run_arp () =
	        (B.Scheduler.fork (fn _ => quick_test constant_args);
		 B.Scheduler.fork (fn _ => snow_test constant_args);
		 B.Scheduler.fork (fn _ => sly_test constant_args))
(*
	   fun run_arp () =
	        (quick_test constant_args;
	         snow_test constant_args;
		 sly_test constant_args)
*)
       in B.Scheduler.fork run_arp;
	  B.Event_Queue.wait {queue = queue, event = (),
			      while_waiting = signal_timeout queue};
	  B.Test.test ("final", fn _ => true)
       end

  fun run () = B.Test.tests ("Arp", num_tests + 1, run_tests)

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
	3.	structure Test_Arp
*)

structure Test_Arp = Test_Arp (structure B = Fox_Basis
			       val debug_level = NONE)

