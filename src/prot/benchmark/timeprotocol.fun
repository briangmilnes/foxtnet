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

	This file contains a functor to time protocols.		



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Time_Protocol
	2.	printing functionality
	3.	other utilities
	4.	function run_client
	5.	function run_server

		iii.	RCS Log
	
$Log: timeprotocol.fun,v $
Revision 1.1  1995/06/20  17:27:21  esb
Initial revision

Revision 1.12  1995/03/10  03:49:26  esb
partial port to 1.07.

Revision 1.11  1994/07/07  16:33:28  esb
adapted to TIMING_TEST signature.

Revision 1.10  1994/06/16  21:52:46  danwang
Updated to use functorized Fox_Basis.

Revision 1.9  1994/05/04  01:39:41  esb
major debugging. Almost everything seems to run now.

Revision 1.8  94/04/26  17:56:11  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.7  94/04/20  19:45:32  milnes
Small change.

Revision 1.6  1994/04/06  23:07:47  esb
minor changes.

Revision 1.5  94/03/29  17:44:40  milnes
./prot/timeprotocol.fun
Changed for streaming tests.

Revision 1.4  1994/03/04  02:29:02  milnes
Added mtu, and many small changes.

Revision 1.2  1994/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file
move problems that confused rcs.

Revision 1.1  1994/02/08  19:10:23  milnes
Initial revision


		1.	functor Time_Protocol
*)

functor Time_Protocol (structure Scheduler: COROUTINE
		       structure P: PROTOCOL
		       structure Time_P: TIME_PROTOCOL
		       structure B: FOX_BASIS
		         sharing Time_P.P = P
		             and type Time_P.ip_number = FoxWord32.word
		             and type Time_P.byte = FoxWord8.word
		             and type P.allocation = int
		             and type P.outgoing = B.Dyn_Array.T
		       structure Event_Queue: EVENT_QUEUE
		       val wait_per_packet: int
		       val wait_per_confirm: int
		       val wait_per_data_byte: int
		       val streaming_protocol: bool
		       val debug_level: int ref option) =
 struct

(*
		2.	printing functionality
*)

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "timeprotocol.fun")
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print 

  val n4u0 = SW.n32 "0"
  val n4u1 = SW.n32 "1"
  fun realmax (a: real , b: real) = if a > b then a else b

  fun exponentialp s = exists (fn #"E" => true | _ => false) s;

  fun chop_off_decimal ([], n, digits_after_decimal) = []
    | chop_off_decimal (d :: s, n, digits_after_decimal) =
       if n = digits_after_decimal then []
       else d :: (chop_off_decimal (s, n + 1, digits_after_decimal))

  fun chop_real_string_at (#"." :: s, digits_after_decimal) =
       #"." :: (chop_off_decimal (s, 0, digits_after_decimal))
   | chop_real_string_at ([], digits_after_decimal) = []
   | chop_real_string_at (d :: s, digits_after_decimal) =
       d :: (chop_real_string_at (s, digits_after_decimal))

  fun real_to_string (digits_after_decimal, r) =
       let val s = B.V.Real.makestring r
           val e = explode s
       in if exponentialp e then s
	  else
	   B.V.String.implode (chop_real_string_at (e, digits_after_decimal))
       end

  fun total_time (total_time, number_of_actions, action_string) =
       B.V.String.concat
               (["Total Time\n"] @
		(B.StopWatch.Timing.makestrings total_time) @
		(B.StopWatch.Timing.makestrings_per
		 (total_time, FoxWord32.wordToInt number_of_actions,
		  "messages " ^ action_string)))

  fun bBpers (total_bytes, total_time) =
       let val epsilon = 0.000001
           val bytes_per_second =
	         if total_time > epsilon then
		  real (FoxWord32.wordToInt total_bytes) / total_time
		 else 0.0
	   val bits_per_second = bytes_per_second * 8.0
	   val bits = if bits_per_second < epsilon then "zero"
		      else real_to_string (2, bits_per_second)
	   val bytes = if bytes_per_second < epsilon then "zero"
		       else real_to_string (2, bytes_per_second)
       in (bits, bytes)
       end

  fun output_rate (size, sent, total_bytes, repetitions, total_time) =
       let val total_time = (B.Time.time_to_real
			           (B.Timing.real_time total_time))
           val (bits, bytes) = bBpers (total_bytes, total_time)
       in B.V.String.concat
	          [(Time_P.header ^ "Sent "^ FoxMakestring.word32 sent ^
		    " messages of size " ^ FoxMakestring.word32 size ^
		    " out of " ^ FoxMakestring.word32 repetitions ^
		    " requested.\n"),
		   (Time_P.header ^ "Sent " ^
		    FoxMakestring.word32 (FoxWord32.* (sent, size)) ^
		    " total bytes.\n"),
		   (Time_P.header ^ "Output rate  " ^ bytes ^
		    " bytes per second.\n"),
		   (Time_P.header ^ "            "  ^ bits  ^
		    " bits per second.\n")]
       end

  fun received_messages (received, repetitions, received_bytes, total_bytes) =
       if streaming_protocol then
	Time_P.header ^ "Received " ^ FoxMakestring.word32 received_bytes ^
	" bytes of " ^ FoxMakestring.word32 total_bytes ^ " in " ^
	FoxMakestring.word32 received ^ " messages.\n"
       else
	Time_P.header ^ "Received " ^ FoxMakestring.word32 received ^
	" messages of " ^ FoxMakestring.word32 repetitions ^ ".\n"

  fun received_confirms (received, messages_per_confirm) =
       (Time_P.header ^ "Confirming every " ^
	FoxMakestring.word32 messages_per_confirm ^ " messages.\n";
	Time_P.header ^ "Received " ^ FoxMakestring.word32 received ^
	" confirming messages.\n")

  fun sent_confirms (sent, messages_per_confirm) =
       (Time_P.header ^ "Confirming every " ^
	FoxMakestring.word32 messages_per_confirm ^ " messages.\n";
	Time_P.header ^ "Sent " ^ FoxMakestring.word32 sent ^
	" confirming messages.\n")

  fun summary (who, size, received_or_sent, repetitions, data_sent,
	       confirm, total_time, trips, received_bytes, total_bytes ) =
       let val total_time = (B.Time.time_to_real
			           (B.Timing.real_time total_time))
	   val epsilon = 0.000001
	   val latency = if trips = n4u0 then 0.0
			 else total_time / (real (FoxWord32.wordToInt trips))
	   val total_received = if streaming_protocol then received_bytes
				else FoxWord32.* (received_or_sent, size)
	   val (bitspers, _) = bBpers (total_received, total_time)
       in Time_P.header ^ who ^ " summary " ^
	  FoxMakestring.word32 size  ^ " " ^
	  (if streaming_protocol then
	    FoxMakestring.word32 received_bytes  ^ " / " ^
	    FoxMakestring.word32 total_bytes ^ " "
	   else
	    FoxMakestring.word32 received_or_sent  ^ " / " ^
	    FoxMakestring.word32 repetitions ^ " ") ^
	   FoxMakestring.word32 data_sent ^ " " ^
	   FoxMakestring.word32 confirm    ^  " " ^
	   B.V.Real.makestring total_time  ^  " " ^
	   bitspers ^  " " ^
	   B.V.Real.makestring latency ^ "\n"
       end

  fun history history_list =
       B.V.String.concat [Time_P.header, "History ",
			  B.V.String.concat (rev history_list), "\n"]

(*
		3.	other utilities
*)

  fun wait_time (repetitions, size, confirm) =
       (30000 +
	floor ((1000000.0 +
		realmax (real wait_per_packet *
			 real (FoxWord32.wordToInt repetitions),
			 real wait_per_data_byte *
			 real (FoxWord32.wordToInt size) *
			 real (FoxWord32.wordToInt repetitions)) +
		real wait_per_confirm *
		real (FoxWord32.wordToInt (FoxWord32.div (repetitions, confirm)))) /
	1000.0 + 0.5))

  fun start_timer (queue, time) _ =
       (Scheduler.sleep time;
	Event_Queue.signal {queue = queue, match = fn _ => true, value = ()};
	())

(*
		4.	function run_client
*)

  fun run_client ({repetitions, size, confirm, check_data,
		   print_packets, print_history}, server) =
       ((Time_P.start_test ();
	 debug_print "starting test.";
	 let val outputs = ref []
	     fun output s = outputs := s :: (! outputs)
	     val history_list = ref []: (string list ref)
	     fun do_record s = history_list := s :: (! history_list)
	     val record  = if print_history then do_record else fn _ => ()
	     val done = Event_Queue.new (): (unit, unit) Event_Queue.T
	     val watch = B.StopWatch.stopwatch ()
	     fun start () =
	          B.StopWatch.start watch;
	     fun stop  () =
	          (if B.StopWatch.running watch then
		    B.StopWatch.stop watch
		   else ();
		   Event_Queue.signal {queue = done, match = fn _ => true,
				       value = ()};
		   ())
	     val events = Event_Queue.new (): (unit, unit) Event_Queue.T
	     val confirms_received = ref n4u0
	     val n1uxff = SW.n8 "0xff"
	     fun receive_confirmation packet =
	          ((if Time_P.byte (packet, 0) = n1uxff then
		      (record "c";
		       debug_print "+";
		       confirms_received := FoxWord32.+ (! confirms_received,
							 n4u1);
		       Event_Queue.signal {queue = events,
				        match = fn () => true, value = ()};
		       ())
		     else
		      (print ("client: received byte " ^
			      FoxMakestring.word8 (Time_P.byte (packet, 0)) ^
			      "\n");
		       record "b")))
		   handle x =>
		           local_print ("exception " ^
					System.exn_name x ^
					" in client receive_confirmation\n")
	     fun receive connection = (receive_confirmation, fn _ => ())
	     val _ = debug_print "opening connection."
	     val connection = P.connect (server, P.Handler receive)
	     val _ = debug_print "finished open."
	     val data = Time_P.make_data (FoxWord32.wordToInt size)
	     val (outgoing_packet, send_fun) =
	            P.allocate_send (connection, FoxWord32.wordToInt size)
	     val _ = B.Dyn_Array.update (outgoing_packet, 0, data)
	     val sent = ref n4u0
	     fun send_packet () =
	          (debug_print ".\n";
		   send_fun ();
		   sent := FoxWord32.+ (! sent, n4u1))
	     fun send_loop n =
	          if (n = repetitions) then
		   (record "s";
		    Event_Queue.wait {queue = events, event = (),
				      while_waiting = send_packet})
		  else
		   (if FoxWord32.mod (n, confirm) = n4u0 then
		     (record "s";
		      Event_Queue.wait {queue = events, event = (),
					while_waiting = send_packet})
		    else
		     send_packet ();
		    send_loop (FoxWord32.+ (n, n4u1)))
	     fun send_thread () =
	          (start ();
		   send_loop n4u1;
		   stop ())
	     val wait = wait_time (repetitions, size, confirm)
	     fun output_statistics () =
	      (output (total_time ((B.StopWatch.time watch), repetitions,
				   "sent"));
	       output (output_rate (size, ! sent, FoxWord32.* (size, ! sent),
				    repetitions, B.StopWatch.time watch));
	       output (received_confirms (! confirms_received, confirm));
	       if print_history then output (history (! history_list)) else ();
	       output (summary ("client", size, ! sent, repetitions,
				FoxWord32.* (repetitions, size), confirm,
                                B.StopWatch.time watch, ! confirms_received,
				FoxWord32.* (! sent, size),
				FoxWord32.* (repetitions, size)));
	       rev (! outputs))

	 in output "Starting timing test as Sender.\n";
	    Time_P.on_connection connection;
   (* The wait in the send_loop may hang if any confirm packet has been
      dropped. So, fork the send loop off in a separate thread and when
      the main thread gets the control again, clear the event queue to
      release the thread.
    *)
	    Scheduler.fork send_thread;
	    Event_Queue.wait {queue = done, event = (),
			      while_waiting = start_timer (done, wait)};
	    if B.StopWatch.running watch then
	     output ("Timed out, wait = " ^ B.V.Integer.makestring wait ^
		     " milliseconds.\n")
	    else ();
	    stop ();
	    P.close connection;
	    Time_P.stop_test ();
	    debug_print "finished test.";
	    B.V.String.concat (output_statistics () @
			       ["Finished timing test as Sender.\n"])
	 end)
        handle x =>
	        (local_print ("Exception " ^
			      System.exn_name x ^ " in run_client.\n");
		 raise x))

(*
		5.	function run_server
*)

  fun run_server ({repetitions, size, confirm, check_data,
		   print_packets, print_history}, client) =
       ((Time_P.start_test ();
	 debug_print "starting test.";
	 let val outputs = ref []
	     fun output s = (outputs := s :: (! outputs))
	     val history_list = ref []: (string list ref)
	     fun do_record s = history_list := s :: (! history_list)
	     val record  = if print_history then do_record else fn _ => ()
	     val done = Event_Queue.new (): (unit, unit) Event_Queue.T
	     val watch = B.StopWatch.stopwatch ()
	     fun start () =
                  B.StopWatch.start watch;
	     fun stop () =
	          (if B.StopWatch.running watch then
		    B.StopWatch.stop watch
		   else ();
		   Event_Queue.signal {queue = done, match = fn _ => true,
				       value = ()};
		   ())
	     val received = ref n4u0
	     val number_of_confirms_sent = ref n4u0
	     val data = Time_P.make_data (FoxWord32.wordToInt size)
	     fun confirm_message send_fun =
	          (record "c";
		   debug_print ".\n";
		   send_fun ();
		   number_of_confirms_sent :=
		      FoxWord32.+ (! number_of_confirms_sent, n4u1))
	     val total_bytes = FoxWord32.* (repetitions, size)
	     val received_bytes = ref n4u0
	     val confirm_byte_interval = FoxWord32.* (size, confirm)
	     fun top_level_receive receive_fun connection =
	          let val (confirm_packet, send_fun) =
		            P.allocate_send (connection, Time_P.min_packet)
		  in B.Dyn_Array.update1 (confirm_packet, 0, SW.n8 "255");
		     (receive_fun send_fun, fn _ => ())
		  end
	     fun streaming_receive send_fun packet =
	          (if print_packets then
		    output (Time_P.makestring_packet packet)
		   else ();
		   (case Time_P.check_data (check_data, data, packet) of
		       Time_P.Valid =>
			(case Time_P.check_other (data, packet) of
			    Time_P.Valid =>
			     (record "r";
			      debug_print "+";
			      received := FoxWord32.+ (! received, n4u1);
			      received_bytes := FoxWord32.+
			                          (! received_bytes,
						   Time_P.size packet);
			      if ! received = n4u1 then
			       (* Got a packet so start timing now. *)
			       start ()
			      else ();
			      if FoxWord32.>= (! received_bytes,
					       total_bytes) then
			       (confirm_message send_fun;
				stop ())
			      else
			       if FoxWord32.mod
				   (! received_bytes,
				    confirm_byte_interval) = n4u0 then
				confirm_message send_fun
			       else ())
			  | Time_P.Ignore => record "."
			  | Time_P.Invalid => record "#")
		     | Time_P.Ignore => record "."
		     | Time_P.Invalid => (* The message has invalid data. *)
			record "b"))
		    handle x =>
		            local_print ("exception " ^
					 System.exn_name x ^
					 " in server streaming_receive\n")
	     fun block_receive send_fun packet =
	          (if print_packets then
		    output (Time_P.makestring_packet packet)
		   else ();
		   (case Time_P.check_data (check_data, data, packet) of
		       Time_P.Valid =>
			(case Time_P.check_other (data, packet) of
			    Time_P.Valid =>
			     (record "r";
			      debug_print "+";
			      received := FoxWord32.+ (! received, n4u1);
			      if ! received = n4u1 then
			       (* Got a packet so start timing now. *)
			       start ()
			      else ();
			      if ! received = repetitions then
			       (confirm_message send_fun;
				stop ())
			      else
			       if FoxWord32.mod (! received,
						 confirm) = n4u0 then
				confirm_message send_fun
			       else ())
			  | Time_P.Ignore =>  record "."
			  | Time_P.Invalid => record "#")
		     | Time_P.Ignore => record "."
		     | Time_P.Invalid => (* The message has invalid data. *)
			record "b"))
		    handle x =>
		            local_print ("exception " ^
					 System.exn_name x ^
					 " in server block_receive\n")
	     val receive = if streaming_protocol then
	                    top_level_receive streaming_receive
			   else top_level_receive block_receive
	     val _ = debug_print "opening connection."
	     val connection = P.connect (client, P.Handler receive)
	     val _ = debug_print "finished open."
	     val wait = wait_time (repetitions, size, confirm)
	     fun output_statistics () =
	          (output (total_time ((B.StopWatch.time watch), repetitions,
				       "sent"));
		   output (received_messages (! received, repetitions,
					      ! received_bytes, total_bytes));
		   output (sent_confirms (! number_of_confirms_sent, confirm));
		   if print_history then output (history (! history_list))
		   else ();
		   output (summary ("server", size, ! received, repetitions,
				    FoxWord32.* (repetitions, size), confirm,
				    B.StopWatch.time watch,
				    ! number_of_confirms_sent,
				    ! received_bytes, total_bytes));
		   rev (! outputs))
	 in output "Starting timing test as Receiver.\n";
	    Time_P.on_connection connection;
	    Event_Queue.wait {queue = done, event = (),
			      while_waiting = start_timer (done, wait)};
	    if B.StopWatch.running watch then
	     (output ("Timed out, wait = " ^
		      B.V.Integer.makestring wait ^ " miliseconds.\n"))
	    else ();
	    stop ();
	    P.close connection;
	    Time_P.stop_test ();
	    debug_print "finished test.";
	    B.V.String.concat (output_statistics () @
			       ["Finished timing test as Receiver.\n"])
	 end)
        handle x =>
	        (local_print ("Exception " ^ System.exn_name x ^
			      " in run_server.\n");
		 raise x))

 end (* struct *)








