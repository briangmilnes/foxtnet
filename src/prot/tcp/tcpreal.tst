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

	tcpclient.tst: client for on-line black-box testing for the
	TCP protocol.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_CLIENT_SERVER
	2.	functor Tcp_Real
	3.	internal structures
	4.	test functions
	5.	intern/extern and log utilities
	6.	client routines
	7.	server routines
	8.	structure Tcp_Real

---------------------------------------------------------------------
	iii.	RCS Log

$Log: tcpreal.tst,v $
Revision 1.21  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.20  1995/03/10  03:47:37  esb
adapted to new vendor.sig.

Revision 1.19  1995/02/21  13:05:02  esb
upgraded for SML/NJ 1.07.

Revision 1.18  1995/02/13  23:28:49  esb
adapted for 1.07.

Revision 1.17  1995/02/09  19:51:56  esb
changed to compile under 1.07

Revision 1.16  1995/01/18  21:11:39  esb
added a scheduler reset so it will work multiple times.

Revision 1.15  1995/01/06  23:18:34  esb
changed print to debug statements.

Revision 1.14  1995/01/06  17:16:22  esb
now using externally-defined Tcp_Ip_Eth.

Revision 1.13  1994/10/27  20:25:29  cline
added SML/NJ 105 compatibility

Revision 1.12  1994/10/20  14:37:36  cline
partial support for SML/NJ 105b

Revision 1.11  1994/08/24  22:18:19  esb
removed a print statement that slowed down execution a bit

Revision 1.10  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.9  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.8  1994/07/07  02:28:18  esb
streamlined log_string to no longer create and print a long string.

Revision 1.7  1994/07/01  02:32:32  danwang
Moved control structures into Fox_Basis.

Revision 1.6  1994/06/16  21:51:05  danwang
Updated to use functorized Fox_Basis.

Revision 1.5  1994/04/27  00:01:16  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.4  94/03/19  17:26:21  esb
added logging on the server side; made window size a functor parameter.

Revision 1.3  1994/03/03  00:33:24  esb
minor changes.

Revision 1.2  94/02/21  00:06:21  esb
minor changes to adapt to new TCP interfaces.

Revision 1.1  94/02/17  01:07:00  esb
Initial revision

*)

(*
---------------------------------------------------------------------
	1.	signature TCP_CLIENT_SERVER
*)

signature TCP_CLIENT_SERVER =
 sig
  val client: string * int (* size *) -> unit
  val server: unit -> unit
 end

(*
---------------------------------------------------------------------
	2.	functor Tcp_Real
*)

functor Tcp_Real (structure B: FOX_BASIS
		  structure Stack: TCP_IP_ETH
                   sharing type Stack.Tcp.allocation = int
		       and type Stack.Tcp.lower_layer_address = FoxWord32.word
		       and type Stack.Tcp.port = FoxWord16.word
		       and type Stack.Tcp.incoming = Stack.Tcp.outgoing
			      = B.Dyn_Array.T
		  val window: int
		  val log_size: int
		  val debug_level: int ref option): TCP_CLIENT_SERVER =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpreal.tst")
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  val server_port = FoxWord16.intToWord 0x222

(*
 ---------------------------------------------------------------------
	3.	internal structures
*)
  structure Watch = B.StopWatch

  structure Tcp = Stack.Tcp

(*
 ---------------------------------------------------------------------
	4.	test functions
*)

  fun byte_at_pos n = FoxWord8.intToWord (n mod 33 + 65)
  fun fill_packet (n, _) = byte_at_pos n

  fun status s =
       local_print ("received status " ^ Tcp.makestring_status s)

  fun handler (count, queue) conn = 
       let val received = ref 0
	   fun simple_receive_packet packet =
		let val size = B.Dyn_Array.size packet
		    val total = ! received + size
		in received := total;
		   if total = count then
		    (B.Test.test ("receive", fn _ => true);
		     B.Event_Queue.signal {queue = queue,
					   match = fn _ => true, value = ()};
		     ())
		   else ()
		end
	   val bad_state = ref false
	   fun check_received _ = true
	   fun receive_packet packet =
		let val size = B.Dyn_Array.size packet
		in if ! bad_state then ()
		   else if check_received (packet, 0, size) then
		    (received := ! received + size;
		     if ! received = count then
		      (B.Test.test ("receive", fn _ => true);
		       B.Event_Queue.signal {queue = queue,
					     match = fn _ => true, value = ()};
		       ())
		     else ())
		   else
		    (B.Test.test ("receive", fn _ => false);
		     B.Event_Queue.signal {queue = queue, match = fn _ => true,
					   value = ()};
		     bad_state := true)
		end
       in (simple_receive_packet, status)
       end

(*
 ---------------------------------------------------------------------
	5.	intern/extern and log utilities
*)

  fun encode_integer (allocate, conn, count) =
       let fun array_size n = if n >= 256 then 1 + array_size (n div 256)
			      else 1
	   val size = array_size count
	   val (array, send) = allocate (conn, size)
	   fun break_digits n =
	        if n >= 256 then
		 (FoxWord8.intToWord (n mod 256)) :: (break_digits (n div 256))
		else [FoxWord8.intToWord n]
	   fun write_array (_, []) = ()
	     | write_array (index, head :: rest) =
	        (B.Dyn_Array.update1 (array, index, head);
		 write_array (index + 1, rest))
       in write_array (0, rev (break_digits count));
	  send
       end

  fun decode_integer data =
       let fun to_list (array, n) =
	        if n >= B.Dyn_Array.size array then []
		else B.Dyn_Array.sub1 (array, n) :: (to_list (array, n + 1))
	   fun parse_num [] = 0
	     | parse_num (head :: rest) =
	        FoxWord8.wordToInt head + 256 * parse_num rest
       in parse_num (rev (to_list (data, 0)))
       end

  fun print_log (_, []) = ()
    | print_log (n, head :: rest) =
      if n >= log_size then local_print "..."
      else
       (local_print ("\t" ^ Tcp.Log.event_string head);
	print_log (n + 1, rest))

(*
 ---------------------------------------------------------------------
	6.	client routines
*)

  fun run_client (name, count) () =
       (B.Scheduler.reset ();
        Stack.initialize ();
        (let exception Name_Not_In_Test_Addresses
	     val remote_ip = case Test_Addresses.name_ip name of
	                        NONE =>
				 (local_print ("error, unable to resolve " ^
					       name);
				  raise Name_Not_In_Test_Addresses)
			      | SOME x => x
             val address = Tcp.Remote_Specified {peer = remote_ip,
						 remote_port = server_port}
	     val _ = debug_print (fn _ => "client connecting to " ^
				  Tcp.makestring_address address)
	     val q = B.Event_Queue.new ()
	     val conn = Tcp.connect (address, Tcp.Handler (handler (count, q)))
	     val _ = debug_print (fn _ => "client connected to" ^
				  Tcp.makestring_key (Tcp.connection_key conn))
	     val _ = Tcp.start_logging conn
	     val send = encode_integer (Tcp.allocate_send, conn, count)
	     fun timer () =
	          (B.Scheduler.sleep (6000 + count * 10);
	           local_print "Tcp-real timer expiration";
	           B.Event_Queue.signal {queue = q, match = fn _=> true,
					 value = ()};
	           ())
	     fun timer_send () =
	          (B.Scheduler.fork timer;
	           send ())
	     val stopwatch = Watch.stopwatch ()
	     val _ = Watch.start stopwatch
	     val _ = B.Event_Queue.wait {queue = q, event = (),
					 while_waiting = timer_send}
	     val time = Watch.time stopwatch
	     val real_time = Watch.Timing.real_time time
	     val time_string = Watch.Timing.Time.makestring real_time
	     val log = Tcp.get_log conn
         in Watch.stop stopwatch;
	    Tcp.close conn;
	    Stack.finalize ();
	    B.Test.test ("close", fn _ => true);
	    local_print ("total time " ^ time_string ^ " seconds");
	    print_log (0, Tcp.Log.relative_time log);
	    local_print ("total time " ^ time_string ^ " seconds")
         end)
         handle x =>
                 (local_print "exception in TCP-real, finalizing TCP";
                  Stack.finalize ();
	          raise x))

      fun client (name, size) =
           let val test_name = "TcpReal:" ^ name ^ "(" ^
	                       B.V.Integer.makestring size ^ ")"
           in B.Test.tests (test_name, 2, run_client (name, size))
           end

(*
 ---------------------------------------------------------------------
	7.	server routines

	To make the test well-behaved even with large amounts of data
	being sent, we allocate a relatively small buffer and send
        it over and over again.
*)

  fun run_server () =
       (B.Scheduler.reset ();
        Stack.initialize ();
	debug_print (fn _ => "stack initialized");
        (let val address = Tcp.Local_Specified {local_port = server_port}
	     val pipe = B.Pipe.new NONE
	     fun receive_size conn data =
	          B.Pipe.enqueue (pipe, (decode_integer data, conn))
	     fun handler conn =
		  (Tcp.start_logging conn;
		   (receive_size conn, status))
	     val single_packet_limit = 66000 (* data repeats every 33 bytes *)
	     val _ = Tcp.start_passive (address, Tcp.Handler handler, SOME 1)
	     val (count, conn) = B.Pipe.dequeue pipe
	     val packet_size = min (single_packet_limit, count)
	     val final_size = count mod single_packet_limit
	     val (data, send) = Tcp.allocate_send (conn, packet_size)
	     val _ = B.Dyn_Array.app1 (data, fill_packet)
	     val (final_data, final_send) =
	          Tcp.allocate_send (conn, final_size)
	     val _ = B.Dyn_Array.app1 (final_data, fill_packet)
	     fun do_send size =
	          if size > packet_size then
	           (send ();
		    do_send (size - packet_size))
	          else
                   (final_send ();
		    debug_print (fn _ => "final send completed"))
	     val _ = do_send count
	     val log = Tcp.get_log conn
         in debug_print (fn _ => "done sending, closing connection");
	    Tcp.close conn;
	    Stack.finalize ();
	    B.Test.test ("close", fn _ => true);
	    print_log (0, Tcp.Log.relative_time log)
         end)
         handle x =>
                 (local_print "exception in TCP-real server, finalizing TCP";
                  Stack.finalize ();
	          raise x))

  fun server () =
       let val test_name = "TcpReal:server"
       in B.Test.tests (test_name, 1, run_server)
       end

 end (* struct *)

(*
 ---------------------------------------------------------------------
	8.	structure Tcp_Real
*)

structure Tcp_Real = Tcp_Real (structure B = Fox_Basis
			       structure Stack = Tcp_Ip_Eth
			       val window = 1024 * 4
			       val log_size = 200
			       val debug_level = NONE)




