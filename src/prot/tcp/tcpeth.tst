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

	tcpeth.tst: testing TCP over ethernet.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCPETH_CLIENT_SERVER
	2.	functor Tcp_Eth
	3.	internal structures
	4.	test functions
	5.	intern/extern and log utilities
	6.	client routines
	7.	server routines
	8.	structure Tcp_Eth

---------------------------------------------------------------------
	iii.	RCS Log

$Log: tcpeth.tst,v $
Revision 1.13  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.12  1995/03/10  03:47:37  esb
adapted to new vendor.sig.

Revision 1.11  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.10  1995/02/13  23:28:49  esb
adapted for 1.07.

Revision 1.9  1995/01/18  21:15:09  esb
got it to work.

Revision 1.8  1995/01/14  02:28:30  esb
added tcp window-scale option.

Revision 1.7  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.6  1994/07/01  02:32:07  danwang
Moved control structures into Fox_Basis.

Revision 1.5  1994/06/16  21:51:05  danwang
Updated to use functorized Fox_Basis.

Revision 1.4  1994/05/10  08:10:41  esb
adapted to store.sig.

Revision 1.3  94/04/27  00:01:16  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.2  94/04/06  23:16:29  esb
adapted to new receive_packet interface, added minor changes.

Revision 1.1  94/03/03  00:31:51  esb
Initial revision

*)

(*
---------------------------------------------------------------------
	1.	signature TCPETH_CLIENT_SERVER
*)

signature TCPETH_CLIENT_SERVER =
 sig
  val client: string * int (* size *) -> unit
  val server: unit -> unit
 end

(*
---------------------------------------------------------------------
	2.	functor Tcp_Eth
*)

functor Tcp_Eth (structure B: FOX_BASIS
		 val log_size: int
		 val debug_level: int ref option): TCPETH_CLIENT_SERVER =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpeth.tst")
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  val tcp_over_eth = FoxWord8.intToWord 66

  val client_port = FoxWord16.intToWord 111
  val server_port = FoxWord16.intToWord 222

(*
 ---------------------------------------------------------------------
	3.	internal structures
*)


  structure Watch = B.StopWatch

  structure Base = Build_Eth_Dev (structure B = B
				  val high_priority_filter = true
				  val debug_level = NONE)

  structure Eth = Ethernet (structure Dev = Base.Eth_Dev
			    structure B = B
			    val debug_level = NONE)

  structure Arp = Arp_Eth (structure Eth = Eth
			   val arp_protocol_number = FoxWord16.intToWord 0x806
			   structure B = B
			   val debug_level = NONE)

  structure Length =
   struct
    structure Length =
        Min_Length (structure Lower = Arp
		    val lower_min_size = Arp.minimum_packet_size
		    structure B = B)
    open Arp
    fun maximum_packet_size () =
         Arp.maximum_packet_size () - Length.length_size
    fun minimum_packet_size () = 0
    open Length
   end

  structure Pseudo_Ip = Pseudo_Ip (structure Lower = Length
				   structure B = B)

  structure Tcp = Tcp (structure Lower = Pseudo_Ip
		       structure B = B
		       val ip_equal =
			    (op= : FoxWord32.word * FoxWord32.word -> bool)
		       val ip_checksum = fn _ => FoxWord16.intToWord 0
		       val tcp_protocol = tcp_over_eth
		       val protocol_checksum = FoxWord16.intToWord 0
		       val initial_window = 1024 * 4
		       val compute_checksums = false
		       val abort_unknown_connections = false
		       val user_timeout = 12000 (* twelve seconds *)
		       val debug_level = debug_level)

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
        Tcp.initialize ();
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
	    Tcp.finalize ();
	    B.Test.test ("close", fn _ => true);
	    local_print ("total time " ^ time_string ^ " seconds");
	    print_log (0, Tcp.Log.relative_time log);
	    local_print ("total time " ^ time_string ^ " seconds")
         end)
         handle x =>
                 (local_print "exception in TCP-real, finalizing TCP";
                  Tcp.finalize ();
	          raise x))

      fun client (name, size) =
           let val test_name = "TcpEth:" ^ name ^ "(" ^
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
        Tcp.initialize ();
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
	    Tcp.finalize ();
	    B.Test.test ("close", fn _ => true);
	    print_log (0, Tcp.Log.relative_time log)
         end)
         handle x =>
                 (local_print "exception in TCP-real server, finalizing TCP";
                  Tcp.finalize ();
	          raise x))

      fun server () =
           let val test_name = "TcpEth:server"
           in B.Test.tests (test_name, 1, run_server)
           end

 end (* struct *)

(*
 ---------------------------------------------------------------------
	8.	structure Tcp_Eth
*)

structure Tcp_Eth = Tcp_Eth (structure B = Fox_Basis
			     val log_size = 200
			     val debug_level = NONE)
