(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpecho.tst: simple correctness tests (in loopback) for TCP.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_ECHO
	2.	functor Tcp_Echo
	3.	structure Tcp_Echo

---------------------------------------------------------------------
	iii.	RCS Log

$Log: tcpecho.tst,v $
Revision 1.32  1996/02/14  20:26:14  esb
changed FoxWord to Word.

Revision 1.31  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.30  1995/03/10  03:47:37  esb
adapted to new vendor.sig.

Revision 1.29  1995/02/13  23:28:49  esb
adapted for 1.07.

Revision 1.28  1995/02/09  19:52:28  esb
changed to compile under 1.07

Revision 1.27  1995/01/18  21:11:39  esb
added a scheduler reset so it will work multiple times.

Revision 1.26  1995/01/06  23:16:31  esb
replaced Tcp.finalize with Stack.finalize.

Revision 1.25  1995/01/06  17:02:08  esb
Adapted to new event.sig.

Revision 1.24  1994/10/27  20:25:15  cline
added SML/NJ 105 compatibility

Revision 1.23  1994/10/20  14:42:41  cline
partial support for SML/NJ 105b

Revision 1.22  1994/08/28  20:06:39  milnes
Added default gateway.

Revision 1.21  1994/08/24  22:11:30  esb
minor fix.

Revision 1.20  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.19  1994/07/07  02:26:20  esb
streamlined log_string so we no longer create and print a long string.

Revision 1.18  1994/07/01  02:32:00  danwang
Moved control structures into Fox_Basis.

Revision 1.17  1994/06/16  21:51:05  danwang
Updated to use functorized Fox_Basis.

Revision 1.16  1994/04/27  00:01:16  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.15  94/04/06  23:16:08  esb
adapted to new receive_packet interface.

Revision 1.14  94/03/07  16:55:23  esb
changed the sequence of print statements.

Revision 1.13  94/02/21  00:05:57  esb
minor changes to adapt to new TCP interfaces.

Revision 1.12  94/02/17  01:12:02  esb
printing of the log.

Revision 1.11  94/01/30  21:02:54  esb
now prints the real time taken to send and receive.

Revision 1.10  1994/01/28  01:18:33  esb
improved error reporting.

Revision 1.9  1994/01/19  21:34:26  esb
adapted to new interface.

Revision 1.8  1994/01/11  20:23:48  esb
changed active_open_timeout to user_timeout.

Revision 1.7  1994/01/09  03:28:03  esb
converted to using buildtcp.

Revision 1.6  1993/12/04  20:59:23  esb
can now specify an IP number instead of a host name.

Revision 1.5  1993/11/11  04:59:43  esb
added functor parameter active_open_timeout.

Revision 1.4  1993/11/04  15:10:08  esb
added ARP address resolution.

Revision 1.3  1993/10/25  19:34:33  cline
removed .U from Byte[421].U

Revision 1.2  1993/10/25  18:29:29  esb
renamed the test "TcpEcho", and adjusted functor parameters.

Revision 1.1  1993/10/22  02:49:04  esb
Initial revision

*)

(*
---------------------------------------------------------------------
	1.	signature TCP_ECHO
 *)

signature TCP_ECHO =
 sig
  val run: string * int -> unit
 end (* sig *)

(*
---------------------------------------------------------------------
	2.	functor Tcp_Echo
 *)

functor Tcp_Echo (structure B: FOX_BASIS
		  structure Stack: TCP_IP_ETH
                   sharing type Stack.Tcp.allocation = int
		       and type Stack.Tcp.lower_layer_address = Word32.word
		       and type Stack.Tcp.port = Word16.word
		       and type Stack.Tcp.incoming = Stack.Tcp.outgoing
			      = B.Dyn_Array.T
		  val debug_level: int ref option): TCP_ECHO =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpecho.tst")
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print
   
  val ip_over_eth = SW.n16 "0x800"
  val arp_over_eth = SW.n16 "0x806"
  val protocol_numbers = [ip_over_eth]
  val tcp_over_ip = SW.n8 "0x6"
  val tcp_echo_port = SW.n16 "7"
  val tcp_local_port = SW.n16 "2222"
  val initial_window = 1024 * 32
  val user_timeout = 120000 (* 2 minutes *)

  structure Watch = B.StopWatch

  structure Tcp = Stack.Tcp

  fun byte_at_pos n = Word8.intToWord (n mod 33 + 65)
  fun received_byte (packet, n) = B.Dyn_Array.sub1 (packet, n)
  fun fill_packet (n, _) = byte_at_pos n

  fun receive (count, queue, conn) = 
       let val received = ref 0
	   val bad_state = ref false
	   fun check_received (packet, pos, size) =
	        if pos >= size then true
		else if received_byte (packet, pos) =
		        byte_at_pos (! received + pos) then
		 check_received (packet, pos + 1, size)
		else
		 (local_print ("expecting byte " ^
			       FoxMakestring.word8 (byte_at_pos (! received +
								 pos)) ^
			       ", got byte " ^
			       FoxMakestring.word8 (received_byte (packet,
								   pos)) ^
			       " at position " ^
			       B.V.Integer.makestring (! received + pos) ^
			       ", test failed");
		  false)
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
		     bad_state := true);
		   Tcp.add_to_window (conn, size)
		end
       in receive_packet
       end

  fun resolve name =
       let val dns = Stack.Dns.new (Stack.Dns.default_authorities ())
       in case Stack.Dns.address_query (dns, name) of
	     (_, NONE) => NONE
	   | (_, SOME []) => NONE
	   | (_, SOME (address :: _)) => SOME address
       end

  fun test_run (name, count) () = 
       (B.Scheduler.reset ();
        Stack.initialize ();
        (let exception Unable_To_Resolve of string
	     val _ = debug_print (fn _ => "resolving name " ^ name)
	     val remote_ip = case ((resolve name) handle _ => NONE) of
	                        NONE =>
				 (local_print ("error, unable to resolve " ^
					       name);
				  raise Unable_To_Resolve name)
			      | SOME x => x
	     val _ = debug_print (fn _ => "resolved name " ^ name)
             val address = Tcp.Remote_Specified {peer = remote_ip,
						 remote_port = tcp_echo_port}
	     val q = B.Event_Queue.new ()
	     fun print_status s =
	          local_print ("got status " ^ Tcp.makestring_status s)
	     fun handler connection = (receive (count, q, connection),
				       print_status)
	     val connection = Tcp.connect (address, Tcp.Handler handler)
	     val _ = Tcp.start_logging connection
	     val (packet, send) = Tcp.allocate_send (connection, count)
	     val _ = B.Dyn_Array.app1 (packet, fill_packet)
	     fun timer () =
	          (B.Scheduler.sleep (3000 + count);
	           B.Event_Queue.signal {queue = q, match = fn _=> true,
					 value = ()};
	           ())
	     fun send_packet () =
	          (B.Scheduler.fork timer;
	           send ())
	     val stopwatch = Watch.stopwatch ()
         in local_print "starting test";
            Watch.start stopwatch;
            B.Event_Queue.wait {queue = q, event = (), while_waiting = send};
	    Watch.stop stopwatch;
	    let val time = Watch.time stopwatch
	        val real_time = Watch.Timing.real_time time
	        val time_string = Watch.Timing.Time.makestring real_time
	        fun adjust_times (_, []) = []
	          | adjust_times (NONE, (time, head) :: rest) =
	             (Tcp.Log.Time.Time {sec = 0, usec = 0}, head) ::
		     adjust_times (SOME time, rest)
	          | adjust_times (SOME start, (time, head) :: rest) =
	             (Tcp.Log.Time.- (time, start), head) ::
		     adjust_times (SOME start, rest)
	        val log = Tcp.get_log connection
	        fun log_string ([], _) = ()
	          | log_string (_, 0) = ()
	          | log_string (head :: rest, n) =
		     (local_print ("\t" ^ Tcp.Log.event_string head);
		      log_string (rest, n - 1))
	    in Tcp.close connection;
	       Stack.finalize ();
	       B.Test.test ("close", fn _ => true);
	       log_string (adjust_times (NONE, log), 2000);
	       local_print ("total time " ^ time_string ^ " seconds")
	    end
         end)
         handle _ =>
                 (local_print "exception in TCP-echo, finalizing stack";
                  Stack.finalize ();
	          ()))

  fun run (name, size) =
       let val test_name = "TcpEcho:" ^ name ^ "(" ^
	                   B.V.Integer.makestring size ^ ")"
       in B.Test.tests (test_name, 2, test_run (name, size))
       end

 end (* struct *)

(*
		3.	structure Tcp_Echo
*)

structure Tcp_Echo = Tcp_Echo (structure B = Fox_Basis
			       structure Stack = Tcp_Ip_Eth
			       val debug_level = NONE)



