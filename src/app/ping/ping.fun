(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A ping operation, similar to typical unix pings.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Ping

		iii.	RCS Log
	
$Log: ping.fun,v $
Revision 1.19  1997/06/04  12:06:59  esb
added serve_forever, removed UDP parameter to functor.

Revision 1.18  97/02/13  01:20:53  esb
adapted to 109.25 by adding "SMLofNJ" at one strategic location.
Can you find it?

Revision 1.17  1997/02/13  00:59:26  esb
removed compiler warnings about non-generalizable type variables.

Revision 1.16  1996/09/17  15:52:02  cline
trap interrupt (^C) and raise the right exception for traceroute

Revision 1.15  1996/05/08  02:04:02  esb
added traceroute, other fixes.

Revision 1.14  1996/03/05  21:12:24  esb
added inverse name lookup for printing routes.

Revision 1.13  1996/03/04  21:34:24  esb
many fixes.

Revision 1.12  1996/03/01  19:37:09  esb
new implementation, for Foxnet V2

Revision 1.11  1995/02/04  20:39:04  robby
updated to 107

Revision 1.10  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.9  1994/11/10  16:15:23  milnes
Updated to the new signatures.

Revision 1.8  1994/09/12  18:30:49  milnes
Added a default gateway.

Revision 1.7  1994/07/13  16:54:40  milnes
Updated to allow ping to use dns.

Revision 1.6  1994/07/01  02:21:38  danwang
Moved control structures into Fox_Basis.

Revision 1.5  1994/06/20  19:59:06  danwang
Updated to use new Fox_Basis.

Revision 1.4  1994/06/05  18:46:49  milnes
Made ping work over icmp.

Revision 1.3  1994/05/23  18:08:47  milnes
Updated to use icmp.

Revision 1.2  1994/05/23  14:05:29  milnes
Changed the ping statistics printing.

Revision 1.1  1994/05/04  19:14:18  milnes
Initial revision

		1.	functor Ping

*)

functor Ping (structure Ip: IP_PROTOCOL
(*
	      structure Udp: UDP_PROTOCOL (* for DNS, traceroute *)
	        sharing type Udp.Transport_Setup.T = Ip.Icmp.Setup.T
		    and type Ip.Icmp.Address.T = Udp.Host_Id.T = Word32.word
		    and type Ip.Icmp.timestamp = Word32.word
		    and type Ip.Icmp.id = Ip.Icmp.seq = Word16.word
		    and type Ip.Icmp.data_out = Ip.Network_Outgoing.T
*)
	      val setup: Ip.Icmp.Setup.T
	      val lookup: string -> Ip.Icmp.Address.T
	      val inverse_lookup: Ip.Icmp.Address.T -> string option
              structure B: FOX_BASIS
	      val debug_level: int ref option): PING =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val makestring = fn _ => NONE
			   val module_name = "ping.fun")

  val zero16 = Word16.fromInt 0
  val one16 = Word16.fromInt 1

  val zero32 = Word32.fromInt 0
  val one32 = Word32.fromInt 1

  structure Icmp = Ip.Icmp
  structure Option = Ip.Option
  structure In = Ip.Network_Incoming
  structure Out = Ip.Network_Outgoing

  local				(* stuff local to ping and complete_ping *)
   fun connect_fun (count, id, size, pipe, interval, record_route)
	           (conn as (Icmp.C {send, ...})) =
	let val bytes = Word_Array.from8
	                 (Word_Array.W8.U_Big.F.create (Word8.fromInt 77,
							size))
	    val option = [Option.Record_Route
			  (Option.UA {previous = [],
				      available = [zero32, zero32, zero32,
						   zero32, zero32, zero32,
						   zero32, zero32, zero32]})]
	    val data = if record_route then Out.new_options (bytes, option)
	               else Out.new bytes
	    fun connect_loop 0 =
		 B.Scheduler.sleep 3000 (* wait for stragglers *)
	      | connect_loop count_down = 
		 let val seq = Word16.fromInt (count - count_down)
		 in B.Pipe.enqueue (pipe, (id, seq, B.V.Time.now ()));
		    send (Icmp.Echo_Request {id = id, sequence = seq,
					     data = data});
		    B.Scheduler.sleep interval;
		    connect_loop (count_down - 1)
		 end
	in connect_loop count
	end

  fun print_ip name =
       Option.makestring_ip_number name ^
       (case inverse_lookup name of
	   NONE => ""
	 | SOME string => "\t" ^ string)

  fun print_route_loop [] = ""
    | print_route_loop [first] = print_ip first
    | print_route_loop (first :: rest) =
       print_ip first ^ "\n\t" ^ print_route_loop rest

   fun print_route (data, last_route) =
	 case In.options data of
	    [] => ""
	  | [Option.Record_Route (Option.UA {previous, available})] =>
	     if previous = (! last_route) then "\t(same route)"
	     else
	      (last_route := previous;
	       "\nRR:\t" ^ print_route_loop previous)
	  | options => "\noptions: " ^ Option.makestrings options

   fun print_received (data, sequence, host, delta_ms, last_route) =
        Trace.local_print (Integer.toString (Word.toInt (In.size data)) ^
			   " bytes from " ^ host ^
			   ": icmp-seq = " ^
			   Integer.toString (Word16.toInt sequence) ^
			   ", time = " ^ Integer.toString delta_ms ^ " ms" ^
			   print_route (data, last_route))

   fun data_fun (pipe, print, host, times, last_route)
                (conn as (Icmp.C {send, ...}),
		 packet as (Icmp.Echo_Reply {id, sequence, data})) =
        let val rcv_time = B.V.Time.now ()
	    val (send_id, send_seq, send_time) = B.Pipe.dequeue pipe
	    val delta_ms = B.V.Time.deltams (rcv_time, send_time)
	in if id = send_id andalso sequence = send_seq then
	    (times := delta_ms :: (! times);
	     print (data, sequence, host, delta_ms, last_route))
	   else if id = send_id then
	    if Word16.> (sequence, send_seq) then
	     (Trace.trace_print (fn _ => "skipping sequence number " ^
				 Word16.toString send_seq);
	      data_fun (pipe, print, host, times, last_route) (conn, packet))
	    else
	     (Trace.local_print ("duplicate or out-of-order sequence number " ^
				 Word16.toString sequence ^
				 ", expecting " ^
				 Word16.toString send_seq);
	      B.Pipe.requeue (pipe, (send_id, send_seq, send_time)))
	   else
	    Trace.local_print ("received reply with ID " ^
			       Word16.toString id ^ ", expected " ^
			       Word16.toString send_id)
	end
     | data_fun (pipe, print, host, times, route) (Icmp.C {send, ...}, data) =
        Trace.local_print ("received data " ^
			   Icmp.Incoming.makestring data)

   fun status_fun (Icmp.C {send, ...}, status) =
        Trace.local_print ("received status " ^
			   Icmp.Status.makestring status)

   fun handler (count, id, size, print, interval, record, host, times) key =
        let type pipe = (Word16.word * Word16.word * B.V.Time.time) B.Pipe.T
	    val pipe = (B.Pipe.new ()): pipe
	    val last_route = ref ([]: Word32.word list)
	in {connection_handler = connect_fun (count, id, size, pipe,
					      interval, record),
	    data_handler = data_fun (pipe, print, host, times, last_route),
	    status_handler = status_fun}
	end

   fun session_fun (remote_ip, count, size, print, interval, record,
		    host, times) (Icmp.S {connect, listen, ...}) =
        let val id = B.V.Time.toMilliseconds (B.V.Time.now ()) mod 256
	in (* give IP time to locate a gateway, if necessary *)
           Trace.trace_constant_string "sleeping in session_fun";
	   B.Scheduler.sleep 200;
           Trace.trace_constant_string "done sleeping in session_fun";
	   connect (remote_ip,
		    Icmp.H (handler (count, Word16.fromInt id, size,
				     print, interval, record, host, times)))
	end

   fun print_summary (times, count) =
        let val received_count = length times
	    val percent_loss = ((count - received_count) * 100) div count
	    val max_time = foldr Int.max 0 times
	    val min_time = foldr Int.min max_time times
	    val total_time = foldr (op+ ) 0 times
	    val line1 = Integer.toString count ^ " packets transmitted, " ^
	                Integer.toString received_count ^
			" packets received, " ^
	                Integer.toString percent_loss ^
			"% packet loss"
	    val line2 = if received_count > 0 then
	                 "round-trip (ms)  min/avg/max = " ^
			 Integer.toString min_time ^ "/" ^
			 Integer.toString (total_time div received_count) ^
			 "/" ^
			 Integer.toString max_time ^ "ms"
			else ""
	in Trace.local_print line1;
	   if line2 <> "" then Trace.local_print line2 else ()
	end

(*
   fun trace_conn (ttl, max_ttl, probles, verbose, wait, pipe)
                  (Udp.C {send, abort, extension}) =
        let val valid = ref true
	    fun timeout () =
	         (B.Scheduler.sleep 1000;
		  if ! valid then B.Scheduler.enqueue (pipe, ()) else ())
	    val 
	in send data
	end

   fun trace_data (Udp.C {send, abort, extension}, data) =
        Trace.local_print ("traceroute received UDP data "  ^
			   Udp.Incoming.makestring data)

   fun trace_status pipe (Udp.C {send, abort, extension}, status) =

   (* eventually "received" should be a pipe of IP numbers * remaining ttls.
      For now we simply pipe unit values. *)
   fun trace_handler (max_ttl, probes, verbose, wait) key =
        let val received = B.Pipe.new ()
	    val conn =  trace_conn (0, max_ttl, probles, verbose,
				    wait, received)
	    val data = trace_data
	    val status = trace_status received
	in Udp.H {conn_handler = conn, data_handler = data,
		  status_handler = status}
	end

   fun traceroute_fun (address, max_ttl, probes, no_lookup, wait)
                      (Udp.S {connect, listen, ...}) =
	(* give IP time to locate a gateway, if necessary *)
        (Trace.trace_constant_string "sleeping in session_fun";
	 B.Scheduler.sleep 200;
	 Trace.trace_constant_string "done sleeping in session_fun";
	 connect (address,
		  Udp.H (trace_handler (max_ttl, probes, verbose, wait))))
*)

  in
   fun complete_ping {host, count, size, quiet, interval, record_route} =
        ((let val _ = Trace.trace_constant_string "converting name to IP"
	      val remote_ip = lookup host
	      val _ = Trace.trace_constant_string "name converted to IP"
	      val dsize = case size of NONE => 0w64 | SOME i => Word.fromInt i
	      val print = if quiet then fn _ => () else print_received
	      val pause = case interval of NONE => 1000 | SOME i => i
	      val remote = Icmp.Address.makestring remote_ip
	      val times = ref ([]: int list)
	  in Icmp.session (setup,
			   session_fun (remote_ip, count, dsize, print,
				        pause, record_route, remote, times));
	     print_summary (! times, count)
	  end)
	   handle x => Trace.print_handled (x, SOME "complete_ping"))

   fun ping (host, count) =
        complete_ping {host = host, count = count, size = NONE,
		       quiet = false, interval = NONE, record_route = false}
   val ping = Keyboard_Interrupts.protect ping

   fun collect running () =
        (B.Scheduler.sleep 100000;
	 if ! running then
	  (Trace.local_print "collecting...";
	   SMLofNJ.Internals.GC.doGC 0;
	   Trace.local_print "collection complete.";
	   collect running ())
	 else ())

   fun serve {seconds} =
        let val running = ref true
	in Trace.local_print "serving ICMP";
	   B.Scheduler.fork (collect running);
	   Icmp.session (setup, fn _ => B.Scheduler.sleep (seconds * 1000));
	   running := false;
	   Trace.local_print "done serving ICMP"
	end

   fun serve_forever () =
	(Trace.local_print "serving ICMP";
	 B.Scheduler.fork (collect (ref true));
	 Icmp.session (setup, fn _ => B.Scheduler.suspend (fn _ => ()));
	 Trace.local_print "error, done serving ICMP")

(*
   fun complete_traceroute {host, max_ttl, probes, verbose, port, wait} =
        let val remote_ip = lookup host
	    val address_data = {peer = remote_ip, remote_port = port}
	    val address = Udp.Transport_Address.Remote_Specified address_data
	    val session_fun = traceroute_fun (address, max_ttl, probes,
					      verbose, udp_port, wait)
        in Icmp.session (setup, session_fun)
        end
*)
  end

  exception Traceroute_Is_Not_Implemented
  exception Make_Exec_Is_Not_Implemented

  fun make_ping_executable exec_name =
       let fun usage () = Trace.local_print "usage: ping hostname [count]"
	   fun main (compiler, arguments) =
	        (case arguments of
		    [] => usage ()
		  | [host] => ping (host, 10)
		  | [host, count] =>
		     (case Integer.fromString count of
		         NONE => usage ()
		       | SOME c => ping (host, c))
		  | _ => usage ();
		 0)			(* exit value *)
       in SMLofNJ.exportFn (exec_name, main)
       end

  fun traceroute host = raise Traceroute_Is_Not_Implemented

  local
   val Ip.Network_Setup.Setup ip_setup_list = setup

   fun connect_fun (count, id, size, pipe, interval)
	           (conn as (Icmp.C {send, extension, ...})) =
	let val bytes = Word_Array.W8.U_Big.F.create (Word8.fromInt 77, size)
	    val ext1: Icmp.connection_extension = extension
	    val ext2: Ip.Icmp.connection_extension = ext1
	    val ext3: Ip.network_connection_extension = ext2
	    val Ip.Connection_Extension {local_address, ...} = ext3
	    val option = [Option.Traceroute {id = id, ip = local_address,
					     outbound = zero16,
					     return = zero16}]
	    val data = Out.new_options (Word_Array.from8 bytes, option)
	    fun connect_loop 0 =
		 B.Scheduler.sleep 3000 (* wait for stragglers *)
	      | connect_loop count_down = 
		 let val seq = Word16.fromInt (count - count_down)
		 in B.Pipe.enqueue (pipe, (id, seq, B.V.Time.now ()));
		    send (Icmp.Echo_Request {id = id, sequence = seq,
					     data = data});
		    B.Scheduler.sleep interval;
		    connect_loop (count_down - 1)
		 end
	in connect_loop count
	end

  fun print_ip name =
       Option.makestring_ip_number name ^
       (case inverse_lookup name of
	   NONE => ""
	 | SOME string => "\t" ^ string)

  fun print_route_loop [] = ""
    | print_route_loop [first] = print_ip first
    | print_route_loop (first :: rest) =
       print_ip first ^ "\n\t" ^ print_route_loop rest

   fun print_route (data, last_route) =
	 case In.options data of
	    [] => ""
	  | [Option.Record_Route (Option.UA {previous, available})] =>
	     if previous = (! last_route) then "\t(same route)"
	     else
	      (last_route := previous;
	       "\nRR:\t" ^ print_route_loop previous)
	  | options => "\noptions: " ^ Option.makestrings options

   fun print_received (data, sequence, host, delta_ms, last_route) =
        Trace.local_print (Integer.toString (Word.toInt (In.size data)) ^
			   " bytes from " ^ host ^
			   ": icmp-seq = " ^
			   Integer.toString (Word16.toInt sequence) ^
			   ", time = " ^ Integer.toString delta_ms ^ " ms" ^
			   print_route (data, last_route))

   fun data_fun (pipe, invert, host, times)
                (conn as (Icmp.C {send, ...}),
		 packet as (Icmp.Echo_Reply {id, sequence, data})) =
        let val rcv_time = B.V.Time.now ()
	    val (send_id, send_seq, send_time) = B.Pipe.dequeue pipe
	    val delta_ms = B.V.Time.deltams (rcv_time, send_time)
	in if id = send_id andalso sequence = send_seq then
	    (times := delta_ms :: (! times);
	     Trace.local_print ("received " ^
				Icmp.Incoming.makestring packet)
	     (* print (data, sequence, host, delta_ms, last_route) *) )
	   else if id = send_id then
	    if Word16.> (sequence, send_seq) then
	     (Trace.trace_print (fn _ => "skipping sequence number " ^
				 Word16.toString send_seq);
	      data_fun (pipe, invert, host, times) (conn, packet))
	    else
	     (Trace.local_print ("duplicate or out-of-order sequence number " ^
				 Word16.toString sequence ^
				 ", expecting " ^
				 Word16.toString send_seq);
	      B.Pipe.requeue (pipe, (send_id, send_seq, send_time)))
	   else
	    Trace.local_print ("received reply with ID " ^
			       Word16.toString id ^ ", expected " ^
			       Word16.toString send_id)
	end
     | data_fun (pipe, invert, host, times) (Icmp.C {send, ...}, data) =
        Trace.local_print ("received data " ^
			   Icmp.Incoming.makestring data)

   fun status_fun times (Icmp.C {send, ...}, status) =
        Trace.local_print ("received status " ^
			   Icmp.Status.makestring status)

   fun handler (count, id, size, invert, interval, host, times) key =
        let type pipe = (Word16.word * Word16.word * B.V.Time.time) B.Pipe.T
            val pipe = (B.Pipe.new ()): pipe
	in {connection_handler = connect_fun (count, id, size, pipe, interval),
	    data_handler = data_fun (pipe, invert, host, times),
	    status_handler = status_fun times}
	end

   fun session_fun (remote_ip, count, size, invert, interval,
		    host, times) (Icmp.S {connect, listen, ...}) =
        let val id = B.V.Time.toMilliseconds (B.V.Time.now ()) mod 256
	in (* give IP time to locate a gateway, if necessary *)
           Trace.trace_constant_string "sleeping in session_fun";
	   B.Scheduler.sleep 200;
           Trace.trace_constant_string "done sleeping in session_fun";
	   connect (remote_ip,
		    Icmp.H (handler (count, Word16.fromInt id, size, invert,
				     interval, host, times)))
	end

   fun complete_traceroute_icmp {host, count, size, verbose, timeout} =
        ((let val _ = Trace.trace_constant_string "converting name to IP"
	      val remote_ip = lookup host
	      val _ = Trace.trace_constant_string "name converted to IP"
	      val dsize = case size of NONE => 0w64 | SOME i => Word.fromInt i
	      val invert = if verbose then inverse_lookup
			   else fn _ => NONE
	      val sleep = case timeout of NONE => 10000 | SOME i => i
	      val remote = Icmp.Address.makestring remote_ip
	      val times = ref ([]: int list)
	  in Icmp.session (setup,
			   session_fun (remote_ip, count, dsize, invert,
					sleep, remote, times)) (* ;
	     print_summary (! times, count) *)
	  end)
	   handle x => Trace.print_handled (x, SOME "complete_ping"))

  in
   fun traceroute_icmp host = (* raise Make_Exec_Is_Not_Implemented *)
        complete_traceroute_icmp {host = host, count = 1, size = NONE,
				  verbose = true, timeout = SOME 10000}
  end


 end
