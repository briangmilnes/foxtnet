(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpserve.tst: server for TCP well-known ports.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Tcp_Serve
	2.	structure Tcp_Serve

---------------------------------------------------------------------
	iii.	RCS Log

$Log: tcpserve.tst,v $
Revision 1.5  1995/03/24  01:46:54  esb
changed the user interface.

Revision 1.4  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.3  1995/03/10  03:47:37  esb
adapted to new vendor.sig.

Revision 1.2  1995/02/21  13:05:02  esb
upgraded for SML/NJ 1.07.

Revision 1.1  1995/01/22  15:47:25  esb
Initial revision


*)

(*
---------------------------------------------------------------------
	1.	functor Tcp_Serve
 *)

functor Tcp_Serve (structure B: FOX_BASIS
		   structure Stack: TCP_IP_ETH
		   val debug_level: int ref option
                    sharing type Stack.Tcp.allocation = int
		        and type Stack.Tcp.lower_layer_address = FoxWord32.word
		        and type Stack.Tcp.port = FoxWord16.word
		        and type Stack.Tcp.incoming = Stack.Tcp.outgoing
			       = B.Dyn_Array.T) =
 struct
  local
   structure Trace = Trace (structure V = B.V
			    val debug_level = debug_level
			    val module_name = "tcpserve.tst")
   val local_print = Trace.local_print

   val echo_port = FoxWord16.intToWord 7
   val discard_port = FoxWord16.intToWord 9
   val daytime_port = FoxWord16.intToWord 13
   val time_port = FoxWord16.intToWord 37 (* RFC 868 *)

   fun echo connection packet =
        let val size = B.Dyn_Array.size packet
	    val (return, send) = Stack.Tcp.allocate_send (connection, size)
        in B.Dyn_Array.update (return, 0, B.Dyn_Array.read packet);
	   send ();
	   ()
        end

   fun discard connection _ = ()

   fun date connection =
        let val value = B.Time.make_date (B.Time.time_of_day ()) ^ "\n"
	    val size = B.V.String.length value
	    val (return, send) = Stack.Tcp.allocate_send (connection, size)
        in B.Dyn_Array.updatestring (return, 0, value);
	   send ();
	   ()
        end

(* The following value is taken from RFC 868 *)
   val epoch_start_ut = FoxWord32.+ (FoxWord32.* (FoxWord32.intToWord 22089,
						  FoxWord32.intToWord 100000),
				     FoxWord32.intToWord 88800)

   fun time connection =
        let val B.Time.Time {sec, usec} = B.Time.time_of_day ()
	    val sec_ut = FoxWord32.+ (FoxWord32.intToWord sec, epoch_start_ut)
	    val size = 4
	    val (return, send) = Stack.Tcp.allocate_send (connection, size)
        in B.Dyn_Array.update4 (return, 0, B.Order.B4.to_big sec_ut);
	   send ();
	   ()
        end

   fun status (pipe, connection, _) Stack.Tcp.Connection_Closing =
	(Stack.Tcp.close connection;
	 B.Pipe.enqueue (pipe, ()))
     | status (pipe, connection, _) Stack.Tcp.Connection_Reset =
	(Stack.Tcp.close connection;
	 B.Pipe.enqueue (pipe, ()))
     | status (_, _, receiver) (Stack.Tcp.Urgent_Data data) =
        receiver data

   fun dequeue_many (pipe, 0) = ()
     | dequeue_many (pipe, n) =
        (B.Pipe.dequeue pipe;
	 dequeue_many (pipe, n - 1))

   fun suspend_forever () =
        B.Scheduler.suspend (fn (x: unit B.Scheduler.suspension) => ())

   fun run_specified specifier =
        (Stack.initialize ();
	 let val pipe = B.Pipe.new NONE
	     fun echo_fun connection =
	          let val receiver = echo connection
		  in (receiver, status (pipe, connection, receiver))
		  end
	     val echo_handler = Stack.Tcp.Handler echo_fun
	     val echo_addr = Stack.Tcp.Local_Specified {local_port = echo_port}
	     val (stop_echo, _) = Stack.Tcp.start_passive (echo_addr,
							   echo_handler,
							   specifier)
	     fun disc_fun connection =
	          let val receiver = discard connection
		  in (receiver, status (pipe, connection, receiver))
		  end
	     val disc_handler = Stack.Tcp.Handler disc_fun
	     val disc_addr = Stack.Tcp.Local_Specified {local_port =
						        discard_port}
	     val (stop_disc, _) = Stack.Tcp.start_passive (disc_addr,
							   disc_handler,
							   specifier)
	     fun date_fun connection =
	          (date connection;
		   Stack.Tcp.close connection;
		   B.Pipe.enqueue (pipe, ());		   
		   (fn _ => (), fn _ => ()))
	     val date_handler = Stack.Tcp.Handler date_fun
	     val date_addr = Stack.Tcp.Local_Specified {local_port =
						        daytime_port}
	     val (stop_date, _) = Stack.Tcp.start_passive (date_addr,
							   date_handler,
							   specifier)
	     fun time_fun connection =
	          (time connection;
		   Stack.Tcp.close connection;
		   B.Pipe.enqueue (pipe, ());		   
		   (fn _ => (), fn _ => ()))
	     val time_handler = Stack.Tcp.Handler time_fun
	     val time_addr = Stack.Tcp.Local_Specified {local_port =
						        time_port}
	     val (stop_time, _) = Stack.Tcp.start_passive (time_addr,
							   time_handler,
							   specifier)
	 in local_print "server ready";
	    case specifier of
	       NONE => suspend_forever ()
	     | SOME n => dequeue_many (pipe, n);
	    stop_echo ();
	    stop_disc ();
	    stop_date ();
	    stop_time ();
	    Stack.finalize ();
	    ()
	 end)
        handle x =>
	        (Stack.finalize ();
	         ())

  in (* local *)

   fun run () = run_specified NONE

   fun count n = run_specified (SOME n)

   fun inetd (_, _) = run_specified NONE

  end (* local *)
 end (* struct *)

(*
		2.	structure Tcp_Serve
*)

structure Tcp_Serve = Tcp_Serve (structure B = Fox_Basis
				 structure Stack = Tcp_Ip_Eth
				 val debug_level = NONE)



