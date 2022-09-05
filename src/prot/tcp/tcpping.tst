(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpping.tst: loopback ping-pong test for TCP.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Tcp_Ping_Pong
	2.	structure Test_Tcp

---------------------------------------------------------------------
	iii.	RCS Log

$Log: tcpping.tst,v $
Revision 1.18  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.17  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.16  1995/02/21  13:05:02  esb
upgraded for SML/NJ 1.07.

Revision 1.15  1995/01/18  21:11:39  esb
added a scheduler reset so it will work multiple times.

Revision 1.14  1995/01/06  17:02:33  esb
adapted to new Test_Addresses.

Revision 1.13  1994/12/05  22:03:05  esb
adapted to new event.sig.

Revision 1.12  1994/11/22  13:56:09  milnes
Updated to initialize its own addressing so that the tests would run.

Revision 1.11  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.10  1994/08/28  20:07:48  milnes
Added default_gateway.

Revision 1.9  1994/08/18  20:32:06  esb
adapted to new actions, support for status messages.

Revision 1.8  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.7  1994/07/01  02:32:24  danwang
Moved control structures into Fox_Basis.

Revision 1.6  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.5  1994/06/09  18:38:16  esb
added a delay to allow the passive open to be set up before the active open,
needed now that we abort unknown connections.

Revision 1.4  1994/04/26  20:12:33  esb
changed number of packets sent to avoid running out of memory.

Revision 1.3  94/04/26  17:59:29  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.2  94/03/25  18:42:15  esb
made the expiration time more reasonable for large packets.

Revision 1.1  94/03/25  16:27:40  esb
Initial revision


*)

(*
---------------------------------------------------------------------
	1.	functor Test_Tcp_Ping_Pong
 *)

functor Test_Tcp_Ping_Pong (structure B: FOX_BASIS
			    val packet_size: int
			    val packets_per_confirm: int
			    val total_confirms: int
			    val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpping.tst")
  val local_print = Trace.local_print
  val trace_constant_string = Trace.trace_constant_string
  val do_prints = Trace.debug_on ()

  val sleep_factor = if do_prints then 9600 else 1000
  val std_debug_level = NONE
  val tcp_debug_level = debug_level
  
  structure Sim = Build_Simulators (structure B = B
				    val debug_level = std_debug_level)

  val ip_over_eth = SW.n16 "0x800"
  val tcp_over_ip = SW.n8 "6"
  val initial_window = 4096
  val user_timeout = 600000 (* ten minutes *)

  val quick_ip = Test_Addresses.get_ip "quick"
  val snow_ip = Test_Addresses.get_ip "snow"

  structure Build_Quick = Build_Tcp (structure Device = Sim.Quick
				     structure B = B
				     val ip_over_eth = ip_over_eth
				     val tcp_over_ip = tcp_over_ip
				     val initial_window_size = initial_window
				     val user_timeout = user_timeout
				     val eth_debug_level = std_debug_level
				     val arp_debug_level = std_debug_level
				     val ip_debug_level = std_debug_level
				     val icmp_debug_level = std_debug_level
				     val tcp_debug_level = tcp_debug_level)

  structure Build_Snow = Build_Tcp (structure Device = Sim.Snow
				    structure B = B 
				    val ip_over_eth = ip_over_eth
				    val tcp_over_ip = tcp_over_ip
				    val initial_window_size = initial_window
				    val user_timeout = user_timeout
				    val eth_debug_level = std_debug_level
				    val arp_debug_level = std_debug_level
				    val ip_debug_level = std_debug_level
				    val icmp_debug_level = std_debug_level
				    val tcp_debug_level = tcp_debug_level)

  fun init_stacks () =
       (Build_Quick.Ip_Mux.add ("SE0", Test_Addresses.get_ip "quick");
	Build_Quick.Ip.initialize ();
	Build_Quick.Ip.set_interface_address ("SE0",
					      Test_Addresses.get_ip "quick");
	Build_Snow.Ip_Mux.add ("SE0", Test_Addresses.get_ip "snow");
	Build_Snow.Ip.initialize ();
	Build_Snow.Ip.set_interface_address ("SE0",
					     Test_Addresses.get_ip "snow"))

  fun fin_stacks () =
       (Build_Quick.Ip.finalize ();
        Build_Snow.Ip.finalize ();
	())

  structure Quick = Build_Quick.Tcp
  structure Snow = Build_Snow.Tcp

  val quick_ctrl_port = SW.n16 "333"
  val snow_ctrl_port = SW.n16 "444"

  val quick_data_port = SW.n16 "666"
  val snow_data_port = SW.n16 "667"

  fun print_log (_, []) = ()
    | print_log (n, head :: rest) =
       (local_print (B.V.Integer.makestring n ^ "\t" ^ head);
	if n < 300 then print_log (n + 1, rest) else local_print "...")

  val quick_ctrl_address =
       Quick.Address (Quick.Key {peer = snow_ip,
				 local_port = quick_ctrl_port,
				 remote_port = snow_ctrl_port})

  val quick_data_address =
       Quick.Address (Quick.Key {peer = snow_ip,
				 local_port = quick_data_port,
				 remote_port = snow_data_port})

  val snow_ctrl_address = Snow.Local_Specified {local_port = snow_ctrl_port}

  val snow_data_address = Snow.Local_Specified {local_port = snow_data_port}


  fun byte21 b = FoxWord8.intToWord (FoxWord16.wordToInt b)

  fun fill_packet n = FoxWord8.intToWord ((if n = 0 then 243 else n) mod 256)

  val client_data = B.Dyn_Array.init1 (packet_size, fill_packet)
  val server_size = 1
  val server_confirm = B.Dyn_Array.init1 (server_size, fill_packet)

  val bytes_per_confirm = packet_size * packets_per_confirm

  fun client_receive pipe connection packet =
       (trace_constant_string "c";
	B.Pipe.enqueue (pipe, ()))

  fun client_send (send, pipe, total_packets, sent) =
       if total_packets = 0 then ()
       else
	(send ();
	 if sent + packet_size >= bytes_per_confirm then
	  (trace_constant_string "s";
	   B.Pipe.dequeue pipe;
	   trace_constant_string "x")
	 else ();
	 client_send (send, pipe, total_packets - 1,
		      (sent + packet_size) mod bytes_per_confirm))

  fun snow_server () =
       let val ctrl_pipe = B.Pipe.new NONE
           val data_pipe = B.Pipe.new NONE
	   val bytes_received = ref 0
	   val packets_received = ref 0
	   fun loop_signal (pipe, expected_packets, send, size) =
	        (bytes_received := ! bytes_received + size;
		 if ! bytes_received >= bytes_per_confirm then
		  (bytes_received := ! bytes_received - bytes_per_confirm;
		   packets_received := ! packets_received +
		                       packets_per_confirm;
		   trace_constant_string "r";
		   send ();
		   trace_constant_string "t ";
		   if ! packets_received >= expected_packets then
		    (trace_constant_string "R\n";
		     packets_received := 0;
		     B.Pipe.enqueue (pipe, ()))
		   else ();
		   loop_signal (pipe, expected_packets, send, 0))
		 else ())
	   fun receive_signal (pipe, expected_packets, send) p =
	        loop_signal (pipe, expected_packets, send, B.Dyn_Array.size p)
	   fun receive_conn (pipe, expected_packets) conn =
	        let val (packet, send) = Snow.allocate_send (conn, server_size)
		in B.Dyn_Array.update (packet, 0,
				       B.Dyn_Array.read server_confirm);
		   receive_signal (pipe, expected_packets, send)
		end
	   val ctrl_rcv = receive_conn (ctrl_pipe, 1)
	   fun status Snow.Connection_Closing =
	        B.Test.test ("server connection closed status", fn _ => true)
	     | status s =
	        local_print ("received server status " ^
			     Snow.makestring_status s)
	   fun ctrl_handler connection = (ctrl_rcv connection, status)
	   val packets = (total_confirms * bytes_per_confirm) div packet_size
	   val data_rcv = receive_conn (data_pipe, packets)
	   fun data_handler connection = (data_rcv connection, status)
	   val (ctrl_stop, ctrl_conn) =
	         Snow.start_passive (snow_ctrl_address,
				     Snow.Handler ctrl_handler, SOME 1)
	   val _ = B.Pipe.dequeue ctrl_pipe;
	   val (data_stop, data_conn) =
	         Snow.start_passive (snow_data_address,
				     Snow.Handler data_handler, SOME 1)
       in map Snow.send_immediately (data_conn ());
	  B.Pipe.dequeue data_pipe;
	  map Snow.close (data_conn ());
	  data_stop ();
          B.Pipe.dequeue ctrl_pipe;
	  map Snow.close (ctrl_conn ());
	  ctrl_stop ()
       end

  fun quick_client completion_queue () =
       let val ctrl_queue = B.Pipe.new NONE
	   val data_queue = B.Pipe.new NONE
	   fun status Quick.Connection_Closing =
	        B.Test.test ("client connection closed status", fn _ => true)
	     | status s =
	        local_print ("received client status " ^
			     Quick.makestring_status s)
	   val ctrl_receive = client_receive ctrl_queue
	   val data_receive = client_receive data_queue
	   fun ctrl_handler connection = (ctrl_receive connection, status)
	   fun data_handler connection = (data_receive connection, status)
	   val control = Quick.connect (quick_ctrl_address,
					Quick.Handler ctrl_handler)
	   val _ = B.Test.test ("control connection established", fn _ => true)
	   val (ctrl_packet, ctrl_send) = Quick.allocate_send (control,
							       packet_size)
	   val _ = B.Dyn_Array.update (ctrl_packet, 0,
				       B.Dyn_Array.read client_data)
	   val _ = client_send (ctrl_send, ctrl_queue, packets_per_confirm, 0)
	   val _ = B.Scheduler.sleep 300  (* wait for peer to passive open. *)
	   val conn = Quick.connect (quick_data_address,
				     Quick.Handler data_handler)
	   val (packet, send) = Quick.allocate_send (conn, packet_size)
       in B.Dyn_Array.update (packet, 0, B.Dyn_Array.read client_data);
          Quick.start_logging conn;
          Quick.send_immediately conn;
	  B.Test.test ("data connection established", fn _ => true);
          client_send (send, data_queue,
		       total_confirms * packets_per_confirm, 0);
	  B.Test.test ("data sent", fn _ => true);
	  Quick.close conn;
	  B.Test.test ("data connection closed", fn _ => true);
	  client_send (ctrl_send, ctrl_queue, packets_per_confirm, 0);
	  Quick.close control;
	  B.Test.test ("control connection closed", fn _ => true);
	  B.Event_Queue.signal {queue = completion_queue, match = fn _ => true, value = ()};
	  ()
       end

  fun signal_timeout (queue, time) () =
       (B.Scheduler.sleep time;
	B.Event_Queue.signal {queue = queue, match = fn _ => true, value = ()};
	())

  fun test_run () =
       let val timeout_queue = B.Event_Queue.new ()
	   val timeout_factor = packets_per_confirm * total_confirms + 100
           val timeout = timeout_factor * sleep_factor
           val wait = signal_timeout (timeout_queue, timeout)
       in Quick.initialize ();
            Snow.initialize ();
          B.Scheduler.fork snow_server;
          B.Scheduler.fork (quick_client timeout_queue);
          B.Scheduler.fork wait;
          B.Event_Queue.wait {queue = timeout_queue, event = (),
			      while_waiting = fn _ => ()};
          Quick.finalize ();
          Snow.finalize ();
          ()
       end

  fun run () =
       (B.Scheduler.reset ();
	init_stacks ();
	B.Test.tests ("Tcp_Ping_Pong", 9, test_run);
	fin_stacks ())

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
		2.	structure Test_Tcp
*)

structure Test_Tcp_Ping_Pong =
             Test_Tcp_Ping_Pong (structure B = Fox_Basis
				 (* val packet_size = 1 *)
				 val packet_size = 1444
				 val packets_per_confirm = 1
				 val total_confirms = 150
				 val debug_level = NONE)



