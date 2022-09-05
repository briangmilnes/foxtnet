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

	ipclient.tst: client for on-line black-box testing for the IP protocol.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature IP_CLIENT_SERVER
	2.	functor Ip_Real
	3.	internal structures
	4.	test functions
	5.	client routines
	6.	server routines
	7.	structure Ip_Real

---------------------------------------------------------------------
	iii.	RCS Log

$Log: ipreal.tst,v $
Revision 1.17  1996/01/19  23:02:29  esb
adapted to the new wordarray signature.

Revision 1.16  1995/03/12  17:50:04  esb
adapted to new trace.sig.

Revision 1.15  1995/03/07  20:35:57  esb
updated tracing.

Revision 1.14  1995/02/09  19:53:21  esb
added reporting for the number of incoming and outgoing packets.

Revision 1.13  1995/01/18  21:02:13  esb
adapted to new coro.sig.

Revision 1.12  1995/01/06  16:55:19  esb
minor changes.

Revision 1.11  1994/11/22  13:56:09  milnes
Updated to initialize its own addressing so that the tests would run.

Revision 1.10  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.9  1994/07/01  02:28:10  danwang
Moved control structures into Fox_Basis.

Revision 1.8  1994/06/16  21:50:38  danwang
Updated to use functorized Fox_Basis.

Revision 1.7  1994/06/13  21:11:29  esb
adapted to the new Ip.incoming_message.

Revision 1.6  1994/04/27  00:00:49  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.5  94/03/03  21:38:02  esb
cleaned up.

Revision 1.4  94/02/21  00:01:10  esb
minor change.

Revision 1.3  94/01/29  04:49:58  esb
minor improvements.

Revision 1.2  1993/12/17  02:33:53  esb
integrated buildip.fun.

Revision 1.1  1993/11/11  05:18:35  esb
Initial revision

*)

(*
---------------------------------------------------------------------
	1.	signature IP_CLIENT_SERVER
*)

signature IP_CLIENT_SERVER =
 sig
  val client: {ip: FoxWord32.word, size: int, count: int} -> unit
  val server: {size: int, count: int} -> unit
 end

(*
---------------------------------------------------------------------
	2.	functor Ip_Real
*)

functor Ip_Real (structure B: FOX_BASIS
                 structure Tcp_Ip_Eth: TCP_IP_ETH
                 sharing type Tcp_Ip_Eth.Ip_No_Icmp.ip_protocol = FoxWord8.word
                     and type Tcp_Ip_Eth.Ip_No_Icmp.ip_number = FoxWord32.word
                     and type Tcp_Ip_Eth.Ip_No_Icmp.allocation = int
		 val debug_level: int ref option): IP_CLIENT_SERVER =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ipreal.tst")
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print
  val debug_constant_string = Trace.debug_constant_string

(*
	3.	test functions
*)

  fun fill_message (p, s) = p

(*

	5.	client routines
*)

  structure Ip = Tcp_Ip_Eth.Ip_No_Icmp

  val test_protocol = SW.n8 "99"

  fun run_client (remote_ip, size, count) () =
       let val address = Ip.Address {ip = remote_ip, proto = test_protocol}
	   fun receive c =
	        let fun receive_data data =
		         local_print "error, received data"
		    fun receive_status status =
		         local_print "error, received status"
		in (receive_data, receive_status)
		end
	   val _ = debug_constant_string "trying to connect"
	   val conn = Ip.connect (address,Ip.Handler receive)
	   val _ = debug_constant_string "connected"
	   val (packet,send) = Ip.allocate_send (conn, size)
	   val new_message = fill_message (packet, size)
	   fun send_loop 0 = ()
	     | send_loop n =
	       (send ();
		 Trace.do_if_debug (fn () => print ".");
		 send_loop (n - 1))
       in send_loop count;
	  B.Scheduler.sleep 500;
	  Ip.close conn;
	  local_print ("IP sent " ^
		       B.V.Integer.makestring (Ip.packets_sent ()) ^
		       " packets, received " ^
		       B.V.Integer.makestring (Ip.packets_received ()) ^
		       " packets");
	  true
       end

  fun fork_client (name, size, count) () =
       (Tcp_Ip_Eth.initialize ();
        B.Test.test ("send some packets ", run_client (name, size, count));
        Tcp_Ip_Eth.finalize ();
        ())
       handle x => (local_print ("exception " ^ System.exn_name x ^
				 " in client");
		    Tcp_Ip_Eth.finalize ();
		    ())

  fun client {ip, size, count} =
       B.Test.tests ("IpClient", 1, fork_client (ip, size, count));

(*

	6.	server routines
*)

  val timeout = 20000 (* 20 seconds *)

  fun run_server (size, count) () =
       (Tcp_Ip_Eth.initialize ();
	let val address = Ip.Partial {proto = test_protocol}
	    val receive_count = ref 0
	    val queue = B.Pipe.new NONE
	    fun receive_fun s =
	         (debug_constant_string "received data";
		  receive_count := ! receive_count + 1;
		  if ! receive_count = count then
		   B.Pipe.enqueue (queue, ())
		  else ())
	    fun status_fun s = print "error, received status"
	    fun receive c =
	         (debug_constant_string "instantiating handlers";
		  (receive_fun, status_fun))
	    val waiting = ref true
	   fun timeout_fun () =
	         (B.Scheduler.sleep timeout;
		  if ! waiting then
		   (B.Test.test ("timeout", fn _ => false);
		    B.Pipe.enqueue (queue, ()))
		  else ())
	   val _ = debug_constant_string "starting passive";
	   val (stop, get_conns) = Ip.start_passive (address,
						     Ip.Handler receive,
						     SOME 1)
	in debug_print (fn _ => "sleeping for " ^ makestring timeout ^
			" milli-seconds");
	   B.Scheduler.fork timeout_fun;
	   B.Pipe.dequeue queue;
	   waiting := false;
	   debug_constant_string "stopping the passive";
	   stop ();
	   debug_constant_string "closing opened passives";
	   map Ip.close (get_conns ());
	   local_print ("IP sent " ^
			B.V.Integer.makestring (Ip.packets_sent ()) ^
			" packets, received " ^
			B.V.Integer.makestring (Ip.packets_received ()) ^
			" packets");
	   B.Test.test ("receive", fn _ => ! receive_count = count)
	end;
	Tcp_Ip_Eth.finalize ();
	())
       handle _ => (Tcp_Ip_Eth.finalize (); ())

  fun server {size, count} =
       B.Test.tests ("IpServer", 1, run_server (size, count));

 end (* struct *)

(*
 ---------------------------------------------------------------------
	7.	structure Ip_Real
*)

structure Ip_Real = Ip_Real (structure B = Fox_Basis
                             structure Tcp_Ip_Eth = Tcp_Ip_Eth
			     val debug_level = NONE)
