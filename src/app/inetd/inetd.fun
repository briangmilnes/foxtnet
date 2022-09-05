(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	The basic (internal) inetd services: daytime, discard, echo, time

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Inetd_Basic
	2.	functor Inetd
	3.	function main (for exportFn)

		iii.	RCS Log
	
$Log: inetd.fun,v $
Revision 1.5  1996/09/17  15:57:42  cline
handle interrupt (^C).  modified main for exportFn.

Revision 1.4  1996/07/22  17:50:21  cline
Use Transport.Transport_Pattern.port instead of Word16.word.

Revision 1.3  1996/06/07  20:26:26  cline
support user specified services

Revision 1.2  1996/05/29  14:16:52  esb
uncommented UDP, added chargen_port (but not the character generator itself).

Revision 1.1  1996/05/14  21:14:19  esb
Initial revision


		1.	functor Inetd_Basic
*)

functor Inetd_Basic (structure Transport: TRANSPORT_PROTOCOL
		     val services: (Transport.Transport_Pattern.port *
				    Transport.handler) list
		     val setup: Transport.Setup.T
		     structure B: FOX_BASIS
		     val debug_level: int ref option) =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val makestring = fn _ => NONE
			   val module_name = "inetd.fun")

  val daytime_port = Word16.fromInt 13
  val discard_port = Word16.fromInt 9
  val echo_port = Word16.fromInt 7
  val time_port = Word16.fromInt 37
  val chargen_port = Word16.fromInt 19

  structure String = Extern_String_Length (structure In = Transport.Incoming
					   structure Out = Transport.Outgoing
					   structure V = B.V)

  (* two connection handlers: one just waits, the other one
     sends data from a generator. *)
  fun wait pipe _ = B.Pipe.dequeue pipe

  fun act generate (Transport.C {send, ...}) = send (generate ())

  val connection_count = ref 0

  fun inc_count () =
       (connection_count := ! connection_count + 1;
	if ! connection_count >= 1000 then
	 (connection_count := 0;
	  System.Runtime.gc 0)
	else ())

  fun status_fun pipe (_, status) = 
       (B.Pipe.enqueue (pipe, ());
	Trace.trace_print (fn _ => "received status " ^
			   Transport.Status.makestring status ^
			   ", closing connection"))

  (* receive handlers. *)
  fun echo_fun (Transport.C {send, ...}, data) =
       let fun create (left, right) =
	        Transport.Outgoing.join (Transport.Outgoing.new left, right)
	   val empty = Transport.Outgoing.uninitialized 0w0
       in send (Transport.Incoming.fold (data, create, empty));
	  inc_count ()
       end

  fun discard_fun _ = inc_count ()

  (* data generators. *)
  fun daytime_fun () =
       let val time = B.V.Time.toDate (B.V.Time.now ()) ^ "\n"
	   val size = String.size time
	   val data = Transport.Outgoing.uninitialized size
       in String.marshal (data, time) 0w0;
          data
	  before inc_count ()
       end

  fun time_fun () =
       let val time = Word32.fromInt (B.V.Time.toSeconds (B.V.Time.now ()))
	   val time_array = Word_Array.from32
	                      (Word_Array.W32.U_Big.F.create (time, 0w1))
       in Transport.Outgoing.new time_array
	  before inc_count ()
       end

  fun wait_handler handler key =
       let val pipe = B.Pipe.new ()
       in {connection_handler = wait pipe, data_handler = handler,
	   status_handler = status_fun pipe}
       end

  fun act_handler generator key =
       {connection_handler = act generator, data_handler = discard_fun,
	status_handler = discard_fun}

  val echo_service    = (echo_port,    Transport.H (wait_handler echo_fun))
  val discard_service = (discard_port, Transport.H (wait_handler discard_fun))
  val daytime_service = (daytime_port, Transport.H (act_handler daytime_fun))
  val time_service    = (time_port,    Transport.H (act_handler time_fun))

  val all_services =
    [echo_service, discard_service, daytime_service, time_service] @ services

  fun session_fun time (session as (Transport.S {listen, ...})) =
       let fun pattern_con port =
	         Transport.Transport_Pattern.Local_Specified {local_port=port}
	   fun listen_to (port, handler) =
	     (listen (pattern_con port, handler, Transport.Count.Unlimited);
	      ())
       in app listen_to all_services;
	  case time of
	    SOME t => (Trace.local_print ("sleeping " ^ makestring t ^
					  " seconds");
		       B.Scheduler.sleep (t * 1000))
	  | NONE => (Trace.local_print "sleeping";
		     B.Scheduler.suspend (fn _ => ()));
	  Trace.local_print "done"
       end
      
  fun inetd time = Transport.session (setup, session_fun time)

 end

(*
		2.	functor Inetd
*)
functor Inetd (structure Tcp: TRANSPORT_PROTOCOL
	       structure Udp: TRANSPORT_PROTOCOL
	         sharing type Tcp.Setup.T = Udp.Setup.T
	       val tcp_services: (Tcp.Transport_Pattern.port *
				  Tcp.handler) list
	       val udp_services: (Udp.Transport_Pattern.port *
				  Udp.handler) list
	       val setup: Tcp.Setup.T
	       structure B: FOX_BASIS
	       val debug_level: int ref option) =
 struct
  structure Tcp_Inetd = Inetd_Basic (structure Transport = Tcp
				     val services = tcp_services
				     val setup = setup
				     val close_connections = false
				     structure B = B
				     val debug_level = debug_level)

  structure Udp_Inetd = Inetd_Basic (structure Transport = Udp
				     val services = udp_services
				     val setup = setup
				     val close_connections = true
				     structure B = B
				     val debug_level = debug_level)

  fun inetd time =
       let val pipe = B.Pipe.new ()
	   fun thread session () =
	        (session time;
		 B.Pipe.enqueue (pipe, ()))
       in B.Scheduler.fork (thread Tcp_Inetd.inetd);
          B.Scheduler.fork (thread Udp_Inetd.inetd);
	  B.Pipe.dequeue pipe;
	  B.Pipe.dequeue pipe
       end
  val inetd = Keyboard_Interrupts.protect inetd

(*
		3.	function main (for exportFn)
*)
  local
   fun usage _ = (print "usage: inetd [seconds]\n"; ~1)
  in
   fun main (name, []) = (inetd NONE; 0)
     | main (name, [""]) = (inetd NONE; 0)
     | main (name, [seconds]) =
	(case Int.fromString seconds of
	    NONE => usage ()
	  | SOME s => (inetd (SOME s); 0))
     | main _ = usage ()
  end

 end
