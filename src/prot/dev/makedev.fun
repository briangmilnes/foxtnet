(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This file, sim.fun, provides a simulated ethernet driver that
	bounces back traffic to the same machine. Currently, it knows
	about three machines: wagosh, snow, and sly.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Device_To_Protocol
	2.	exceptions and debugging
	3.	required sub-structures
	4.	protocol types
	5.	protocol state
	6.	function compute_funs
	7.	function session

		iii.	RCS Log
	
$Log: makedev.fun,v $
Revision 1.9  1997/02/13  00:43:02  esb
updated to avoid compiler warning messages about non-generalizable variables.

Revision 1.8  1996/05/16  18:23:32  cline
handle closed connections in receive_data

Revision 1.7  1996/04/18  21:26:47  cline
converted hash from int to word

Revision 1.6  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.5  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.4  1995/10/02  23:49:13  cline
fixed listen count bug

Revision 1.3  1995/09/18  19:30:07  esb
adapted to new dev.sig.

Revision 1.2  1995/08/29  14:10:33  esb
adapted to new protoexn.sig

Revision 1.1  1995/06/20  16:52:02  esb
Initial revision


		1.	functor Device_To_Protocol
*)

functor Device_To_Protocol (structure Device: RAW_DEVICE
			    structure B: FOX_BASIS
			    val debug_level: int ref option): DEVICE_PROTOCOL =
 struct

(*
		2.	exceptions and debugging
*)

  structure X =
   struct
    exception Session of string
    exception Listen of string
    exception Connection of string
    exception Send of string
    exception Receive of string
    fun makestring (Session s) = SOME ("Session, " ^ s)
      | makestring (Listen s) = SOME ("Listen, " ^ s)
      | makestring (Connection s) = SOME ("Connection, " ^ s)
      | makestring (Send s) = SOME ("Send, " ^ s)
      | makestring (Receive s) = SOME ("Receive, " ^ s)
      | makestring Device.Session_Already_Open = SOME "device already open"
      | makestring _ = NONE
   end (* struct *)

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "makedev.fun"
			   val makestring = X.makestring)

(*
		3.	required sub-structures
*)

  structure Unit: KEY =
   struct
    type T = unit
    fun makestring () = "()"
    fun equal _ = true
    fun hash _ = 0w0
   end (* struct *)

  structure Setup = Device.Setup
  structure Address = Unit
  structure Pattern = Unit
  structure Connection_Key = Unit
  structure Incoming = Device.External
  structure Outgoing = Device.External
  structure Status = Unit
  structure Count =
   struct
    datatype continue = Continue | Done
    datatype T = Unlimited | Maximum of int | Incremental of unit -> continue
    fun makestring_continue Continue = "continue"
      | makestring_continue Done = "done"
    fun makestring Unlimited = "unlimited"
      | makestring (Maximum n) = Integer.toString n
      | makestring (Incremental f) = makestring_continue (f ())
   end (* struct *)

(*
		4.	protocol types
*)

  type connection_extension = unit
  type listen_extension = unit
  datatype dev_session_extension =
      Dev_Session_Extension of
        {local_address: Word_Array.T,
         packets_sent: unit -> Word64.word,
         packets_received: unit -> Word64.word,
         read_timeouts: unit -> Word64.word,
         failed_sends: unit -> Word64.word,
         packets_rejected: unit -> Word64.word}

  type session_extension = dev_session_extension

  datatype connection = C of {send: Outgoing.T -> unit,
			      abort: unit -> unit,
			      extension: connection_extension}

  datatype listen = L of {stop: unit -> unit, extension: listen_extension}

  datatype handler = H of Connection_Key.T
                   -> {connection_handler: connection -> unit,
	               data_handler: connection * Incoming.T -> unit,
	               status_handler: connection * Status.T -> unit}

  datatype session = S of {connect: Address.T * handler -> unit,
			   listen: Pattern.T * handler * Count.T -> listen,
			   extension: session_extension}

  exception Already_Open of Connection_Key.T

(*
		5.	protocol state
*)

  val handler_state = ref (NONE: (Device.External.T -> unit) option)
  val listen_state = ref (NONE: (handler*Count.T) option)

(*
		6.	function compute_funs
*)

  fun compute_funs session_fun {send, local_address, packets_sent,
				packets_received, read_timeouts,
				failed_sends, packets_rejected} =
       let val conn = C {send = send, abort = fn _ => (), extension = ()}
	   val extension = Dev_Session_Extension
	                     {local_address = local_address,
			      packets_sent = packets_sent,
			      packets_received = packets_received,
			      read_timeouts = read_timeouts,
			      failed_sends = failed_sends,
			      packets_rejected = packets_rejected}
	   fun call_handler handler packet =
	        handler (conn, packet)
	   fun connect ((), H h) =
	        case ! handler_state of
		   NONE =>
		    let val {connection_handler, data_handler,
			     status_handler} = h ()
		    in handler_state := SOME (call_handler data_handler);
		       ((connection_handler conn
			 before handler_state := NONE)
			handle x =>
			        (handler_state := NONE;
				 Trace.print_raise_again
				    (x, SOME "connection handler")))
		    end
		 | SOME _ =>
		    raise (Already_Open ())
	   fun listen ((), handler, count) =
	        case ! listen_state of
		   NONE =>
		    (listen_state := SOME (handler,count);
		     L {stop = fn _ => listen_state := NONE, extension = ()})
		 | SOME _ =>
		    raise X.Listen "already listening"
	   fun connect_thread (receive_synch_pipe, args) () = 
	        (B.Pipe.enqueue (receive_synch_pipe, ());
		 (connect args)
		 handle (Already_Open ()) => ()
		      | x =>
		         Trace.print_raise_again (x, SOME "connect_thread"))
	   fun receive_data packet =
	        case ! handler_state of
		   SOME f =>
		    (Trace.debug_constant_string "calling packet handler";
		     f packet)
		 | NONE =>
		    (case ! listen_state of
		        SOME (handler, count) =>
			 let val receive_synch_pipe = B.Pipe.new ()
			                            : unit B.Pipe.T
			 in case count of
			      Count.Unlimited => ()
			    | Count.Maximum 1 => listen_state := NONE
			    | Count.Maximum i =>
				listen_state := SOME (handler,
						      Count.Maximum (i-1))
			    | Count.Incremental f =>
				if f() = Count.Continue then ()
				else listen_state := NONE;
			    B.Scheduler.fork
			       (connect_thread (receive_synch_pipe,
						((), handler)));
			    Trace.debug_constant_string "waiting for handler";
			    B.Pipe.dequeue receive_synch_pipe;
			    Trace.debug_constant_string "receive done";
			    case !handler_state of
			      SOME f => (Trace.debug_constant_string
					   "calling packet handler";
					 f packet)
			    | NONE => Trace.trace_constant_string "dropping"
			 end
		      | NONE => Trace.debug_constant_string "dropping packet")
	   fun session_handler () =
	        session_fun (S {connect = connect, listen = listen,
				extension = extension})
       in (session_handler, receive_data)
       end
	
(*
		7.	function session
*)

  fun session (setup, session_fun) =
       Device.session (setup, compute_funs session_fun)

 end (* struct *)

