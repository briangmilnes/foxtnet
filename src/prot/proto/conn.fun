(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken.Cline@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	conn.fun: basic connection-handling for protocols.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	iv.	Overview
	v.	Limitations
	vi.	Functor Parameters
	1.	functor Connection
	2.	sub-structure and type redeclaration
	3.	structure state
	4.	function local_abort
	5.	function local_send
	6.	function dispatch
	7.	function match_passive
	8.	function compute_count
	9.	function try_passive
	10.	function failed_delivery
	11.	function local_data_handler
	12.	function local_status_handler
	13.	function conn_handler
	14.	function listen_handlers
	15.	function make_handlers
	16.	function disconnect_lower
	17.	function get_lower
	18.	function connect_lower
	19.	function deliver_queued_packets
	20.	function setup_connection
	21.	function exec_connection
	22.	function check_passive
	23.	function add_lower_listen
	24.	function start_lower_listen
	25.	function remove_limit
	26.	function stop_lower_listen
	27.	function stop_listen
	28.	internal value fold_funs
	29.	function local_listen
	30.	function local_connect
	31.	function remove_connections
	32.	function remove_passives
	33.	function call_status
	34.	function initialize_session
	35.	function finalize_session
	36.	function session

	iii.	RCS Log

$Log: conn.fun,v $
Revision 1.26  1997/02/13  00:46:03  esb
changed to use single-stores (single.sig) instead of general stores.

Revision 1.25  1997/02/11  00:28:59  esb
moved one variable into its proper scope, removed compiler warnings.

Revision 1.24  1996/12/20  21:42:23  esb
added some debugging messages.

Revision 1.23  1996/07/22  17:44:21  cline
*** empty log message ***

Revision 1.22  1996/07/15  17:46:15  esb
fixed an intermittent bug in connect.

Revision 1.21  1996/06/07  20:21:52  cline
fixed usage of before in exec_connection

Revision 1.20  1996/05/14  01:01:21  esb
minor fix in a debugging statement.

Revision 1.19  1996/05/08  02:02:22  esb
added a debugging message when raising Already_Open.

Revision 1.18  1996/02/23  21:36:58  esb
lots of cleanup.

Revision 1.17  1996/01/19  23:05:43  esb
adapted to the new wordarray signature.

Revision 1.16  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.15  1995/10/17  21:51:34  esb
fixed numerous bugs.

Revision 1.14  1995/10/04  18:52:30  esb
added the "undelivered" call.

Revision 1.13  1995/10/02  21:18:36  esb
cleaned up completion

Revision 1.12  1995/09/19  18:48:36  esb
minor fix.

Revision 1.11  1995/09/19  17:58:48  esb
printing of the state is now optional (debug_level of 1).

Revision 1.10  1995/09/12  16:28:35  esb
changed identify to return a list rather than an option.

Revision 1.9  1995/08/29  14:11:29  esb
now raises exception receive on failed receive.

Revision 1.8  1995/07/24  15:32:18  esb
added comments to describe the functor parameters.

Revision 1.7  1995/07/21  12:49:49  esb
added new parameters to make_key and match.

Revision 1.6  1995/07/03  23:31:07  esb
added the lower connection as an argument to init_connection.

Revision 1.5  1995/06/28  10:21:35  esb
cleaned up some synchronization statements.

Revision 1.4  1995/06/27  16:56:44  esb
added a parameter to make_key.

Revision 1.3  1995/06/26  17:28:48  esb
added some synchronization listen->packet delivery.

Revision 1.2  1995/06/23  19:56:38  esb
added call_status as an argument to init_proto.

Revision 1.1  1995/06/20  17:08:23  esb
Initial revision

Revision 1.6  1995/03/12  17:58:17  esb
adapted to new trace.sig.

Revision 1.5  1995/03/07  23:52:11  esb
updated tracing.

Revision 1.4  1995/01/22  00:44:03  esb
fixed a storage leak by no longer storing all passive connections,
which is a bug.  This bug will be fixed with the new PROTOCOL signature.

Revision 1.3  1994/09/30  17:05:06  esb
added lower_key_count.

Revision 1.2  1994/08/12  06:19:22  esb
added set_handler.

Revision 1.1  1994/08/02  20:36:34  esb
Initial revision

	iv.	Overview

This functor is designed to provide most of the connection and state
management functionality needed by most protocol implementations.  The
functor is instantiated by providing the set of functions and
structures, and the resulting structure is a full protocol.

	v.	Limitations

This functor is not designed to provide virtual protocol
functionality.

Also, there are three separate calls (identify, receive, and the
handler call) on receipt of every incoming message, and this may be
inefficient (increase the latency) for some protocols, though I
personally kind of doubt it.  Likewise for send.

	vi.	Functor Parameters

Lower is the lower protocol; Connection can only be used if there is a
lower protocol.  Setup and all the other structures and the extension
types are used to satisfy the protocol signature.  Protocol state is
any state kept by Connection on behalf of the entire protocol; there
is at most one state for any protocol at any time.  Connection state
is any state kept by Connection for each connection.

The specification of the functions provided as parameters to the
functor should be understandable from their signatures.  Lower setup
is called by connection to obtain a value of the lower setup type.
Init proto is called to obtain a value of protocol state, fin proto is
called right before this state is discarded.  Resolve is called during
connect to find out a lower address given a Connection address; it may
fail and return NONE, or succeed and return SOME loweraddress.  Make
key is called to compute a connection key when a connection is
established; map pattern converts a Connection pattern to a lower
pattern.  The "conns" and "listens" parameters to make key and map
pattern are meant to make it easy to test whether a connection or
listen has already been established.  Match is used to find out if a
listen matches a given connection key; the listen extension is
provided for reference.

Init connection is called once at connection establishment; fin
connection is correspondingly called at connection teardown.  Send is
called once for each outgoing packet (after partial application), and
converts an outgoing packet to a (possibly empty, possibly singleton)
list of lower protocol outgoing packets.  Identify and receive are
called for each incoming packet: identify may return a connection key;
receive is the opposite of send.  Lower status is called when a lower
status message is received.

	1.	functor Connection
*)

functor Connection (structure Lower: PROTOCOL
		    structure Setup: KEY
		    structure Address: KEY
		    structure Pattern: KEY
		    structure Connection_Key: KEY
		    structure Incoming: EXTERNAL
		    structure Outgoing: EXTERNAL
		    structure Status: PRINTABLE
		    structure Count: COUNT
		    structure X: PROTOCOL_EXCEPTIONS
		    type connection_extension
		    type listen_extension
		    type session_extension
		    type connection_state
		    type protocol_state
		    val lower_setup: Setup.T -> Lower.Setup.T
		    val init_proto: Setup.T * Lower.session
		                  * (Connection_Key.T * Status.T -> unit)
		                  -> (protocol_state * session_extension)
		    val fin_proto: protocol_state -> unit
		    val resolve: protocol_state * Address.T
		               -> Lower.Address.T option
		    val make_key: protocol_state * Address.T
		                * Lower.Connection_Key.T
				* {conns: unit -> Connection_Key.T list,
				   listens: unit
				          -> (Pattern.T
					      * listen_extension) list}
			        -> Connection_Key.T
		    val map_pattern: protocol_state * Pattern.T
				   * {conns: unit -> Connection_Key.T list,
				      listens: unit
				             -> (Pattern.T
						 * listen_extension) list}
		                   -> (listen_extension
				       * Lower.Pattern.T) option
		    val match: protocol_state * Pattern.T * listen_extension
		             * Connection_Key.T -> bool
		    val init_connection:
		          protocol_state * Connection_Key.T * Lower.connection
			  -> connection_state * connection_extension
		    val fin_connection: connection_state -> unit
		    val send: Connection_Key.T * connection_state
		            -> Outgoing.T -> Lower.Outgoing.T list
		    val identify: (Lower.Connection_Key.T * protocol_state)
		                -> Lower.Incoming.T -> Connection_Key.T list
		    val receive: Connection_Key.T * connection_state
		               -> Lower.Incoming.T -> Incoming.T option
		    val undelivered: (Lower.Connection_Key.T * protocol_state)
		                   -> (Lower.connection * Lower.Incoming.T)
		                   -> unit
		    val lower_status: protocol_state * Lower.Connection_Key.T
		                    -> Lower.Status.T -> unit
		    structure B: FOX_BASIS
		    val module_name: string
		    val debug_level: int ref option): PROTOCOL =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "conn.fun (" ^ module_name ^ ")"
			   val makestring = X.makestring)

(*
	2.	sub-structure and type redeclaration
*)

  structure Setup = Setup
  structure Address = Address
  structure Pattern = Pattern
  structure Connection_Key = Connection_Key
  structure Incoming = Incoming
  structure Outgoing = Outgoing
  structure Status = Status
  structure Count = Count
  structure X = X

  type connection_extension = connection_extension
  type listen_extension = listen_extension
  type session_extension = session_extension

  datatype connection = C of {send: Outgoing.T -> unit,
		     abort: unit -> unit,
	             extension: connection_extension}

  exception Already_Open of Connection_Key.T

  datatype listen = L of {stop: unit -> unit, extension: listen_extension}

  datatype handler = H of Connection_Key.T
                  -> {connection_handler: connection -> unit,
	              data_handler: connection * Incoming.T -> unit,
	              status_handler: connection * Status.T -> unit}

  datatype session = S of {connect: Address.T * handler -> unit,
		     listen: Pattern.T * handler * Count.T -> listen,
	             extension: session_extension}

(*
	3.	structure state

Implementation Note:

The store "lower" contains a pointer to the lower connection, as well
as a pipe that will terminate the lower connection (when given a
return pipe option) and a reference count.  The count is the number of
local connections in the "conn" store that use this lower connection.
The reference count is started at one if the lower connection was
opened as a result of a local connect, and at zero if it was opened as
a result of a lower listen completion.
*)

  local
   datatype 'a result = Result of 'a | Exception of exn

   type session_id = bool ref

   local
(* for debugging reference counts
    datatype x = Still_Open of int | Closed of int
    val count = ref 0
    fun gen_id () =
         ! count
         before count := ! count + 1
*)
   in
(*
    type openp_connection = x
    fun openp_string (Still_Open id) = "open (" ^ Integer.toString id ^ ")"
      | openp_string (Closed id) = "closed (" ^ Integer.toString id ^ ")"
    fun trace_openp (string, openp) =
	 (Trace.local_print (string ^ openp_string openp);
	  openp)
    fun init_openp () = trace_openp ("creating openp ", Still_Open (gen_id ()))
    fun is_open s =
         (trace_openp ("testing ", s);
	  case s of Still_Open _ => true | _ => false)
    fun close_openp (s as (Still_Open id)) =
         trace_openp ("closing ", Closed id)
      | close_openp s = trace_openp ("re-closing ", s)
*)
(* for production code *)
    type openp_connection = bool
    val openp_string = Bool.toString
    fun init_openp () = true
    fun is_open b = b
    fun close_openp _ = false
   end

   datatype conn_info =
       Conn of {session_id: session_id,
		valid: openp_connection ref,
		state: connection_state,
		connection: connection,
		data_handler: Lower.Incoming.T -> unit,
		status_handler: Status.T -> unit,
		lower: Lower.Connection_Key.T}
     | Pending_Conn of {session_id: session_id,
			valid: openp_connection ref,
			queue: Lower.Incoming.T B.Pipe.T,
			lower: Lower.Connection_Key.T}
   structure Conns = Single_Store (structure V = B.V
				   type key = Connection_Key.T
				   type value = conn_info
				   val eq = Connection_Key.equal
				   val hash = Connection_Key.hash)

   fun makestring_conns () =
        Conns.makestring (fn (k, v) =>
			   (Connection_Key.makestring k ^
			    (case v of Pending_Conn _ => "p" | _ => "")),
			   ", ")

   type passive_info = session_id * Count.T ref * handler
                     * (unit -> unit) * listen_extension
   structure Passives = Single_Store (structure V = B.V
				      type key = Pattern.T
				      type value = passive_info
				      val eq = Pattern.equal
				      val hash = Pattern.hash)
   fun makestring_passives () =
        Passives.makestring (fn (k, v) => Pattern.makestring k, ", ")

   type lower_passive_info = (unit -> unit) * (Pattern.T * Count.T ref) list
   structure Lower_Passives = Single_Store (structure V = B.V
					    type key = Lower.Pattern.T
					    type value = lower_passive_info
					    val eq = Lower.Pattern.equal
					    val hash = Lower.Pattern.hash)
   fun makestring_lower_passives () =
        Lower_Passives.makestring (fn (k, v) =>
				     Lower.Pattern.makestring k, ", ")

   local
(* for debugging reference counts
    datatype x = R of int * int
    val count = ref 0
    fun gen_id () =
         ! count
         before count := ! count + 1
*)
   in
(*
    type refcnt = x
    fun lezero (R (a, _)) = a <= 0
    fun lesszero (R (a, _)) = a < 0
    fun refcnt_string (R (a, id)) =
         Integer.toString a ^ "_" ^ Integer.toString id
    fun trace_refcnt (string, count) =
	 (Trace.local_print (string ^ refcnt_string count);
	  count)
    fun newrefcnt a =
         trace_refcnt ("creating reference count with value ",
		       R (a, gen_id ()))
    fun refcnt_inc (R (a, id)) =
         trace_refcnt ("increasing reference count to ", R (a + 1, id))
    fun refcnt_dec (R (a, id)) =
         trace_refcnt ("decreasing reference count to ", R (a - 1, id))
*)
(* for production code *)
    type refcnt = int
    fun lezero a = a <= 0
    fun lesszero a = a < 0
    val refcnt_string = Integer.toString
    fun newrefcnt a = a
    fun refcnt_inc a = a + 1
    fun refcnt_dec a = a - 1
   end

   type lower_info = (refcnt * unit B.Pipe.T option B.Pipe.T
		      * Lower.connection)
   structure Lower_Conns = Single_Store (structure V = B.V
					 type key = Lower.Connection_Key.T
					 type value = lower_info
					 val eq = Lower.Connection_Key.equal
					 val hash = Lower.Connection_Key.hash)

   fun makestring_lower () =
        Lower_Conns.makestring (fn (k, (r, _, _)) =>
				  (Lower.Connection_Key.makestring k ^ "/" ^
				   refcnt_string r), ", ")

   type protocol_info = int * (protocol_state * session_extension)
   structure Protocol_Info = Single_Store (structure V = B.V
					   type key = Setup.T
					   type value = protocol_info
					   val eq = Setup.equal
					   val hash = Setup.hash)

   fun makestring_protocol_info () =
        Protocol_Info.makestring (fn (k, v) => Setup.makestring k, ", ")

   type lower_session_pipe = unit B.Pipe.T B.Pipe.T
   val lower_session = ref (NONE: (Lower.session * lower_session_pipe) option)
   fun makestring_lower_session () =
        case ! lower_session of
	   NONE => ""
	 | SOME _ => "has lower session"

(* for debugging semaphores and deadlocks
   fun sem_new name =
        (Trace.trace_print (fn _ => "allocating new semaphore " ^ name);
	 (B.Semaphore.new (), name))

   fun sem_acquire ((sem, name), function) =
        if B.Semaphore.free sem then
	 (Trace.trace_print (fn _ => function ^ " acquiring semaphore " ^
			     name);
	  B.Semaphore.acquire sem)
	else
	 (Trace.trace_print (fn _ => function ^ " waiting for semaphore " ^
			     name);
	  B.Semaphore.acquire sem;
	  Trace.trace_print (fn _ => function ^ " acquired semaphore " ^
			     name))

   fun sem_release ((sem, name), function) =
        if B.Semaphore.free sem then
	 (Trace.trace_print (fn _ => function ^
			     " double-releasing semaphore " ^ name);
	  B.Semaphore.release sem)
	else
	 (Trace.trace_print (fn _ => function ^ " releasing semaphore " ^
			     name);
	  B.Semaphore.release sem)

   fun sem_with_lock (sem, f, arg, function) =
        (sem_acquire (sem, function);
	 f arg
	 before sem_release (sem, function))

*)
(* for production code *)
   fun sem_new name = B.Semaphore.new ()

   fun sem_acquire (sem, _) = B.Semaphore.acquire sem

   fun sem_release (sem, _) = B.Semaphore.release sem

   fun sem_with_lock (sem, f, arg, _) = B.Semaphore.with_lock (sem, f, arg)

   val session_semaphore = sem_new "session_semaphore"

(*
	4.	function local_abort

	Invariant: this connection is in the store and also is counted
	in the reference count for the lower connection IFF its valid
	field says it is open.  Otherwise, and even though there may
	be a connection with the same key in the store, it is not the
	same connection.

	Local_abort should only be called when holding the session
	semaphore, since disconnect_lower should only be called when
	holding the session semaphore.
*)

   fun local_abort (key, valid, lower_key, disconnect_lower) =
	if is_open (! valid) then
	 (Conns.remove key;
	  valid := close_openp (! valid);
	  Trace.debug_print (fn _ =>
			     "local_abort: disconnect_lower for " ^
			     Lower.Connection_Key.makestring lower_key ^
			     ", key " ^ Connection_Key.makestring key);
	  disconnect_lower lower_key)
	else
	 Trace.debug_print (fn _ => "local_abort, connection " ^
			    Connection_Key.makestring key ^
			    " already invalidated")

(*
	5.	function local_send
*)

   fun local_send (key, valid, lower_send, send_fun) packet =
        if is_open (! valid) then
	 app lower_send (send_fun packet)
	else
	 Trace.print_raise (X.Send ("connection " ^
				    Connection_Key.makestring key ^
				    " aborted"), SOME "local_send")

(*
	6.	function dispatch
*)

   fun dispatch (key, valid, connection, receive_fun, upper_handler) incoming =
        if is_open (! valid) then
	 case receive_fun incoming of
	    SOME packet =>
	     upper_handler (connection, packet) (* normal case *)
	  | NONE =>
	     Trace.debug_print (fn _ =>
				"receive on connection " ^
				Connection_Key.makestring key ^
				" consumed packet " ^
				Lower.Incoming.makestring_max (incoming, 0w30))
	else
	 Trace.trace_print (fn _ =>
			    "received packet for closed connection " ^
			    Connection_Key.makestring key ^
			    ", first 50 bytes = " ^ 
			    Lower.Incoming.makestring_max (incoming, 0w50) ^
			    ", discarding")

(*
	7.	function match_passive
*)

   fun match_passive (state, key) ((pattern, value as (_, _, _, _, e)), NONE) =
        if match (state, pattern, e, key) then SOME (key, value) else NONE
     | match_passive (state, key) (_, SOME x) = SOME x

(*
	8.	function compute_count
*)

   fun compute_count (limit, stop) =
        case ! limit of
	   Count.Unlimited => ()
	 | Count.Maximum n =>
	    if n > 1 then limit := Count.Maximum (n - 1)
	    else
	     (limit := Count.Maximum 0;
	      stop ())
	 | Count.Incremental f =>
	     (case f () of
	       Count.Continue => ()
	     | Count.Done =>
		(limit := Count.Maximum 0;
		 stop ()));

(*
	9.	function try_passive

	When adding a connection, try_passive must check that the
	lower connection is open (with the session semaphore lock)
	since the lower connection might have been closed since the
	packet was received by the data handler (e.g.,
	Semaphore.acquire could context switch).  The other crucial
	detail is that the semaphore must be released in each branch
	of the case statement.
*)

   datatype successful_try_passive = Try_Passive_Success | Try_Passive_Failure

   fun try_passive (session_id, state, key, lower_key,
		    lower_conn, incoming, exec) =
        case Passives.fold (match_passive (state, key)) NONE of
	   SOME (pattern, (session_id, limit, H handler, stop, extension)) =>
	    let val pending_packets = B.Pipe.new (): Lower.Incoming.T B.Pipe.T
	    in sem_acquire (session_semaphore, "try_passive");
               case (! session_id, Lower_Conns.look lower_key,
		     Conns.look key) of
		  (true, SOME (count, pipe, lower_conn), NONE) =>
		   (Trace.debug_print (fn _ =>
				       "try_passive increasing refcnt to " ^
				       refcnt_string (refcnt_inc count) ^
				       " for lower connection " ^
				       Lower.Connection_Key.makestring
				       lower_key);
		    Lower_Conns.add (lower_key,
				     (refcnt_inc count, pipe, lower_conn));
		    Trace.trace_print (fn _ =>
				       "storing pending connection for key " ^
				       Connection_Key.makestring key);
		    Conns.add (key,
			       Pending_Conn {session_id = session_id,
					     valid = ref (init_openp ()),
					     queue = pending_packets,
					     lower = lower_key});
		    B.Pipe.enqueue (pending_packets, incoming);
	            sem_release (session_semaphore, "try_passive 1");
		    compute_count (limit, stop);
		    B.Scheduler.fork (fn _ =>
				      exec (session_id, state, key, lower_key,
					    lower_conn, handler));
		    Try_Passive_Success)
		| (false, _, _) =>
		   (sem_release (session_semaphore, "try_passive 2");
		    Trace.trace_print (fn _ =>
				       "try_passive dropping packet because " ^
				       " session ended");
		    Try_Passive_Failure)
		| (_, _, SOME (Pending_Conn {queue, ...})) =>
		   (sem_release (session_semaphore, "try_passive 3");
		    Trace.trace_print (fn _ =>
				       "try_passive queueing packet for " ^
				       Connection_Key.makestring key);
		    B.Pipe.enqueue (queue, incoming);
		    Try_Passive_Success)
		| (_, _, SOME (Conn {data_handler, valid, ...})) =>
		   (sem_release (session_semaphore, "try_passive 4");
		    Trace.trace_print (fn _ =>
				       "try_passive delivering packet for " ^
				       Connection_Key.makestring key);
		    if not (is_open (! valid)) then
		     Trace.local_print ("connection " ^
					Connection_Key.makestring key ^
					" is in store but closed, keys are " ^
					makestring_conns ())
		    else ();
		    data_handler incoming;
		    Try_Passive_Success)
		| (_, NONE, _) =>
		   (sem_release (session_semaphore, "try_passive 5");
		    Trace.trace_print (fn _ =>
				       "try_passive dropping packet because" ^
				       " no lower connection " ^
				       Lower.Connection_Key.makestring
				       lower_key);
		    Try_Passive_Failure)
	    end
	 | NONE =>
	    (Trace.trace_print (fn _ =>
				"no listen for connection " ^
				Connection_Key.makestring key ^
				" from lower connection " ^
				Lower.Connection_Key.makestring lower_key ^
				", currently listening for " ^
				Passives.makestring (fn (k, v) =>
						     Pattern.makestring k,
						     ", "));
	     Try_Passive_Failure)

(*
	10.	function failed_delivery
*)

   fun failed_delivery (lower_key, lower_conn, undeliver, data) =
        (case Lower_Conns.look lower_key of
	    NONE =>
	     Trace.trace_print (fn _ =>
				"failed delivery and no lower connection " ^
			         Lower.Connection_Key.makestring lower_key ^
				 ", dropping packet ")
	  | SOME (count, pipe, _) =>
	     (Trace.trace_print (fn _ => "dropping packet from lower " ^
				 Lower.Connection_Key.makestring lower_key ^
				 " with refcnt " ^
				 refcnt_string count);
	      B.Pipe.enqueue (pipe, NONE);
	      undeliver (lower_conn, data)))

(*
	11.	function local_data_handler
*)

   fun local_data_handler (session_id, state, lower_key, identify, exec,
			   undeliver) (lower_conn, incoming) =
	case identify incoming of
	   [] =>
	    failed_delivery (lower_key, lower_conn, undeliver, incoming)
	 | key :: rest =>
	    (case Conns.look key of
	        SOME (Conn {data_handler, valid, ...}) =>
		 (Trace.debug_constant_string "calling packet handler"; 
		  if not (is_open (! valid)) then
		   Trace.local_print ("connection " ^
				      Connection_Key.makestring key ^
				      " is in store but closed, keys are " ^
				      makestring_conns ())
		  else ();
		  data_handler incoming)
	      | SOME (Pending_Conn {queue, ...}) =>
		 (Trace.debug_constant_string "pending connection"; 
		  B.Pipe.enqueue (queue, incoming))
	      | NONE =>			(* try a passive open *)
		 (Trace.debug_print (fn _ => "no connection for key " ^
				     Connection_Key.makestring key ^
				     ", connections are " ^
				     makestring_conns ());
		  case try_passive (session_id, state, key, lower_key,
				    lower_conn, incoming, exec) of
		     Try_Passive_Success => ()
		   | Try_Passive_Failure =>
		      (case rest of
		          [] =>
			   failed_delivery (lower_key, lower_conn,
					    undeliver, incoming)
		        | _ =>
			   (Trace.trace_print
			     (fn _ => "no match for key " ^
			      Connection_Key.makestring key ^
			      ", trying next");
			    local_data_handler (* recurse w/ new "identify" *)
			       (session_id, state, lower_key,
			        fn _ => rest, exec, undeliver)
			       (lower_conn, incoming)))))

(*
	12.	function local_status_handler
*)

   fun local_status_handler arg (_, status) = lower_status arg status

(*
	13.	function conn_handler

	conn_handler gets signaled whenever someone is ready to let
	the connection close, either because the refcount is
	decremented, or because the refcount was not incremented (in
	the case of a packet which cannot be delivered). If the
	refcount is zero, we return, otherwise we loop.

	Note: without end_loop, there is a race condition that occurs
	when disconnect_lower signals the queue (e.g., because the
	session ends) and waits for confirmation.  failed_delivery may
	have signaled the queue just before disconnect_lower, and
	since when conn_handler gets control the reference count is
	zero, wait_for_end would terminate without signaling the
	pending disconnect_lower.  To avoid this problem, end_loop
	loops until there is nobody left on the queue.
*)

   fun conn_handler (lower_key, synch, done, name, count) lower_conn =
        let val lower_conn_info = (newrefcnt count, done, lower_conn)
	    fun enqueue_done NONE = ()
	      | enqueue_done (SOME pipe) = B.Pipe.enqueue (pipe, ())
	    fun end_loop () =
	         case B.Pipe.dequeue_immediately done of
		    NONE => ()
		  | SOME done_pipe =>
		     (Trace.trace_print
		         (fn _ => name ^ "/conn_handler signaling end for " ^
			  Lower.Connection_Key.makestring lower_key);
		      enqueue_done done_pipe;
		      end_loop ())
	    fun wait_for_end () =
	         let val done_pipe = B.Pipe.dequeue done
		 in case Lower_Conns.look lower_key of
		       NONE =>
		        (enqueue_done done_pipe;
			 Trace.print_raise
			    (X.Connection ("lower " ^
					   Lower.Connection_Key.makestring
					   lower_key ^
					   " already removed from store"),
			     SOME "conn_handler"))
		     | SOME (count, _, _) =>
			if lezero count then
		         (if lesszero count then
			   Trace.local_print
			      (name ^ "/conn_handler removing lower " ^
			       Lower.Connection_Key.makestring lower_key ^
			       ", refcnt is " ^ refcnt_string count)
			  else
			   Trace.trace_print
		              (fn _ => name ^
			       "/conn_handler removing lower " ^
			       Lower.Connection_Key.makestring lower_key ^
			       ", refcnt is " ^ refcnt_string count);
		          Lower_Conns.remove lower_key;
			  enqueue_done done_pipe;
			  B.Scheduler.fork end_loop) (* return ASAP *)
			else
			 (Trace.trace_print
		            (fn _ => name ^
			     "/conn_handler restarting on lower " ^
			     Lower.Connection_Key.makestring lower_key ^
			     ", refcnt is " ^ refcnt_string count);
			  enqueue_done done_pipe;
			  wait_for_end ())
		    end
	in case Lower_Conns.look lower_key of
	      NONE => ()
	    | SOME _ =>
	       Trace.print_raise (X.Connection
				  ("lower " ^
				   Lower.Connection_Key.makestring lower_key ^
				   " already in store"),
				  SOME "conn_handler");
	   Trace.debug_print (fn _ => name ^
			      "/conn_handler adding with refcnt " ^
			      Integer.toString count ^
			      " for lower " ^
			      Lower.Connection_Key.makestring lower_key);
	   Lower_Conns.add (lower_key, lower_conn_info);
	   B.Pipe.enqueue (synch, Result (lower_key, lower_conn));
	   Trace.debug_constant_string "lower conn_handler waiting";
	   wait_for_end ()
	end

(*
	14.	function listen_handlers
*)

   fun listen_handlers (session_id, state, lower_pattern, exec) lower_key =
        let val done = B.Pipe.new (): unit B.Pipe.T option B.Pipe.T
	    val dummy = B.Pipe.new ()
	              : (Lower.Connection_Key.T
			 * Lower.connection) result B.Pipe.T
	in Trace.debug_print (fn _ => "computed listen handlers for key " ^
			      Lower.Connection_Key.makestring lower_key);
	   {connection_handler = conn_handler (lower_key, dummy,
					       done, "listen", 0),
	    data_handler = local_data_handler (session_id, state, lower_key,
					       identify (lower_key, state),
					       exec,
					       undelivered (lower_key, state)),
	    status_handler = local_status_handler (state, lower_key)}
	end

(*
	15.	function make_handlers
*)

   fun make_handlers (session_id, state, synch, done, exec) lower_key =
        {connection_handler = conn_handler (lower_key, synch, done,
					    "connect", 1),
	 data_handler = local_data_handler (session_id, state, lower_key,
					    identify (lower_key, state), exec,
					    undelivered (lower_key, state)),
	 status_handler = local_status_handler (state, lower_key)}

(*
	16.	function disconnect_lower

	If the lower connection is to be closed, this function
	actually waits for the connection handler to terminate
	(by enqueuing on a pipe) before returning.
*)

   fun disconnect_lower lower_key =
        case Lower_Conns.look lower_key of
	   SOME (count, signal_pipe, lower_conn) =>
	    let val wait_pipe = B.Pipe.new (): unit B.Pipe.T
	        val new_count = refcnt_dec count
	    in Lower_Conns.add (lower_key,
				(new_count, signal_pipe, lower_conn));
	       Trace.debug_print (fn _ =>
				  "disconnect_lower refcnt is now " ^
				  refcnt_string new_count ^
				  " for lower connection " ^
				  Lower.Connection_Key.makestring lower_key);
	       if lezero new_count then
		(if lesszero new_count then
		  Trace.local_print ("disconnect_lower refcnt is " ^
				     refcnt_string new_count)
		 else ();
		 Trace.debug_constant_string
		       "disconnect_lower signaling lower conn_handler";
	         B.Pipe.enqueue (signal_pipe, SOME wait_pipe);
		 Trace.debug_constant_string
		       ("disconnect_lower waiting for " ^
			"lower conn_handler to complete");
	         B.Pipe.dequeue wait_pipe;
	         Trace.debug_constant_string "disconnect_lower done")
	       else ()
	    end
	 | NONE =>
	    Trace.print_raise (X.Connection "no lower connection",
			       SOME "disconnect_lower")

(*
	17.	function get_lower

	Get a lower connection from the state, returning NONE
	if the connection is not available.
*)

   fun get_lower lower_key =
        case Lower_Conns.look lower_key of
	   SOME (count, pipe, lower_conn) =>
	    (Trace.debug_print (fn _ => "increasing refcnt to " ^
				refcnt_string (refcnt_inc count) ^
				" for lower connection " ^
				Lower.Connection_Key.makestring lower_key);
	     Lower_Conns.add (lower_key, (refcnt_inc count, pipe, lower_conn));
	     SOME (lower_key, lower_conn))
	 | NONE => NONE

(*
	18.	function connect_lower

	Two possibilities: either the connection does not exist, in
	which case connect_lower creates it, or it does exist, in
	which case the caller to connect_lower will get Already_Open
	and be handed the connection key which can be used as an index
	into the lower store to retrieve the connection.

	Note: unfortunately the existence of a lower connection is not
	atomically determinable.  In particular, we need two separate
	events to be in the same state:

	- the connection being in the store (lower_conns)

	- the connection being open in the lower protocol.

	Since there is no practical way to synchronize these two, and
	in particular the situation is legal where the connection has
	been removed from lower_conns but the lower protocol hasn't
	completed closing it yet, we call connect_thread up to N times
	if we keep getting Lower.Already_Open but the connection is
	not in the lower_conns store (as shown by get_lower raising an
	exception).
*)

   fun connect_lower (session_id, protocol_state, lower_address, exec) =
        let val synch = B.Pipe.new (): (Lower.Connection_Key.T
					* Lower.connection) result B.Pipe.T
	    val done = B.Pipe.new (): unit B.Pipe.T option B.Pipe.T
	    type lower_handler =
	           Lower.Connection_Key.T
		   -> {connection_handler: Lower.connection -> unit,
		       data_handler: Lower.connection * Lower.Incoming.T
		                   -> unit,
		       status_handler: Lower.connection * Lower.Status.T
		                     -> unit}
	    val handler: lower_handler
                       = make_handlers (session_id, protocol_state,
					synch, done, exec)
	    val connect = case ! lower_session of
	                     SOME (Lower.S {connect, ...}, _) => connect
			   | NONE =>
			      Trace.print_raise (X.Session "no lower session",
						 SOME "connect_lower")
	    fun connect_thread count () =
	         ((connect (lower_address, Lower.H handler))
		  handle Lower.Already_Open lower_key =>
		          (case ((get_lower lower_key) handle _ => NONE) of
			      SOME x => B.Pipe.enqueue (synch, Result x)
			    | _ =>
			       if count <= 1 then
				(Trace.local_print ("in connect_lower, " ^
						    "raising exception " ^
						    "'Connection, " ^
						    "no lower connection'");
				 B.Pipe.enqueue
				    (synch,
				     Exception (X.Connection
						"no lower connection")))
			       else connect_thread (count - 1) ())
		       | x =>
			  (Trace.print_handled (x, SOME "connect_lower");
		           B.Pipe.enqueue (synch, Exception x)))
	in B.Scheduler.fork (connect_thread 3);
	   case B.Pipe.dequeue synch of
	      Result v => v
	    | Exception x => Trace.print_raise_again (x, SOME "connect_lower")
	end

(*
	19.	function deliver_queued_packets

	Asynchronously call the data handler with all the packets in
	the queue.  If the connection is closed while we do this,
	simply go away.
*)

   fun deliver_queued_packets (_, _, NONE, _) () = ()
     | deliver_queued_packets (data_handler, valid, SOME queue, key) () =
        let fun call packet () =
	         if is_open (! valid) then
		  (Trace.debug_print (fn _ => "delivering pending packet to " ^
				      Connection_Key.makestring key);
		   data_handler packet)
		 else ()
	    fun loop NONE = ()
	      | loop (SOME packet) =
	         (B.Scheduler.fork (call packet);
		  loop (B.Pipe.dequeue_immediately queue))
        in Trace.debug_print (fn _ => "delivering " ^
			      Integer.toString (B.Pipe.size queue) ^
			      " initial packets for key " ^
			      Connection_Key.makestring key);
	   loop (B.Pipe.dequeue_immediately queue)
	end

(*
	20.	function setup_connection

	This must be executed under the session semaphore, and
	therefore should not block.
*)

   fun setup_connection (session_id, key, state, ext, data_handler,
			 status_handler, lower_key, lower_send) =
        let val (valid, queue) =
	      case Conns.look key of
	         SOME (Conn {valid, lower, ...}) =>
	          (local_abort (key, valid, lower, disconnect_lower);
		   Trace.print_raise (X.Connection
				      ("connection " ^
				       Connection_Key.makestring key ^
				       " already open, aborting"),
				      SOME "setup_connection"))
	       | SOME (Pending_Conn {queue, valid, ...}) =>
		  (valid, queue)
	       | NONE =>
		  (disconnect_lower lower_key;
		   Trace.print_raise (X.Connection
				      ("connection " ^
				       Connection_Key.makestring key ^
				       " not in store, aborting"),
				      SOME "setup_connection"))
	    val send_fun = local_send (key, valid, lower_send,
				       send (key, state))
	    fun abort_fun () =
	         sem_with_lock (session_semaphore, local_abort,
				(key, valid, lower_key,
				 disconnect_lower),
				"setup_connections/abort_fun")
	    val conn = C {send = send_fun, abort = abort_fun, extension = ext}
	    fun status s = status_handler (conn, s)
	    val data = dispatch (key, valid, conn,
				 receive (key, state), data_handler)
	    val conn_info = Conn {session_id = session_id,
				  valid = valid,
				  state = state,
				  connection = conn,
				  data_handler = data,
				  status_handler = status,
				  lower = lower_key}
	in Conns.add (key, conn_info);
	   (conn, data, queue, valid, abort_fun)
	end

(*
	21.	function exec_connection

	Call the user handler and the protocol functions to initialize
	and finalize the connection.
*)

   fun exec_connection debug_name
                       (session_id, protocol_state, key, lower_key,
			lower_conn, handler) =
        let val Lower.C {send = lower_send, abort = lower_abort,
			 extension = lower_ext} = lower_conn
(* initialize connection for this protocol. *)
	    val (state, ext) =
	          ((init_connection (protocol_state, key, lower_conn))
		   handle x =>
	                   Trace.print_raise_again (x,
						    SOME (debug_name ^
							  "/exec_connection/" ^
							  "init_connection")))
(* get handlers from higher protocol. *)
	    val {connection_handler, data_handler,
		 status_handler} = handler key
(* initialize the internal state. *)
	    val setup_args = (session_id, key, state, ext,
			      data_handler, status_handler,
			      lower_key, lower_send)
	    fun fin x = 
	         (fin_connection state;
		  Trace.print_raise_again (x, SOME (debug_name ^
						    "/exec_connection/fin")))
	    val setup_call = (session_semaphore, setup_connection, setup_args,
			      debug_name ^ "/exec_connection")
	    val (conn, data, queue, valid, abort_fun) =
	          ((sem_with_lock setup_call)
		   handle x => fin x)
	    fun cleanup () =
	      (abort_fun ();
	       fin_connection state
	       handle x => Trace.print_handled (x, SOME (debug_name ^
							 "/exec_connection/" ^
							 "fin_connection")))
	in B.Scheduler.fork (deliver_queued_packets (data, valid, SOME queue,
						     key));
	  ((connection_handler conn)
	   handle x =>
	           Trace.print_handled (x, SOME (debug_name ^
						 "/exec_connection/handler")));
	  cleanup ()
	end
	handle x => Trace.print_handled (x, SOME (debug_name ^
						  "/exec_connection/main"))

(*
	22.	function check_passive
*)

   fun check_passive lower_pattern () =
        case Lower_Passives.look lower_pattern of
	   SOME (_, []) => Lower.Count.Done
	 | SOME (_, _) => Lower.Count.Continue
	 | NONE => Lower.Count.Done

(*
	23.	function add_lower_listen
*)

   fun add_lower_listen (session_id, protocol_state, lower_pattern,
			 listen, pattern, limit) =
        let val limit_fun = check_passive lower_pattern
	    val lower_limit = Lower.Count.Incremental limit_fun
	    val handler =  Lower.H (listen_handlers
				    (session_id, protocol_state,
				     lower_pattern, exec_connection "listen"))
	    val listen_args = (lower_pattern, handler, lower_limit)
	    val _ = Trace.debug_constant_string "calling lower listen"
	    val Lower.L {stop, extension} = listen listen_args
	    val record = (stop, [(pattern, limit)])
	in Lower_Passives.add (lower_pattern, record)
	end

(*
	24.	function start_lower_listen
*)

   val lower_listen_semaphore = sem_new "lower_listen_semaphore"

   fun start_lower_listen (pattern, lower_pattern, limit,
			   session_id, protocol_state, synch) =
        case Lower_Passives.look lower_pattern of
	   NONE =>
	    (case ! lower_session of
	        SOME (Lower.S {listen, ...}, _) =>
		 (sem_with_lock (lower_listen_semaphore,
				 add_lower_listen,
				 (session_id, protocol_state,
				  lower_pattern, listen, pattern, limit),
				 "start_lower_listen");
		  B.Pipe.enqueue (synch, ());
		  ())
	      | NONE => Trace.print_raise (X.Session "not initialized",
					   SOME "start_lower_listen"))
	 | SOME (stop, list) =>
	    (Trace.debug_print (fn _ =>
				"have lower listen, adding " ^
				Pattern.makestring pattern ^ " to [" ^
				(B.V.List.fold (fn ((p, _), s) =>
						Pattern.makestring p ^
						", " ^ s)
				               list "]"));
	     Lower_Passives.add (lower_pattern,
				 (stop, (pattern, limit) :: list));
	     B.Pipe.enqueue (synch, ());
	     ())

(*
	25.	function remove_limit
*)

   fun remove_limit (limit, []) = []
     | remove_limit (limit, (head as (pattern, this_limit)) :: rest) =
        if limit = this_limit then rest
	else head :: remove_limit (limit, rest)

(*
	26.	function stop_lower_listen
*)

   fun stop_lower_listen (lower_pattern, limit) =
        case Lower_Passives.look lower_pattern of
	   SOME (lower_stop, list) =>
	    (case remove_limit (limit, list) of
	        [] =>
		 (Lower_Passives.remove lower_pattern;
		  lower_stop ())
	      | new_list =>
		 Lower_Passives.add (lower_pattern, (lower_stop, new_list)))
	 | NONE =>
	    Trace.print_raise (X.Listen "no waiting lower passive",
			       SOME "stop")

(*
	27.	function stop_listen
*)

   fun stop_listen (pattern, lower_pattern, limit) () =
        (Trace.debug_print (fn _ => "stop_listen (" ^
			    Pattern.makestring pattern ^ ")");
	 case Passives.look pattern of
	    SOME _ =>
	     (limit := Count.Maximum 0;
	      Passives.remove pattern;
	      sem_with_lock (lower_listen_semaphore, stop_lower_listen,
			     (lower_pattern, limit), "stop_listen"))
	  | NONE => ())

(*
	28.	internal value fold_funs
*)

   local
    fun get_connections () = Conns.fold (fn ((k, _), a) => (k :: a)) []

    fun get_passives () =
         Passives.fold (fn ((p, (_, _, _, _, ext)), a) => ((p, ext) :: a)) []

   in
    val fold_funs = {conns = get_connections, listens = get_passives}
   end

(*
	29.	function local_listen
*)

   fun local_listen (session_id, protocol_state) (pattern, H handler, limit) =
        case Passives.look pattern of
	   NONE =>
	    (case map_pattern (protocol_state, pattern, fold_funs) of
	        SOME (extension, lower_pattern) =>
		 let val unique_limit = ref limit
		     val stop = stop_listen (pattern, lower_pattern,
					     unique_limit)
		     val passive_info = (session_id, unique_limit,
					 H handler, stop, extension)
		     val synch = B.Pipe.new () : unit B.Pipe.T
		 in start_lower_listen (pattern, lower_pattern, unique_limit,
					session_id, protocol_state, synch);
		    B.Pipe.dequeue synch;
		    Passives.add (pattern, passive_info);
		    L {stop = stop, extension = extension}
		 end
	      | NONE =>
		 Trace.print_raise (X.Listen "bad pattern",
			    SOME "listen-resolve"))
	 | SOME _ =>
	    Trace.print_raise (X.Listen "already listening for pattern",
			       SOME "listen")

(*
	30.	function local_connect
*)

   fun local_connect (session_id, protocol_state) (address, H handler) =
        case resolve (protocol_state, address) of
	   SOME lower_address =>
	    let val _ = Trace.debug_constant_string
		           "local_connect calling connect_lower"
	        val (lower_key, lower_conn) =
	              connect_lower (session_id, protocol_state, lower_address,
				     exec_connection "lower")
		val key = make_key (protocol_state, address, lower_key,
				    fold_funs)
		val preliminary = Pending_Conn {session_id = session_id,
						valid = ref (init_openp ()),
						queue = B.Pipe.new (),
						lower = lower_key}
	    in case Conns.look key of
	          NONE =>	(* exec_connection eventually
				   calls disconnect_lower *)
		   (Conns.add (key, preliminary);
		    exec_connection "connect"
		                    (session_id, protocol_state, key,
				     lower_key, lower_conn, handler))
	        | SOME _ =>
		   (Trace.debug_print
		      (fn _ =>
		       "local_connect calling disconnect_lower for " ^
		       Lower.Connection_Key.makestring lower_key);
		    disconnect_lower lower_key;
(* connection may have gone away during disconnect_lower, so we check again. *)
		    case Conns.look key of
		       NONE => local_connect (session_id, protocol_state)
			                     (address, H handler)
		     | SOME _ =>
			(Trace.debug_print (fn _ =>
					    "raising already open for " ^
					    Connection_Key.makestring key);
			 raise Already_Open key))
	    end
	 | NONE =>
	    Trace.print_raise (X.Connection "bad address",
			       SOME "connect-resolve")

(*
	31.	function remove_connections
*)

   fun remove_connections id =
        let fun conn_data (key, Conn {session_id, lower, valid, ...}) =
	         (session_id, (key, valid, lower, disconnect_lower))
	      | conn_data (key, Pending_Conn {session_id, lower, valid, ...}) =
		 (session_id, (key, valid, lower, disconnect_lower))
	    fun conditional_complete (false, _) = false
	      | conditional_complete (true, arg) =
	         (local_abort arg;
		  true)
	    fun same_session (key, conn) =
	         let val (session_id, args) = conn_data (key, conn)
		 in conditional_complete (id = session_id, args)
		 end
	in Conns.remove_selected same_session
	end

(*
	32.	function remove_passives

	The stop function updates the passives and lower_listen data
	structures, so remove_passive has no explicit assignments.
*)

   fun remove_passives id =
        let fun same_session (_, (session_id, _, _, _, _)) = id = session_id
	    fun call_stop (arg as (_, value as (_, _, _, stop, _))) =
	         if same_session arg then stop () else ()
	in Passives.app call_stop
	end

(*
	33.	function call_status
*)

   fun call_status (connection_key, status) =
        case Conns.look connection_key of
	   SOME (Conn {status_handler, ...}) =>
	    status_handler status
	 | _ => ()

(*
	34.	function initialize_session

	Single-threaded initialization.
*)

   fun initialize_session (initialization, session_id) =
        let type initialized = (Lower.session * lower_session_pipe) B.Pipe.T
	    val initialized = B.Pipe.new (): initialized
	    fun wait_session synch argument =
	         (lower_session := SOME (argument, synch);
		  B.Pipe.enqueue (initialized, (argument, synch));
		  Trace.debug_constant_string "initialized lower session";
		  let val result_pipe = B.Pipe.dequeue synch
		  in Trace.debug_constant_string "finalizing lower session";
		     lower_session := NONE;
		     result_pipe
		  end)
	    fun lower_thread () =
	         let val synch = B.Pipe.new (): lower_session_pipe
		     val pipe = Lower.session (lower_setup initialization,
					       wait_session synch)
		 in Trace.debug_constant_string "lower session completed";
		    B.Pipe.enqueue (pipe, ())
		 end
	    val (lower_session_value, synch) =
	           case ! lower_session of
		      NONE =>
		       (B.Scheduler.fork lower_thread;
			B.Pipe.dequeue initialized)
		    | SOME s => s
	    val (old_count, state) =
	         case Protocol_Info.look initialization of
	            SOME result => result
	          | NONE =>
		     (0, init_proto (initialization, lower_session_value,
				     call_status))
	in Protocol_Info.add (initialization, (old_count + 1, state));
	   (state, synch)
	end

(*
	35.	function finalize_session

	Single-threaded finalization.
*)

   fun finalize_session (session_id, initialization, protocol_state, synch) =
	(Trace.debug_constant_string "removing passives and connections";
	 remove_passives session_id;
	 remove_connections session_id;
	 Trace.debug_print (fn _ => "finalizing overall protocol");
	 case Protocol_Info.look initialization of
	    NONE =>			(* some error. *)
	     Trace.print_raise (X.Session "disappeared", SOME "session")
	  | SOME (1, _) =>
	     (fin_proto protocol_state;
	      Protocol_Info.remove initialization;
	      if Protocol_Info.empty () then
	       let val pipe = B.Pipe.new (): unit B.Pipe.T
	       in Trace.debug_print (fn _ => "pipe size is " ^
				     Integer.toString (B.Pipe.size synch));
		  B.Pipe.enqueue (synch, pipe);
		  Trace.debug_print (fn _ => "waiting for session completion");
		  B.Pipe.dequeue pipe
		  before Trace.debug_print (fn _ => "session completed")
	       end
		else ())
	    | SOME (count, state) =>
	       Protocol_Info.add (initialization, (count - 1, state)))

(*
	36.	function session
*)

   fun print_state () =
        Trace.trace_print (fn _ => "state is: conns " ^
			   makestring_conns () ^
			   ", passives " ^
			   makestring_passives () ^
			   ", lower passives " ^
			   makestring_lower_passives () ^
			   ", lower connections " ^
			   makestring_lower () ^
			   ", protocol " ^
			   makestring_protocol_info () ^
			   ", lower session " ^
			   makestring_lower_session ())

  in (* local *)

   fun session (initialization, session_fun) =
        let val _ = print_state ()
	    val session_id = ref true
	    val (state, synch) = sem_with_lock (session_semaphore,
						initialize_session,
						(initialization, session_id),
						"session/init")
	    val (protocol_state, extension) = state
	    val connect = local_connect (session_id, protocol_state)
	    val listen = local_listen (session_id, protocol_state)
	    val session = S {connect = connect, listen = listen,
			     extension = extension}
	    val result = ((Result (session_fun session))
			  handle x => Exception x)
	in session_id := false;
	   sem_with_lock (session_semaphore, finalize_session,
			  (session_id, initialization, protocol_state,
			   synch), "session/fin");
	   print_state ();
	   case result of
	      Result x => x
	    | Exception x =>
	       Trace.print_raise_again (x, SOME "session handler")
	end

  end (* local *)

 end (* struct *)

