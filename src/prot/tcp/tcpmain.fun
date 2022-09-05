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

	tcpmain.fun: a functor to implement the top level of TCP.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Tcp
	2.	protocol parameters
	3.	datatype instantiation
	4.	structure Tcp_Connection
	5.	structures Tcp_Key, Tcp_Address, and Tcp_Pattern
	6.	structures Incoming and Outgoing
	7.	structure Tcp_Status
	8.	subsidiary structures
	9.	extension types
	10.	datatype connection_internals
	11.	functions for Connection functor
	12.	structure Conn
	13.	Expose shared elements of Tcp_Connection.Conn
	14.	Other elements shared with Tcp_Connection
	15.	datatype connection, handler, listen, and session
	16.	subsidiary structures
	17.	internal function get_tcb
	18.	function add_to_window
	19.	internal functions start_user_timer and stop_user_timer
	20.	internal function delete_tcb
	21.	internal data and status delivery functions
	22.	internal function execute_actions
	23.	internal function wait_handlers
	24.	internal function tcp_send_packet
	25.	internal function close_or_abort
	26.	internal function convert_connection
	27.	internal function instantiate_handlers
	28.	internal function create_tcb
	29.	internal function do_connect
	30.	internal function tcp_handler
	31.	function session

		iii.	RCS Log
	
$Log: tcpmain.fun,v $
Revision 1.88  1997/12/11  19:48:42  esb
added some printing that might help track down obscure bugs.

Revision 1.87  97/11/19  13:50:11  cline
109.32 compatibility

Revision 1.86  97/06/04  11:49:34  esb
changed to not require connection state for connection in Time_Wait,
and (temporarily) to check that this state is indeed being returned.

Revision 1.85  97/04/22  11:24:14  esb
Where possible, switched to B.Event instead of B.Event_Queue;
also adapted to new tcptcb.sig.

Revision 1.84  97/01/24  14:58:48  cline
eliminated (illegal) signature declaration within functor body.

Revision 1.83  1996/10/28  21:39:20  esb
added per-session packet counts.

Revision 1.82  1996/10/18  20:53:05  esb
adapted to new tcpsend.sig.

Revision 1.81  1996/07/24  18:35:44  cline
*** empty log message ***

Revision 1.80  1996/07/22  18:54:47  cline
reformatted

Revision 1.79  1996/07/05  17:26:11  esb
improved error reporting and tracing.

Revision 1.78  1996/06/11  03:33:15  esb
adapted to new timer interface, added a store for connections in
time_wait state, and cleaned up the handling of errors during
connection setup.

Revision 1.76  1996/05/14  01:26:25  esb
changed to support timestamp options.

Revision 1.75  1996/04/18  21:22:03  cline
updated to match new TIME signature

Revision 1.74  1996/03/12  22:25:18  esb
now sends ICMP port_unreachable, also adapted to new FOXWORD.

Revision 1.73  1996/02/23  21:33:26  esb
adapted to new network signature.

Revision 1.72  1996/02/14  20:25:47  esb
added sub-structure Transport_Setup

Revision 1.71  1996/01/19  23:03:30  esb
adapted to the new wordarray signature.

Revision 1.70  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.69  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.68  1995/10/18  14:51:24  cline
added undelivered

Revision 1.67  1995/10/18  14:46:18  cline
updated inlined connection functor

Revision 1.66  1995/10/13  15:53:20  cstone
Added Host_Id structure

Revision 1.65  1995/10/13  15:29:35  cline
updated (inlined) connection functor

Revision 1.64  1995/10/03  13:01:59  cline
handle exceptions in connect_lower's connect_thread
wait for all connections to close before exiting session

Revision 1.63  1995/09/26  16:20:35  cline
removed unused protocol_checksum functor parameter.

Revision 1.62  1995/09/25  16:48:47  cline
added tracing of execute_actions when debugging

Revision 1.61  1995/09/20  19:52:57  cline
added datatypes.  added a call to execute_actions.

Revision 1.60  1995/09/15  16:40:39  cline
work around for representation analysis bug

Revision 1.59  1995/09/14  21:11:22  cline
work around for representation bug

Revision 1.58  1995/08/08  18:31:39  cline
upgraded to new signatures

Revision 1.57  1995/03/24  01:47:09  esb
adapted to new icmp.sig.

Revision 1.56  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.55  1995/03/10  03:47:37  esb
adapted to new vendor.sig.

Revision 1.54  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.53  1995/02/04  21:46:45  robby
updated to 107

Revision 1.52  1995/01/18  21:15:23  esb
adapted to new COROUTINE signature.

Revision 1.51  1995/01/17  21:07:02  esb
adapted to new icmp.sig

Revision 1.50  1994/12/01  18:45:08  esb
renamed parameters to Event_Queue.{signal,wait}.

Revision 1.49  1994/11/11  18:11:01  esb
changed the functor parameters for Debug_Trace_Printer.

Revision 1.48  1994/11/10  20:09:03  esb
fixed a bug with equality types.

Revision 1.47  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.46  1994/11/07  21:33:24  cline
use V.Print

Revision 1.45  1994/11/01  23:14:57  esb
added, but did not test, code to "do the right thing" with IP status messages

Revision 1.44  1994/10/25  16:33:22  esb
fixed circularity problems of instantiating user handlers by adding a
data queue used only during connection establishment.

Revision 1.43  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.42  1994/09/30  17:01:25  esb
now closes lower connection from passive open if there are no other refs

Revision 1.41  1994/09/12  18:18:19  milnes
Added prints to handle _'s.

Revision 1.40  1994/08/28  21:42:31  milnes
Rearranged for icmp.

Revision 1.39  1994/08/24  22:10:48  esb
added minor optimizations and streamlining.

Revision 1.38  1994/08/24  15:09:02  esb
modified start_passive_port to take an arbitrary address.

Revision 1.37  1994/08/23  15:46:47  esb
added start_passive_port

Revision 1.36  1994/08/18  20:31:09  esb
adapted to new tcptcb.sig, with status message support.

Revision 1.35  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.34  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.33  1994/07/11  17:54:08  esb
replaced do_if_debug with debug_print, removed do_traces.

Revision 1.32  1994/07/07  02:29:07  esb
added exception handlers for Bad_Tcb outside the log functions.

Revision 1.31  1994/07/01  02:32:14  danwang
Moved control structures into Fox_Basis.

Revision 1.30  1994/06/20  20:19:21  esb
changed logging.

Revision 1.29  1994/06/17  22:04:12  esb
changed delete_tcb so it signals pending threads even if the tcb is gone.

Revision 1.28  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.27  1994/06/14  18:58:24  esb
Fixed passive_process which would loop forever on finalize.

Revision 1.26  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.25  94/05/23  14:03:27  milnes
Added print functions.

Revision 1.24  1994/05/10  08:06:30  esb
many minor changes, optimizations.  Simplified the receive path.

Revision 1.23  94/04/26  17:59:29  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.22  94/04/15  03:20:57  esb
added check in passive open for connection already open.

Revision 1.21  94/04/14  20:19:31  milnes
Extended the printing of all error messages.

Revision 1.20  1994/04/06  23:17:25  esb
adapted to new receive_packet interface.

Revision 1.19  94/03/25  16:25:59  esb
added some debugging statements.

Revision 1.18  94/03/11  04:58:55  esb
a failed send will no properly raise an exception.

Revision 1.17  1994/03/10  19:44:14  esb
changed messages.

Revision 1.16  94/03/08  21:46:49  esb
timing board bug -- stopped the wrong counter.

Revision 1.15  94/03/07  16:51:23  esb
added a counter to execute_actions to detect infinite loops.

Revision 1.14  1994/03/03  00:33:42  esb
changed a print statement.

Revision 1.13  94/02/28  14:21:23  esb
changed the positions of the timing board calls.

Revision 1.12  94/02/25  18:35:12  milnes
Updated timing and moved to safe array operations.

Revision 1.11  1994/02/21  00:08:41  esb
removed unnecessary handlers.

Revision 1.10  94/02/18  14:33:35  milnes
Extended the timing calls and made the fast path timing calls non-cumulative.

Revision 1.9  1994/02/17  01:10:22  esb
many minor changes.

Revision 1.8  94/02/14  15:30:34  esb
tested and fixed logging of events.

Revision 1.7  94/02/14  14:25:28  esb
added logging.

Revision 1.6  1994/01/30  20:56:00  esb
send now waits for the data to be acked before returning.

Revision 1.5  1994/01/28  02:10:54  esb
fixed minor bug.

Revision 1.4  1994/01/28  01:12:31  esb
fixed window update, added send_immediately, minor changes.

Revision 1.3  1994/01/19  21:33:14  esb
adapted to new interface, some bug fixes.

Revision 1.2  1994/01/11  23:08:13  esb
added user timeouts for active and passive open, send, close, and abort.

Revision 1.1  1994/01/09  03:24:53  esb
Initial revision

	1.	functor Tcp
*)

functor Tcp (structure Lower: NETWORK_PROTOCOL
	     structure B: FOX_BASIS
	     val compute_checksums: bool
	     val tcp_protocol: Lower.Protocol_Id.T
	     val user_timeout: Word32.word
	     val initial_window: int
	     val debug_level: int ref option): TCP_PROTOCOL =
 struct

  val zero16 = Word16.fromInt 0
  val zero32 = Word32.fromInt 0
  val zero64 = Word64.fromInt 0
  val one16 = Word16.fromInt 1
  val one64 = Word64.fromInt 1
  val n4ux80000000 = Word32.<< (Word32.fromInt 0x8000, 0w16)
  val n4ux3fffffff = Word32.fromInt 0x3fffffff

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpmain.fun"
			   val makestring = Lower.X.makestring)

(*
	2.	protocol parameters
*)

	      (* maximum segment lifetime: *)
 (* val msl = Word32.fromInt 120000	(* RFC 793, p. 28  *) *)
  val msl = Word32.fromInt 240000	(* RFC 1122, p. 96 *)
  val time_wait_time = Word32.<< (msl, 0w1)  (* wait 2 MSL in Time_Wait. *)

  val ack_time = Word32.fromInt 10	(* ms before sending acks *)

(*
	3.	datatype instantiation
*)

 datatype ('a, 'b) instantiation = Uninstantiated of 'a | Instantiated of 'b

(*
	4.	structure Tcp_Connection
*)

  structure Tcp_Connection =
   struct

(*
	5.	structures Tcp_Key, Tcp_Address, and Tcp_Pattern
*)

    local
      structure Ids = Transport_Ids (structure Lower = Lower)
    in
      structure Tcp_Key = Ids.Key
      structure Tcp_Address = Ids.Address
      structure Tcp_Pattern = Ids.Pattern
      structure Tcp_Host_Id = Ids.Host_Id
      type host_id = Ids.host_id
      type port = Ids.port
    end

(*
	6.	structures Incoming and Outgoing
*)

    structure Incoming = Lower.Network_Incoming
    type incoming = Incoming.T

    structure Outgoing = Lower.Outgoing
    type outgoing = Outgoing.T

(*
	7.	structure Tcp_Status
*)

    structure Tcp_Status =
     struct
       type incoming = Incoming.T
       datatype tcp_status =
	 Urgent_Data of incoming
       | Connection_Closing
       | Connection_Reset
       type T = tcp_status
       fun makestring (Urgent_Data incoming) =
	   "Urgent Data:" ^ Incoming.makestring incoming
	 | makestring Connection_Closing = "Connection_Closing"
	 | makestring Connection_Reset = "Connection_Reset"
     end

(*
	8.	subsidiary structures
*)

    structure Log = Tcp_Log (structure B = B
			     type time = Word32.word
			     val time_makestring = Int.toString o Word32.toInt
			     val zero_time = Word32.fromInt 0
			     val delta = Word32.- )

    structure Tcp_Tcb =
      Tcp_Tcb (structure B = B
	       structure Incoming = Incoming
	       structure Outgoing = Outgoing
	       structure Tcp_Log = Log)

    fun send_exception x =
	 Trace.local_print (case Lower.X.makestring x of
			       SOME x => x
			     | NONE => "Unexpected Exception")

    structure Tcp_Header =
      Tcp_Header (structure Tcp_Tcb = Tcp_Tcb
		  structure Incoming = Incoming
		  structure Outgoing = Outgoing
		  structure B = B
		  val compute_checksums = compute_checksums
		  val send_exception = send_exception
		  val debug_level = debug_level)

(*
	9.	extension types
*)

    datatype tcp_connection_extension =
	Tcp_Connection_Extension of
	  {urgent_send: Outgoing.T -> unit,

	   automatic_window_update: unit -> bool,
	   update_automatically: unit -> unit,
	   update_explicitly: unit -> unit,
	   add_to_window: int -> unit,

	   send_immediately: unit -> unit,

	   start_logging: unit -> unit,
	   get_log: unit -> (Log.time * Log.tcp_event) list,
	   start_stats: unit -> unit,
	   get_stats: unit -> Log.tcp_stats option}
    datatype tcp_session_extension =
	Tcp_Session_Extension of
	  {bytes_sent: unit -> Word64.word,
	   bytes_received: unit -> Word64.word,
	   packets_sent: unit -> Word64.word,
	   packets_received: unit -> Word64.word,
	   failed_sends: unit -> Word64.word,
	   packets_rejected: unit -> Word64.word}
    type session_extension = tcp_session_extension
    type protocol_state = {sent_count:        Word64.word ref,
			   received_count:    Word64.word ref,
			   packets_sent:      Word64.word ref,
			   packets_received:  Word64.word ref,
			   failed_send_count: Word64.word ref,
			   rejected_count:    Word64.word ref,
			   pseudo_header_sum: Outgoing.T -> Word16.word}
    type connection_state = protocol_state
    datatype transport_listen_extension =
	Listen_Extension of {local_port: port, additional: unit}
    type listen_extension = transport_listen_extension

(*
	10.	datatype connection_internals

*)

    datatype data_or_status = Data of Incoming.T | Status of Tcp_Status.T
    datatype actual_handler = Actual_Handlers of {data: Incoming.T->unit,
						  status: Tcp_Status.T->unit}
			    | Queued of data_or_status Tcp_Tcb.Q.T

    type connection_internals_record =
	{actions: Tcp_Header.action_state ref,
	 auto_add: bool ref,
	 conn_send: Outgoing.T -> unit,
	 cstate: Tcp_Tcb.tcp_state ref,
	 handlers: actual_handler ref,
	 key: Tcp_Key.T,
	 last_error: string ref,
	 log: (Log.time * Log.tcp_event) list option ref,
	 lower_extension: Lower.connection_extension,
	 pending_open: B.Event.T,
	 send_ack: (Word32.word, bool) B.Event_Queue.T,
	 stats: Log.tcp_stats option ref,
	 tcp_abort: (unit -> unit) ref,
	 tcp_send: (Outgoing.T -> unit) ref,
	 upper: (unit, bool) B.Event_Queue.T,
	 urgent_send: (Outgoing.T -> unit) ref}

    datatype connection_internals = Connection_Internals of
				      connection_internals_record

(*
	type connection_extension

	Here is the connection extension for (internal) Conn connections.
	it includes the externally visible tcp_connection_extension as
	well as connection_internals which hold much of the state of the
	connection.  This is all wrapped in a ref so Conn can create
	the connection before the connection handler instantiates the
	connection extension.
*)
    type connection_extension =
      (Lower.connection_extension,
       (tcp_connection_extension * connection_internals)) instantiation ref

(*
	11.	functions for Connection functor
*)

    fun init_proto (setup, lower_session, call_status) = 
         let val state: protocol_state = 
	         {sent_count = ref zero64 (* bytes *),
	          received_count = ref zero64 (* bytes *),
		  packets_sent = ref zero64 (* packets *),
		  packets_received = ref zero64 (* packets *),
		  failed_send_count = ref zero64,
		  rejected_count = ref zero64 (* packets *),
		  pseudo_header_sum = fn _ => zero16}
	     val extension =
	      Tcp_Session_Extension
	        {bytes_sent       = fn () => ! (#sent_count state),
	         bytes_received   = fn () => ! (#received_count state),
	         packets_sent     = fn () => ! (#packets_sent state),
	         packets_received = fn () => ! (#packets_received state),
	         failed_sends     = fn () => ! (#failed_send_count state),
	         packets_rejected = fn () => ! (#rejected_count state)}
	 in (state, extension)
	 end

    fun fin_proto _ = ()

    fun resolve (_, Tcp_Address.Complete {peer, ...}) = 
	  SOME (Lower.Network_Address.Address
		  {peer = peer, proto = tcp_protocol})
      | resolve (_, Tcp_Address.Remote_Specified {peer, ...}) = 
	  SOME (Lower.Network_Address.Address
		  {peer = peer, proto = tcp_protocol})

    (* new_port is supposed to choose an unused local port for
       a Remote_Specified address *)
    local
     val first_port = Word16.fromInt 1024	(* RFC 1700, p. 16 *)
     val last_port = Word16.fromInt 0xffff
     val current_port = ref first_port

     fun fold_same _ (_, true) = true
       | fold_same match (key, false) = match key

     fun same_addr port (Tcp_Key.Key {local_port, ...}) = port = local_port

     fun same_pattern port (_, Listen_Extension {local_port, ...}) =
	  port = local_port

     fun already_open ({conns, listens}, port) =
	  B.V.List.fold (fold_same (same_addr port)) (conns ()) false orelse
	  B.V.List.fold (fold_same (same_pattern port)) (listens ()) false

     fun find_loop (start, current, info) =
	  let val new_port = Word16.+ (current, one16)
	      val new_current =
		   if Word16.< (new_port, first_port) then first_port
		   else new_port
	  in if already_open (info, current) then
	      if start = new_current then (* VERY unlikely *)
	       Trace.print_raise (Lower.X.Connection "no free port found",
				  SOME "new_port")
	      else
	       find_loop (start, new_current, info)
	     else
	      (current_port := new_current;
	       new_current)
	  end
    in
     fun new_port info = 
	  find_loop (! current_port, ! current_port, info)
    end

    fun make_key (_, Tcp_Address.Complete addr,_,_) = Tcp_Key.Key addr
      | make_key (_, Tcp_Address.Remote_Specified {peer, remote_port},_,info)=
	  Tcp_Key.Key {peer = peer, remote_port = remote_port,
		       local_port = new_port info}

    fun map_pattern (_, Tcp_Pattern.Complete {peer, local_port, ...}, _) = 
	  SOME (Listen_Extension {local_port=local_port, additional=()},
		Lower.Network_Pattern.Complete {peer = peer,
						proto = tcp_protocol})
      | map_pattern (_, Tcp_Pattern.Remote_Specified {peer, ...}, info) = 
	  SOME (Listen_Extension {local_port=new_port info, additional=()},
		Lower.Network_Pattern.Complete {peer = peer,
						proto = tcp_protocol})
      | map_pattern (_, Tcp_Pattern.Local_Specified {local_port}, _) = 
	  SOME (Listen_Extension {local_port=local_port, additional=()},
		Lower.Network_Pattern.Partial {proto = tcp_protocol})
      | map_pattern (_, Tcp_Pattern.Unspecified, info) = 
	  SOME (Listen_Extension {local_port=new_port info, additional=()},
		Lower.Network_Pattern.Partial {proto = tcp_protocol})

    fun match (_, Tcp_Pattern.Complete a1, _, a2) = 
	 Tcp_Key.equal (Tcp_Key.Key a1, a2)
      | match (_, Tcp_Pattern.Remote_Specified {peer = p1, remote_port = r1},
	       Listen_Extension {local_port = l1, additional},
	       Tcp_Key.Key {peer = p2, remote_port = r2, local_port = l2}) = 
	 Lower.Host_Id.equal (p1, p2) andalso r1 = r2 andalso l1 = l2
      | match (_, Tcp_Pattern.Local_Specified {local_port = l1}, _,
	       Tcp_Key.Key {local_port = l2, ...}) = 
	 l1 = l2
      | match (_, Tcp_Pattern.Unspecified,
	       Listen_Extension {local_port = l1, additional},
	       Tcp_Key.Key {local_port = l2, ...}) = 
	 l1 = l2

    fun init_connection ({sent_count, received_count, packets_sent,
			  packets_received, failed_send_count,
			  rejected_count, pseudo_header_sum}, _, lower_conn)=
         let val Lower.C {extension, ...} = lower_conn
	     val Lower.Connection_Extension
		   {pseudo_header_checksum, max_packet_size, ...} = extension
	     val connection_state: connection_state =
	         {sent_count = sent_count,
	          received_count = received_count,
	          packets_sent = packets_sent,
	          packets_received = packets_received,
	          failed_send_count = failed_send_count,
	          rejected_count = rejected_count,
	          pseudo_header_sum = pseudo_header_checksum}
         in (connection_state, ref (Uninstantiated extension))
         end

    fun fin_connection state = ()

    fun send (_, {sent_count, packets_sent, ...}:protocol_state) packet =
	 (packets_sent := Word64.+ (! packets_sent, one64);
	  sent_count := Word64.+ (! sent_count, 
				  Word64.fromInt
				  (Word.toInt (Outgoing.size packet)));
	  [packet])

    fun identify (lower_key, _) packet =
	 let val address = Lower.key_to_address lower_key
	     val Lower.Network_Address.Address {peer, proto} = address
	     val {src, dest} = Tcp_Header.identify packet
	 in [Tcp_Key.Key {peer = peer, local_port = dest, remote_port = src}]
	 end

    fun receive (key, {received_count, packets_received,
		       ...}: protocol_state) packet = 
	 (packets_received := Word64.+ (! packets_received, one64);
	  received_count := Word64.+ (! received_count,
				      Word64.fromInt
				      (Word.toInt (Incoming.size packet)));
	  SOME packet)

    fun lower_status (state, key) icmp =
	  Trace.local_print ("Received ICMP message (" ^
			     Lower.Status.makestring icmp ^ ").  Continuing.")

    fun undelivered (lower_key, state) (lower_conn, data) =
	 let val Lower.C {extension, ...} = lower_conn
	     val Lower.Connection_Extension {port_unreachable, ...} = extension
	 in Trace.trace_print (fn _ => "dropping packet from " ^
			       Lower.Connection_Key.makestring lower_key ^
			       ", data (first 60 bytes) is " ^
			       Lower.Incoming.makestring_max (data, 0w60));
	    port_unreachable data
	 end

(*
	12.	structure Conn
*)

    structure Conn =
      Connection (structure Lower = Lower
		  structure Setup = Lower.Setup
		  structure Address = Tcp_Address
		  structure Pattern = Tcp_Pattern
		  structure Connection_Key = Tcp_Key
		  structure Incoming = Incoming
		  structure Outgoing = Outgoing
		  structure Status = Tcp_Status
		  structure Count = Lower.Count
		  structure X = Lower.X
		  type connection_extension = connection_extension
		  type listen_extension = listen_extension
		  type session_extension = session_extension
		  type connection_state = connection_state
		  type protocol_state = protocol_state
		  val lower_setup = (fn setup => setup)
		  val init_proto = init_proto
		  val fin_proto = fin_proto
		  val resolve = resolve
		  val make_key = make_key
		  val map_pattern = map_pattern
		  val match = match
		  val init_connection = init_connection
		  val fin_connection = fin_connection
		  val send = send
		  val identify = identify
		  val receive = receive
		  val undelivered = undelivered
		  val lower_status = lower_status
		  structure B = B
		  val module_name = "tcpmain.fun"
		  val debug_level = debug_level)

   end (* structure Tcp_Connection *)

(*
	13.	Expose shared elements of Tcp_Connection.Conn
*)

  local
   structure Conn:
    sig
     structure Setup: KEY
     structure Address: KEY
     structure Pattern: KEY
     structure Connection_Key: KEY
     structure Incoming: EXTERNAL
     structure Outgoing: EXTERNAL
     structure Status: PRINTABLE
     structure Count: COUNT
     structure X: PROTOCOL_EXCEPTIONS
     exception Already_Open of Connection_Key.T
     type listen_extension
     type session_extension
     datatype listen = L of {stop: unit -> unit, extension: listen_extension}
    end
   = Tcp_Connection.Conn
  in
    open Conn
  end

(*
	14.	Other elements shared with Tcp_Connection
*)

  structure Conn = Tcp_Connection.Conn

  structure Transport_Setup = Lower.Network_Setup
  structure Transport_Address = Tcp_Connection.Tcp_Address
  structure Transport_Pattern = Tcp_Connection.Tcp_Pattern
  structure Transport_Key     = Tcp_Connection.Tcp_Key
  structure Tcp_Status        = Tcp_Connection.Tcp_Status

  structure Log        = Tcp_Connection.Log
  structure Tcp_Tcb    = Tcp_Connection.Tcp_Tcb
  structure Tcp_Header = Tcp_Connection.Tcp_Header

  structure Host_Id = Tcp_Connection.Tcp_Host_Id
  type host_id = Tcp_Connection.host_id
  type port    = Tcp_Connection.port

  local
    (* this is necessary to expose the constructor Listen_Extension *)
    structure Export_Listen_Extension:
      sig
	datatype transport_listen_extension =
	  Listen_Extension of {local_port: port, additional: unit}
      end
    = Tcp_Connection
  in
    open Export_Listen_Extension
  end
  type additional_listen_extension = unit
(*  type transport_listen_extension = listen_extension*)

  (* expose datatype Connection_Internals *)
  local
    structure Internals:
     sig datatype data_or_status = Data of Incoming.T | Status of Tcp_Status.T
	 datatype actual_handler =
	   Actual_Handlers of {data: Incoming.T->unit,
			       status: Tcp_Status.T->unit}
	 | Queued of data_or_status Tcp_Tcb.Q.T
	 datatype connection_internals =
	   Connection_Internals of Tcp_Connection.connection_internals_record
	 datatype tcp_connection_extension =
	   Tcp_Connection_Extension of
	   {urgent_send: Outgoing.T -> unit,
	    automatic_window_update: unit -> bool,
	    update_automatically: unit -> unit,
	    update_explicitly: unit -> unit,
	    add_to_window: int -> unit,
	    send_immediately: unit -> unit,
	    start_logging: unit -> unit,
	    get_log: unit -> (Log.time * Log.tcp_event) list,
	    start_stats: unit -> unit,
	    get_stats: unit -> Log.tcp_stats option}
	 datatype tcp_session_extension =
	   Tcp_Session_Extension of
	   {bytes_sent: unit -> Word64.word,
	    bytes_received: unit -> Word64.word,
	    packets_sent: unit -> Word64.word,
	    packets_received: unit -> Word64.word,
	    failed_sends: unit -> Word64.word,
	    packets_rejected: unit -> Word64.word}

     end = Tcp_Connection
  in
    open Internals
  end

  type connection_extension = tcp_connection_extension


(*
	15.	datatype connection, handler, listen, and session
*)

  datatype connection = C of {send: Outgoing.T -> unit,
			      abort: unit -> unit,
			      extension: connection_extension}
  datatype handler = H of Connection_Key.T
		  -> {connection_handler: connection -> unit,
		      data_handler: connection * Incoming.T -> unit,
		      status_handler: connection * Status.T -> unit}
  datatype session = S of {connect: Address.T * handler -> unit,
			   listen: Pattern.T * handler * Count.T -> listen,
			   extension: session_extension}

(*
	16.	subsidiary structures
*)

  structure Tcp_Retransmit = Tcp_Retransmit (structure Tcp_Tcb = Tcp_Tcb
					     structure B = B)

  structure Tcp_State = Tcp_State (structure Tcp_Tcb = Tcp_Tcb
				   structure Retransmit = Tcp_Retransmit
				   structure Incoming = Incoming
				   structure Outgoing = Outgoing
				   structure B = B
				   val ack_time = ack_time
				   val debug_level = debug_level)

  structure Tcp_Receive = Tcp_Receive (structure Tcp_Tcb = Tcp_Tcb
				       structure Incoming = Incoming
				       structure Outgoing = Outgoing
				       structure Retransmit = Tcp_Retransmit
				       val ack_time = ack_time
				       val time_wait_time = time_wait_time
				       structure B = B
				       val debug_level = debug_level)

  structure Tcp_Send = Tcp_Send (structure Tcp_Tcb = Tcp_Tcb
				 structure Outgoing = Outgoing
				 structure Retransmit = Tcp_Retransmit
				 structure B = B
				 val debug_level = debug_level)

(*
	17.	internal function get_tcb

	note: it is expected that the caller will handle Bad_Get_Tcb
	and/or print any relevant error messages.  Common cases are
	handled first.
*)

  exception Bad_Get_Tcb

  fun get_tcb (Tcp_Tcb.Estab tcb) = tcb
    | get_tcb (Tcp_Tcb.Close_Wait tcb) = tcb
    | get_tcb (Tcp_Tcb.Listen (tcb, _)) = tcb
    | get_tcb (Tcp_Tcb.Syn_Sent (tcb, _)) = tcb
    | get_tcb (Tcp_Tcb.Syn_Active tcb) = tcb
    | get_tcb (Tcp_Tcb.Syn_Passive (tcb, _)) = tcb
    | get_tcb (Tcp_Tcb.Fin_Wait_1 (tcb, _)) = tcb
    | get_tcb (Tcp_Tcb.Fin_Wait_2 tcb) = tcb
    | get_tcb (Tcp_Tcb.Closing tcb) = tcb
    | get_tcb (Tcp_Tcb.Last_Ack tcb) = tcb
    | get_tcb (Tcp_Tcb.Closed to_do) = raise Bad_Get_Tcb
    | get_tcb (Tcp_Tcb.Time_Wait tcb) = raise Bad_Get_Tcb

(*
	18.	function add_to_window
*)

  local
   fun subtype (tcb as (Tcp_Tcb.Tcb {rcv_wnd, rcv_sws, unacked_segs,
				     mss, to_do, timers, ...})) =
	(rcv_wnd, rcv_sws, unacked_segs, mss, to_do, timers)

   val expire = Tcp_Tcb.Timer_Expiration Tcp_Tcb.Ack_Timer

   val b4_initial = Word32.fromInt (initial_window div 2)

   fun print_window (bytes, rcv_wnd, rcv_sws) =
	Trace.local_print ("added " ^ Integer.toString bytes ^
			   " to TCP window, result is " ^
			   Word32.fmt StringCvt.DEC (! rcv_wnd) ^
			   " + " ^ Word32.fmt StringCvt.DEC (! rcv_sws))

  in (* local *)
   fun add_to_window (Connection_Internals {cstate, ...}, bytes) =
	let val (rcv_wnd, rcv_sws, unacked_segs, mss, to_do, timers) =
	       case ! cstate of
		  Tcp_Tcb.Closed (to_do, timers) =>
		   (ref zero32, ref zero32, ref 0, ref zero16, to_do, timers)
		| x => subtype (get_tcb x)
			handle e =>
			  Trace.print_raise_again (e, SOME "add_to_window")
	    val delta = Word32.fromInt bytes
	    val b4_mss = Word32.fromInt (Word16.toInt (! mss))
	    val limit = Word32.min (b4_mss, b4_initial)
	in (* receiver Silly-Window-Syndrome avoidance: only increment the
	      window when we have at least one MSS or 1/2 the initial window
	      worth to add. RFC 1122, p. 97 (4.2.3.3). *)
	   rcv_sws := Word32.+ (delta, ! rcv_sws);
	   if Word32.>= (! rcv_sws, limit) then
	    (rcv_wnd := Word32.+ (! rcv_wnd, ! rcv_sws);
	     rcv_sws := zero32;
	 (* the window is updated, so send an ack to inform the peer.
	    If unacked_segs is zero, we send the ack by starting the
	    ack timer; we also set unacked_segs to 1, to let tcpstate.fun
	    know that the ack really needs to be sent.
	    If unacked_segs is one and the window is less than 4 times
	    the MSS, we place a timer expiration on the to_do list to
	    send the ack ASAP.
	    If unacked_segs is > 1, we place a timer expiration on the
	    to_do list to send the ack ASAP. *)
	    case ! unacked_segs of
	       0 =>
		((#start_ack timers) ack_time;
		 unacked_segs := 1)
	     | 1 => ()
	     | _ =>
		(to_do := Tcp_Tcb.Q.add (! to_do, expire);
		 (#stop_ack timers) ()))
	   else () (* ; *)
	end
  end (* local *)

(*
	19.	internal functions start_user_timer and stop_user_timer
*)

  fun start_user_timer cstate =
       ((#start_user (#timers (Tcp_Tcb.state_basics (! cstate))) user_timeout))
       handle x => Trace.print_raise_again (x, SOME "start_user_timer")

  fun stop_user_timer cstate =
       ((#stop_user (#timers (Tcp_Tcb.state_basics (! cstate))) ()))
       handle Tcp_Tcb.Time_Wait_State_Does_Not_Have_Basics => ()

(*
	20.	internal function delete_tcb

	Remove the TCB from the protocol state, including the connections
	table, and any lower connection information.
	Also clear all the timers associated with this connection and
	restart any pending processes. Do not try to execute all the
	pending actions, since this may be called as part of executing
	an action on the to_do list.
*)

  structure Wait_Store = Single_Store (structure V = B.V
				       type key = Tcp_Connection.Tcp_Key.T
				       type value = unit
				       val hash = Tcp_Connection.Tcp_Key.hash
				       val eq = Tcp_Connection.Tcp_Key.equal)

  local
  (* signal_all is called to signal all the threads that are
     waiting on the event. *)
   fun signal_all (queue, value) =
	case B.Event_Queue.signal {queue = queue, match = fn _ => true,
				   value = value} of
	   NONE => ()
	 | _ => signal_all (queue, value)

   fun signal_all_event queue =
	if B.Event.signal queue then signal_all_event queue else ()

  in (* local *)
   fun delete_tcb (cstate, key, pending_open, upper, send_ack) =
        (case cstate of
	    Tcp_Tcb.Time_Wait {stop_timers, ...} =>
	     stop_timers ()
	  | _ =>
	     let val {timers, ...} = Tcp_Tcb.state_basics cstate
	         val {stop_resend, stop_user, stop_ack, stop_window,
		      stop_time_wait, ...} = timers
	     in stop_resend ();
	        stop_user ();
	        stop_ack ();
	        stop_window ();
	        stop_time_wait ()
	     end;
	 signal_all_event pending_open;
(* we signal "false" because the connection should have been closed by now. *)
	 signal_all (upper, false);
	 signal_all (send_ack, false);
	 Wait_Store.remove key)
	handle x => Trace.print_raise_again (x, SOME "delete_tcb")
  end (* local *)

(*
	21.	internal data and status delivery functions
*)

  fun receive_exception (x, action, Connection_Internals {cstate, ...}) =
       Trace.print_handled (x, SOME ("handler (user code?) for " ^
				     action ^ " (TCP state = " ^
				     Tcp_Tcb.state_string (! cstate) ^ ")"))

  fun deliver_data (data_handler, segment, internals) =
       let val Connection_Internals {auto_add, ...} = internals
       in (data_handler segment)
	   handle x => receive_exception (x, "data", internals);
	  if ! auto_add then
	   add_to_window (internals, Word.toInt (Incoming.size segment))
	 else ()
       end

  fun deliver_urgent (status_handler, segment, internals) =
       let val Connection_Internals {auto_add, ...} = internals
       in status_handler (Tcp_Status.Urgent_Data segment)
	    handle x => receive_exception (x, "urgent", internals);
	  if ! auto_add then
	   add_to_window (internals, Word.toInt (Incoming.size segment))
	  else ()
       end

  fun deliver_status (status_handler, status_value, name, internals) =
       status_handler status_value
	 handle x => receive_exception (x, name, internals)

(*
	22.	internal function execute_actions

	If there are no actions queued, returns. Otherwise removes an
	action from the queue of actions and executes it. Note that
	the queue is updated before the action is executed, just in
	case the action fails to terminate.

	If the connection is waiting for an open to complete (as
	determined by the size of the pending_open queue being
	greater than zero), queue up on the pending_open queue;
	when awakened, wake up the next thread on the pending_open
	queue and start executing again.

	If completing open or close, signal the upper queue. If completing
	open, also queue up on the pending_open queue.
*)

  local

   fun no_user_timeout (_, false) = false
     | no_user_timeout (Tcp_Tcb.Timer_Expiration Tcp_Tcb.User_Timer,
			true) = false
     | no_user_timeout (_, true) = true

   (* signal_all is called to signal all the threads that are
      waiting on the given event. *)
   fun signal_all queue =
	if B.Event.signal queue then signal_all queue else ()

   (* signal_upper signals all the threads that are
      waiting on the upper event. *)
   fun signal_upper (queue, value) () =
	case B.Event_Queue.signal {queue = queue, match = fn _ => true,
				   value = value} of
	   NONE => ()
	 | _ => signal_upper (queue, value) ()

   (* signal_send is called to signal all the threads that are
      waiting on the send event. match_ack matches all the
      sends whose ack is less than or equal to the actual ack. *)
   fun match_ack actual registered =
	Word32.< (Word32.- (actual, registered), n4ux80000000)

   fun signal_send (send, ack) =
	case B.Event_Queue.signal {queue = send, match = match_ack ack,
				   value = true} of
	   NONE => ()
	 | _ => signal_send (send, ack)

   (* only send actions and data actions produce a log here: there are many
      different places that send_segment packets are produced
      (in receive, state, and send), and it seems easiest to
      log them in this centralized place. Other actions are
      logged where they are produced. *)
   fun log_event (log, list, event) =
	log := SOME ((Tcp_Tcb.current_time (), event) :: list)

   fun offset_int (n, offset) =
	Word32.toInt (Word32.andb (n4ux3fffffff, Word32.- (n, offset)))

   fun log_segment (segment, cstate, log, list) =
	 let val (seq, ack, len, wnd) =
		   case segment of
		      Tcp_Tcb.Fast_Out {seq, ack, len, wnd, ...} =>
		       (seq, ack, len, wnd)
		    | Tcp_Tcb.Fast_Empty {seq, ack, wnd} =>
		       (seq, ack, zero32, wnd)
		    | Tcp_Tcb.Timestamp_Out {seq, ack, len, wnd, ...} =>
		       (seq, ack, len, wnd)
		    | Tcp_Tcb.Timestamp_Empty {seq, ack, wnd, ...} =>
		       (seq, ack, zero32, wnd)
		    | Tcp_Tcb.Out_Seg {seg, ...} =>
		       let val Tcp_Tcb.Seg {seq, ack, len, wnd, ...} = seg
		       in (seq, ack, len, wnd)
		       end
	     val Tcp_Tcb.Tcb {iss, irs, ...} = get_tcb (! cstate)
	     val w = offset_int (wnd, zero32)
	     val s = Word32.- (seq, iss)
	     val a = Word32.- (ack, ! irs)
	     val event = if Word32.> (len, zero32) then
			  Log.Packet_Sent {size = Word32.toInt len,
					   window = w, seq = s, ack = a}
			 else Log.Empty_Sent {window = w, seq = s, ack = a}
	 in log_event (log, list, event)
	 end handle Bad_Get_Tcb => ()

   fun log_data (Tcp_Tcb.Fast_In {seq, ack, len, wnd, data, times},
		 cstate, log, list) =
	 (let val Tcp_Tcb.Tcb {iss, irs, ...} = get_tcb (! cstate)
	      val w = offset_int (wnd, zero32)
	      val s = Word32.- (seq, ! irs)
	      val a = Word32.- (ack, iss)
	      val event = if Word32.> (len, zero32) then
			   Log.Packet_Received {size = Word32.toInt len,
						window = w, seq = s, ack = a}
			  else
			   Log.Empty_Received {window = w, seq = s, ack = a}
	  in log_event (log, list, event)
	  end handle Bad_Get_Tcb => ())
     | log_data (Tcp_Tcb.In_Seg {seg, data}, cstate, log, list) =
	 (let val Tcp_Tcb.Seg {len, wnd, seq, ack, ...} = seg
	      val Tcp_Tcb.Tcb {iss, irs, ...} = get_tcb (! cstate)
	      val w = offset_int (wnd, zero32)
	      val s = Word32.- (seq, ! irs)
	      val a = Word32.- (ack, iss)
	      val event = if Word32.> (len, zero32) then
			   Log.Packet_Received {size = Word32.toInt len,
						window = w, seq = s, ack = a}
			  else
			   Log.Empty_Received {window = w, seq = s, ack = a}
	  in log_event (log, list, event)
	  end handle Bad_Get_Tcb => ())

   fun wait_for_open (queue, key) =
	(Trace.debug_print (fn _ => "execute_actions " ^
			    Connection_Key.makestring key ^
			    " waiting for open");
	 B.Event.wait (queue, fn _ => ());
	 Trace.debug_print (fn _ => "execute_actions " ^
			    Connection_Key.makestring key ^
			    " restarted after open");
	 signal_all queue) (* in case this thread is woken up first *)

   fun wait_for_open_abort (pending_open, upper_queue) =
	(* user timeout; terminate upper level thread, then continue *)
	(Trace.debug_print (fn _ =>
			    "execute_actions waiting for open or abort");
	 B.Event.wait (pending_open, signal_upper (upper_queue, false));
  (* in case this thread is woken up first, wake up everyone else also. *)
	 signal_all pending_open)

   fun data_handler (connection, segment) =
	let val Connection_Internals {handlers, ...} = connection
	in case ! handlers of
	      Actual_Handlers {data, ...} =>
	       deliver_data (data, segment, connection)
	    | Queued queue =>
	       handlers := Queued (Tcp_Tcb.Q.add (queue, Data segment))
	end

   fun urgent_handler (connection, segment) =
	let val Connection_Internals {handlers, ...} = connection
	in case ! handlers of
	      Actual_Handlers {status, ...} =>
	       deliver_urgent (status, segment, connection)
	    | Queued queue =>
	       handlers := Queued (Tcp_Tcb.Q.add
				    (queue,
				     Status (Tcp_Status.Urgent_Data segment)))
	end

   fun status_handler (connection, status_value, name) =
	let val Connection_Internals {handlers, ...} = connection
	in case ! handlers of
	      Actual_Handlers {status, ...} =>
	       deliver_status (status, status_value, name, connection)
	    | Queued queue =>
	       handlers := Queued (Tcp_Tcb.Q.add (queue, Status status_value))
	end

  in (* local *)
   fun execute_actions (connection as Connection_Internals internals) =
	let val {cstate, key, log, conn_send, ...} = internals
	    val to_do = Tcp_Tcb.state_to_do (! cstate)
	    val Transport_Key.Key {local_port, remote_port, ...} = key
	    (* execute the next action on the to-do list, AFTER updating
	       the to-do list. *)
	    fun loop () =
		 if B.Event.empty (#pending_open internals) then
		  (case Tcp_Tcb.Q.next (! to_do) of
		      NONE => ()	(* done for now *)
		    | SOME (rest, action) =>
		       (to_do := rest;
			Trace.debug_print (fn _ => "                  " ^
					   Tcp_Tcb.action_string action);
			case action of
			   Tcp_Tcb.User_Data segment =>
			    data_handler (connection, segment)
			 | Tcp_Tcb.Urgent_Data segment =>
			    urgent_handler (connection, segment)
			 | Tcp_Tcb.Process_Data tcp_in =>
			    (case ! log of
				NONE => ()
			      | SOME list =>
				 log_data (tcp_in, cstate, log, list);
			     cstate := Tcp_Receive.receive (! cstate, tcp_in))
			 | Tcp_Tcb.Send_Segment tcp_out =>
			    (case ! log of
				NONE => ()
			      | SOME list =>
				 log_segment (tcp_out, cstate, log, list);
		             Tcp_Header.send_segment
			      (! (#actions internals), tcp_out, conn_send))
			 | Tcp_Tcb.Complete_Send ack =>
			    signal_send (#send_ack internals, ack)
			 | Tcp_Tcb.Probe_Window =>
			    cstate := Tcp_Send.send_queued (! cstate)
			 | Tcp_Tcb.Peer_Close =>
			    status_handler (connection,
					    Tcp_Status.Connection_Closing,
					    "remote close")
			 | Tcp_Tcb.Peer_Reset =>
			    status_handler (connection,
					    Tcp_Status.Connection_Reset,
					    "reset")
			 | Tcp_Tcb.Log_Event event =>
			    (case ! log of
				NONE => ()
			      | SOME list => log_event (log, list, event))
			 | Tcp_Tcb.Timer_Expiration timer =>
			    cstate := Tcp_State.timeout (! cstate, timer)
			 | Tcp_Tcb.Close_After_Sends =>
			    cstate := Tcp_State.close (! cstate)
			 | Tcp_Tcb.Complete_Open success =>
      (* this is the thread whose wait blocks this connection until
	 the open completes. When it completes, it awakens all
	 the other waiting threads. *)
			    let val {pending_open, upper, ...} = internals
			    in Trace.debug_print
				 (fn _ => "completing open for " ^
					   Connection_Key.makestring key);
			       B.Event.wait (pending_open,
					     signal_upper (upper, success));
			       signal_all pending_open
			    end
			 | Tcp_Tcb.Complete_Close success =>
			    signal_upper (#upper internals, success) ()
			 | Tcp_Tcb.Delete_Tcb =>
			    let val {pending_open, upper,
				     send_ack, ...} = internals
			    in delete_tcb (! cstate, key, pending_open,
					   upper, send_ack)
			    end
			 | Tcp_Tcb.User_Error s =>
			    (Trace.local_print ("error on connection " ^
						Connection_Key.makestring key^
						", " ^ s);
			     (#last_error internals) := s);
(*
			 | _ =>
			    local_print ("error, action '" ^
					 Tcp_Tcb.action_string action ^
					 " failed");
*)
		     loop ()))	(* loop and do the rest of the to_do list *)
		 else if Tcp_Tcb.Q.empty (! to_do) then () (* done for now. *)
		 else if Tcp_Tcb.Q.fold no_user_timeout (! to_do) true then
		  (wait_for_open (#pending_open internals, key);
		   loop ())
		 else 
	(* user timeout; terminate upper level thread, then continue *)
		  let val {pending_open, upper, ...} = internals
		  in wait_for_open_abort (pending_open, upper);
		     loop ()
		  end
       in Trace.debug_print (fn _ => "execute_actions: executing ...");
	  loop ()
       end (* let *)

  end (* local *)

(*
	23.	internal function wait_handlers

	Process the packet(s) that caused us to wake up again, then
	close the lower connection again.
*)

  fun wait_handlers (key) =
       let val pipe = B.Pipe.new (): unit B.Pipe.T
	   val disabled = ref false
	   fun conn _ = B.Pipe.dequeue pipe
	   fun data (conn as Conn.C {extension, ...}, packet) =
	        case ! extension of
		   Instantiated (_, connection) =>
		    let val Connection_Internals {actions, ...} = connection
		    in Trace.debug_constant_string "wait data handler";
		       Tcp_Header.process_packet (! actions, packet);
		       execute_actions connection;
		       B.Pipe.enqueue (pipe, ())
		    end
	         | _ =>
		    (B.Pipe.enqueue (pipe, ());
		     Trace.local_print ("removing from wait_store " ^
					Transport_Key.makestring key ^
					", uninstantiated");
		     Wait_Store.remove key;
		     if ! disabled then
		      Trace.local_print (" (already disabled)")
		     else 
		      disabled := true;
		     Trace.local_print ("discarding packet " ^
					Lower.Incoming.makestring packet);
		     ())
	   fun status _ = ()
       in {connection_handler = conn, data_handler = data,
	   status_handler = status}
       end

(*
	24.	internal function tcp_send_packet
*)

  fun tcp_send_packet (internals, packet, urgent) =
       let val Connection_Internals {cstate, upper, send_ack, conn_send,
				     actions, ...} = internals
	   val tcb as (Tcp_Tcb.Tcb {snd_nxt, ...}) = get_tcb (! cstate)
	   val data_size = Word32.fromInt (Word.toInt (Outgoing.size packet))
	   val expected_ack = Word32.+ (! snd_nxt, data_size)
	   fun execute () = execute_actions internals
	   fun update_header_and_send segment =
		Tcp_Header.send_segment (! actions, segment, conn_send)
       in while Tcp_Send.send_state (! cstate) <> Tcp_Send.Valid_Send do
	   case Tcp_Send.send_state (! cstate) of
	      Tcp_Send.Opening_Send =>
	       (Trace.debug_print (fn _ =>
				   "queueing up for allocate in state " ^
				   Tcp_Tcb.state_string (! cstate));
		B.Event_Queue.wait {queue = upper, event = (),
				    while_waiting =
				    fn () => execute_actions internals};
		Trace.debug_constant_string "allocate wakeup")
	    | Tcp_Send.Closing_Send =>
	       Trace.print_raise (X.Session "connection closing",
				  SOME "allocate failed: connection closing")
	    | Tcp_Send.Valid_Send => (); (* end while *)
      (* The connection is now in a Valid_Send state *)
	  start_user_timer cstate;
	  cstate := Tcp_Send.send_packet (! cstate, packet, urgent);
	  if B.Event_Queue.wait {queue = send_ack, event = expected_ack,
				 while_waiting = execute} then
	   stop_user_timer cstate
	  else
	   (stop_user_timer cstate;
	    Trace.print_raise (X.Send "failed", SOME "send"))
       end

(*
	25.	internal function close_or_abort
*)

  fun close_or_abort (internals, action, name) =
       let val Connection_Internals {key, cstate, actions, upper,
				     last_error, ...} = internals
	   fun close_state () =
		(cstate := action (! cstate);
		 start_user_timer cstate;
		 execute_actions internals)
	   fun is_closed (Tcp_Tcb.Closed _) = true
	     | is_closed (Tcp_Tcb.Time_Wait _) = true
	     | is_closed _ = false
       in Trace.debug_print (fn _ => name ^ " request for connection " ^
			     Transport_Key.makestring key);
	  if is_closed (! cstate) then
	   () (* connection has already been aborted *)
	  else if B.Event_Queue.wait {queue = upper, event = (),
				      while_waiting = close_state} then
	   (Trace.debug_print (fn _ =>
			       "connection " ^ Transport_Key.makestring key ^
			       " closed");
	    stop_user_timer cstate)
	  else
	   (Trace.local_print ("error (" ^ (! last_error) ^
			       ") closing connection " ^
			       Transport_Key.makestring key);
	    stop_user_timer cstate;
	    Trace.print_raise (X.Session ("already closed, " ^ ! last_error),
			       SOME "close_or_abort"))
       end

(*
	26.	internal function convert_connection

	convert a higher connection to a lower connection.
*)

  fun convert_connection (Conn.C {send, abort,
				  extension as
				  ref (Instantiated (tcp_extension,
						     internals))}) =
       let val Connection_Internals {key, cstate, upper, last_error,
				     tcp_send = tcp_send_ref,
				     urgent_send = urgent_send_ref,
				     tcp_abort = tcp_abort_ref, ...} =
		 internals
	   fun tcp_send packet = tcp_send_packet (internals, packet, false)
	   fun urgent_send packet = tcp_send_packet (internals, packet, true)
	   fun tcp_abort () =
		close_or_abort (internals, Tcp_State.abort, "abort")
       in tcp_send_ref := tcp_send;
	  urgent_send_ref := urgent_send;
	  tcp_abort_ref := tcp_abort;
	  C {send = tcp_send, abort = tcp_abort, extension = tcp_extension}
       end
    | convert_connection _ =
       Trace.print_raise (X.Session "impossible (convert_connection)",
			  SOME "convert_connection/tcp_handler")

(*
	27.	internal function instantiate_handlers

	A function to instantiate the handlers and deliver any queued
	data or status.
*)

  fun instantiate_handlers (connection as Conn.C {extension, ...},
			    tcp_data_handler, tcp_status_handler) =
       let val (tcp_extension, internals) =
		 case ! extension of
		    Instantiated x => x
		  | Uninstantiated_ =>
		     Trace.print_raise (X.Session "uninstantiated",
					SOME "instantiate_handlers")
	   val tcp_connection = convert_connection connection
	   fun data x = tcp_data_handler (tcp_connection, x)
	   fun status x = tcp_status_handler (tcp_connection, x)
	   val Connection_Internals {handlers, ...} = internals
	   fun deliver (Data segment) =
		deliver_data (data, segment, internals)
	     | deliver (Status (Tcp_Status.Urgent_Data segment)) =
		deliver_urgent (status, segment, internals)
	     | deliver (Status Tcp_Status.Connection_Closing) =
		deliver_status (status, Tcp_Status.Connection_Closing,
				"remote close", internals)
	     | deliver (Status Tcp_Status.Connection_Reset) =
		deliver_status (status, Tcp_Status.Connection_Reset, "reset",
				internals)
	   fun queue_loop NONE =
		handlers := Actual_Handlers {data = data, status = status}
	     | queue_loop (SOME (new_queue, first)) =
		(handlers := Queued new_queue;
		 deliver first;
		 case ! handlers of
		    Actual_Handlers _ => ()
		  | Queued queue => queue_loop (Tcp_Tcb.Q.next queue))
       in case ! handlers of
	     Actual_Handlers _ => ()
	   | Queued queue => queue_loop (Tcp_Tcb.Q.next queue)
       end

(*
	28.	internal function create_tcb

	Create a connection in state closed, and add it to the global state.
	Call the specified "open" function on this state and
	instantiate the handlers appropriately.

	handler is the higher-level protocol's handler for this connection.
	receive is the function to be used as a handler for the
	lower connection.

	There are two main points we reach in this code:
	the first is when we create the connection, the second
	is when we call Conn.create.  We want to be careful to
	do nothing after calling Conn.create, since Conn.create
	instantiates the handlers, and if the state is inconsistent
	at that point, someone will notice.  Note also that until
	we call Conn.create, we have not modified the internal state
	in any visible way (except for creating the lower connection),
	so recovering is easy.
*)

  fun create_tcb (open_fun, key, conn_send, thread,
		  Lower.Connection_Extension lower_extension) =
    let val Transport_Key.Key {peer, local_port, remote_port} = key
      (* compute the local IP and the values that depend on it. *)
	exception Tcp_Ip_Interface_Error
	val action_address =
	    Tcp_Header.Address {local_port = local_port,
				remote_port = remote_port}
	    (* RFC 1122, p.85 says the tcp header size is 20 bytes, plus
	       any space for the options.  We normally try to send the
	       12-byte time option, so we use 32 bytes as a maximum
	       TCP header size. *)
	val tcp_header_size_max = 0w32
	val mtu = Word.toInt ((#max_packet_size lower_extension) -
			      tcp_header_size_max)
	(* generate the initial values for the connection. *)
	val to_do = ref (Tcp_Tcb.Q.new (): Tcp_Tcb.tcp_action Tcp_Tcb.Q.T)
	fun null_action _ =
	     (Trace.local_print "implementation error, null action called";
	      raise (X.Session "Tcp_Null_Action"))
	val execute_ref = ref (null_action: unit -> unit)
	val initial_actions =
	     Tcp_Header.new {act = null_action,
			     to_do = to_do,
			     address = action_address,
			     peer_checksum =
			       #pseudo_header_checksum lower_extension}
	val actions_ref = ref initial_actions
	val dummy_state = Tcp_Tcb.Time_Wait
	                     {to_do = to_do,
			      restart_time_wait_timer = fn _ => (),
			      stop_timers = fn _ => (),
			      snd_nxt = Word32.fromInt 0,
			      rcv_nxt = Word32.fromInt 0}
	val state_ref = ref dummy_state
	val pending_open = B.Event.new ()
	val upper = B.Event_Queue.new (): (unit, bool) B.Event_Queue.T
	val send = B.Event_Queue.new (): (Word32.word, bool) B.Event_Queue.T
	fun timer_exec action () =
	     (to_do := Tcp_Tcb.Q.add (! to_do, action);
	      (! execute_ref) ())
	val resend_exec = timer_exec (Tcp_Tcb.Timer_Expiration
				      Tcp_Tcb.Resend_Timer)
	val user_exec = timer_exec (Tcp_Tcb.Timer_Expiration
				    Tcp_Tcb.User_Timer)
	val ack_exec = timer_exec (Tcp_Tcb.Timer_Expiration
				   Tcp_Tcb.Ack_Timer)
	val window_exec = timer_exec (Tcp_Tcb.Timer_Expiration
				      Tcp_Tcb.Window_Timer)
(* time_wait_exec should have no link to the internal state, so the GC can
   collect the internal state while the connection is in state time_wait. *)
	val pointer_freed = ref (fn () => "not known")
	fun time_wait_exec () =
	     (case (! pointer_freed) () of
	         "freed" => ()
	       | s => Trace.local_print ("time-wait expiration, pointer is " ^
					 s);
	      delete_tcb (! state_ref, key, pending_open, upper, send))
	val timers = Tcp_Tcb.new_timer_set
			(thread,
			 {resend_expiration = resend_exec,
			  user_expiration = user_exec,
			  ack_expiration = ack_exec,
			  window_expiration = window_exec,
			  time_wait_expiration = time_wait_exec})
	val closed_state = Tcp_Tcb.Closed (to_do, timers)
	val _ = Trace.debug_print (fn _ => "calling open_fun")
	val open_state = open_fun (closed_state, initial_window, mtu)
	val _ = Trace.debug_print (fn _ => "open_fun completed")
	val _ = state_ref := open_state
	(* generate the connection, then update the refs that are circular
	   and depend on it. *)
	val handlers = Queued (Tcp_Tcb.Q.new ())
	fun tcp_send _ =
	     Trace.print_raise (X.Session "uninitialized", SOME "tcp_send")
	fun urgent_send _ =
	     Trace.print_raise (X.Session "uninitialized", SOME "urgent_send")
	fun tcp_abort _ =
	     Trace.print_raise (X.Session "uninitialized", SOME "tcp_abort")
	val _ = Trace.debug_print (fn _ => "defining connection_internals")
	val connection_internals =
	     Connection_Internals {key = key,
				   cstate = state_ref,
				   actions = actions_ref,
				   pending_open = pending_open,
				   upper = upper,
				   send_ack = send,
				   auto_add = ref true,
				   last_error = ref "",
				   handlers = ref handlers,
				   conn_send = conn_send,
				   tcp_send = ref tcp_send,
				   urgent_send = ref urgent_send,
				   tcp_abort = ref tcp_abort,
				   log = ref NONE, stats = ref NONE,
				   lower_extension = Lower.Connection_Extension
						      lower_extension}
	val _ = Trace.debug_print (fn _ => "defining execute")
	fun execute () = execute_actions connection_internals
	val _ = Trace.debug_print (fn _ => "defining real_actions")
	val real_actions =
	     Tcp_Header.new
	     {act = execute, to_do = to_do, address = action_address,
	      peer_checksum = #pseudo_header_checksum lower_extension}
	val weak_pointer = SMLofNJ.Weak.weak' connection_internals
	fun is_freed () =
	     if SMLofNJ.Weak.strong' weak_pointer then "not freed" else "freed"
    in pointer_freed := is_freed;
       actions_ref := real_actions;
       execute_ref := execute;
       Trace.debug_print (fn _ => "returning from create_tcb");
       connection_internals
    end (* let *)

(*
	29.	internal function do_connect

	Given a Tcp_Connection.Conn connection:

	Establish the TCP level connection.
*)

  fun do_connect (key, (conn as Conn.C {send, abort, extension}),
		  open_fun, thread) =
       let val lower_extension =
	     case ! extension of
		 Uninstantiated lower_extension => lower_extension
	      | _ =>
		 Trace.print_raise (X.Session "unexpected in do_connect",
				    SOME "do_connect")
	   val internals = create_tcb (open_fun, key, send, thread,
				       lower_extension)
	   val Connection_Internals {cstate, pending_open, last_error, upper,
				     auto_add, urgent_send, log, stats, ...}
		      = internals
	   val _ = Trace.debug_print (fn _ => "connected to " ^
				      Connection_Key.makestring key)
	   fun open_actions () = (start_user_timer cstate;
				  execute_actions internals)
	   fun raise_fun () =
		(stop_user_timer cstate;
		 B.Event.signal pending_open;
		 close_or_abort (internals, Tcp_State.abort, "abort");
		 raise X.Session ("Open Failed: " ^ ! last_error))
	   fun urgent_send_fun packet = (! urgent_send) packet
	   fun automatic_window_update () = ! auto_add
	   fun update_automatically () = auto_add := true
	   fun update_explicitly () = auto_add := false
	   fun connection_add_to_window i = add_to_window (internals, i)
	   fun send_immediately () =
		let val Tcp_Tcb.Tcb {send_immediately, ...} =
			   get_tcb (! cstate)
		in send_immediately := true
		end handle x =>
		      Trace.print_raise (X.Session "protocol finalized",
					 SOME "send_immediately")
	   fun start_logging () =
		case ! log of
		   NONE => log := SOME []
		 | SOME _ => ()
	   fun get_log () =
		case ! log of
		   NONE => []
		 | SOME x => rev x
	   fun start_stats () =
		case ! stats of
		   SOME _ => ()
		 | NONE => stats :=
		    SOME (Log.Tcp_Stats {packets_sent = zero64,
					 packets_resent = zero64,
					 packets_received = zero64,
					 bad_packets_received = zero64,
					 bytes_sent = fn _ => zero64,
					 bytes_received = fn _ => zero64})
	   fun get_stats () = ! stats
	   fun build_extension () =
		let val tcp_extension =
			   Tcp_Connection_Extension
			      {urgent_send = urgent_send_fun,
			       automatic_window_update =
				   automatic_window_update,
			       update_automatically = update_automatically,
			       update_explicitly = update_explicitly,
			       add_to_window = connection_add_to_window,
			       send_immediately = send_immediately,
			       start_logging = start_logging,
			       get_log = get_log,
			       start_stats = start_stats,
			       get_stats = get_stats}
		in (tcp_extension, internals)
		end
       in Trace.debug_constant_string
		   "open waiting for connection to complete";
	  extension := Instantiated (build_extension ());
	  if B.Event_Queue.wait {queue = upper, event = (),
				 while_waiting = open_actions} then ()
	  else raise_fun ();
	  stop_user_timer cstate;
	  B.Event.signal pending_open;
	  ()
       end

(*
	30.	internal function tcp_handler

	This function implements tcp handshaking, taking a tcp handler
	as its argument, and returning a handler for the tcp connection
	manager Tcp_Connection.Conn
*)

  (* since this connection handler returns immediately, the connection will
     be closed right away if the tcp_handler raises an exception. *)
  fun no_op _ = ()
  val handler_exception_value = {connection_handler = no_op,
				 data_handler = no_op,
				 status_handler = no_op}

  fun tcp_handler (open_fun, tally_open, tally_close, thread,
		   H tcp_handler_fun) =
       let fun conn_handler_fun key =
		let val started = ref false
		    val tcp_connection_handler =
		         ref (no_op: connection -> unit)
		    val sem = B.Semaphore.new ()
		    fun start_connection conn =
			 if ! started then ! tcp_connection_handler
			 else
			  ((started := true;
			    Trace.debug_print (fn _ => "calling do_connect");
			    do_connect (key, conn, open_fun, thread);
			    Trace.debug_print (fn _ => "calling tcp_handler");
			    let val {connection_handler, data_handler,
				     status_handler} =
				       tcp_handler_fun key
					 handle x => handler_exception_value
			    in Trace.debug_print (fn _ => "instantiating");
			       instantiate_handlers (conn, data_handler,
						     status_handler);
			       tcp_connection_handler := connection_handler;
			       Trace.debug_print (fn _ => "started");
			       connection_handler
			    end)
			   handle x =>
				   (! tcp_connection_handler))
		    fun register conn =
			 if ! started then ! tcp_connection_handler
			 else
			  B.Semaphore.with_lock (sem, start_connection, conn)
		    fun conn_data_handler (conn as Conn.C {extension, ...},
					   packet) =
			 (register conn;
			  case ! extension of
			     Instantiated (_, connection) =>
			      let val Connection_Internals
					{handlers, actions,
					 cstate, ...} = connection
			      in Trace.debug_constant_string
				    "starting higher data handler";
				 Tcp_Header.process_packet (! actions, packet);
				 execute_actions connection
			      end
			   | _ =>
			      Trace.print_raise (X.Session "uninstantiated",
						 SOME "conn_data_handler"))
		    fun conn_status_handler (conn as Conn.C {extension, ...},
					     status_value) =
			 (register conn;
			  case ! extension of
			     Instantiated (_, connection) =>
			      let val Connection_Internals
					{handlers, ...} = connection
			      in Trace.debug_constant_string
				    "starting higher debug handler";
				 case ! handlers of
				    Actual_Handlers {status, ...} =>
				     deliver_status (status, status_value,
						     "statue handler",
						     connection)
				  | Queued queue =>
				     handlers := Queued
						  (Tcp_Tcb.Q.add
						   (queue,
						    Status status_value))
			      end
			   | _ =>
			      Trace.print_raise (X.Session "uninstantiated",
						 SOME "conn_status_handler"))
		    fun conn_connection_handler (connection as
						 Conn.C {send, abort,
							 extension}) =
			 (let val tcp_connection_handler = register connection 
			      val (tcp_extension,
				   Connection_Internals {tcp_send,
							 tcp_abort, ...}) =
				    case ! extension of
				       Instantiated x => x
				     | _ =>	(* should not happen *)
					Trace.print_raise
					  (X.Session "uninstantiated",
					   SOME "conn_connection_handler")
			      val tcp_connection = C {send = ! tcp_send,
						      abort = ! tcp_abort,
						      extension =
							 tcp_extension}
			  in tally_open ();
			     Trace.debug_constant_string
				"starting higher connection handler";
			     tcp_connection_handler tcp_connection
			       handle x => Trace.print_handled
					     (x, SOME "connection_handler");
			     case ! extension of
				Instantiated (_, internals) =>
				 (Wait_Store.add (key, ());
				  close_or_abort (internals, Tcp_State.close,
						  "close"))
			      | _ =>
				 Trace.local_print "no connection to close";
			     tally_close ()
			  end)
		in case Wait_Store.look key of
		      NONE =>
		       {connection_handler = conn_connection_handler,
			data_handler = conn_data_handler,
			status_handler = conn_status_handler}
		    | SOME () =>
		       wait_handlers (key)
		end
       in Conn.H conn_handler_fun
       end

(*
	31.	function session
*)

  fun tcp_session session_fn conn_session = 
       let val done_queue = B.Event.new () 
	   val thread = Tcp_Tcb.new_timer_thread ()
	   val connection_count = ref 0;
	   fun tally_open () = connection_count := ! connection_count + 1
(* tally-close is called an additional time when the session handler
   exits, so we test for connection_count < 0 before signaling. *)
	   fun tally_close () =
		(connection_count := ! connection_count - 1;
		 Trace.trace_print (fn _ => "closing, connection count is " ^
				    Int.toString (! connection_count));
		 if ! connection_count < 0 then
		   (B.Event.signal done_queue;
		    ())
		 else ())
	   fun tcp_connect (Conn.S {connect = conn_connect, ...})
			   (address, handler) =
		conn_connect (address,
			      tcp_handler (Tcp_State.active_open, tally_open,
					   tally_close, thread, handler))
	   fun tcp_listen (Conn.S {listen = conn_listen, ...})
			  (pattern, handler, count) =
		conn_listen (pattern,
			     tcp_handler (Tcp_State.passive_open, tally_open,
					  tally_close, thread, handler),
			     count)
	   fun tcp_extension (Conn.S {extension = conn_extension, ...}) =
		conn_extension
	   val tcp_session = S {connect = tcp_connect conn_session,
				listen = tcp_listen conn_session,
				extension = tcp_extension conn_session}
	   fun after_session () =
		B.Event.wait (done_queue, tally_close)
       in (session_fn tcp_session
	   before after_session ())
	  handle x => (after_session (); raise x)
       end

  fun session (setup: Setup.T, session_fn: session -> 'a) =
       Conn.session (setup, tcp_session session_fn)

 end (* struct *)
