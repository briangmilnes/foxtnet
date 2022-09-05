(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract
	arp.fun: a generic implementation of the Address Resolution Protocol


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	iv.	Overview
	1.	functor Arp
	2.	protocol types
	3.	protocol state
	4.	address cache
	5.	function request
	6.	function reply
	7.	function arp_connect
	8.	function listen_lower
	9.	function arp_listen
	10.	function try_listen
	11.	function arp_data
	12.	function start_lower_session
	13.	functions create_session and close_session
	14.	function session


	iii.	RCS Log

$Log: arp.fun,v $
Revision 1.61  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.60  97/06/04  11:43:57  esb
added a debug_level, as required by cache.fun.

Revision 1.59  97/04/17  14:35:49  esb
changed caching to use cache.sig/cache.fun

Revision 1.58  96/07/22  21:01:48  cline
*** empty log message ***

Revision 1.57  1996/06/07  20:21:02  cline
handle exceptions in send

Revision 1.56  1996/04/30  20:24:02  esb
minor change.

Revision 1.55  1996/04/18  21:29:25  cline
converted hash from int to word

Revision 1.54  1996/02/16  16:32:16  cline
added cache

Revision 1.53  1996/02/07  19:15:19  cline
added null_hardware_address

Revision 1.52  1996/01/19  23:05:07  esb
adapted to the new wordarray signature.

Revision 1.51  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.50  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.49  1995/10/17  21:46:50  esb
fixed some bugs.

Revision 1.48  1995/10/02  21:57:42  esb
made the connection key into a datatype.

Revision 1.47  1995/10/02  21:19:19  esb
changed type of Arp_Connection_Key to be abstract.

Revision 1.46  1995/09/19  18:48:49  esb
changes in some print statements.

Revision 1.45  1995/09/13  15:32:47  esb
changed to handle Already_Open.

Revision 1.44  1995/08/08  18:24:48  esb
separated in and out external structures.

Revision 1.43  1995/06/28  10:19:24  esb
fixed a major bug where every ARP session had a lower session, but
only the first one would listen for broadcast ARP requests.

Revision 1.42  1995/06/27  19:07:03  cline
adapted to new extern.sig

Revision 1.41  1995/06/27  16:56:12  esb
fixed minor bugs.

Revision 1.40  1995/06/20  17:21:17  esb
major rewrite.

Revision 1.39  1995/03/12  17:54:12  esb
adapted to new trace.sig.

Revision 1.38  1995/03/07  23:51:23  esb
updated tracing.

Revision 1.37  1995/02/04  20:39:33  robby
updated to 107

Revision 1.36  1995/01/14  02:25:44  esb
adapted to new filter interface.

Revision 1.35  1995/01/06  01:37:58  esb
renamed the interface to filter.

Revision 1.34  1994/12/21  20:36:49  milnes
Updated for new shadowed addressing, but it produces terrible performance
problems when the duplicate filters are installed.

Revision 1.33  1994/12/01  18:45:34  esb
renamed parameters to Event_Queue.{signal,wait}.

Revision 1.32  1994/11/12  17:00:50  cstone
Updated to change in Debug_Trace_Printer functor argument

Revision 1.31  1994/11/10  22:15:57  milnes
Added some different tracing and timeouts.

Revision 1.30  1994/11/08  00:03:21  esb
found the location of the excessive close in ARP, and added a comment.

Revision 1.29  1994/10/25  16:32:41  esb
added handles around all calls to Conn.functions.

Revision 1.28  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.27  1994/09/30  17:04:28  esb
added closing of lower connections on failed passive opens.

Revision 1.26  1994/09/23  16:49:53  milnes
Added lower connection close on a failed passive open.

Revision 1.25  1994/09/12  18:21:24  milnes
Added prints for handle _'s.

Revision 1.24  1994/08/28  21:43:23  milnes
Added prints.

Revision 1.23  1994/08/12  06:24:43  esb
fixed reply source/dest addresses, which were reversed; added passive opens.

Revision 1.22  1994/08/02  20:29:33  esb
adapted to new protocol signature.

Revision 1.21  1994/07/04  21:35:30  esb
adapted to Copy/Create split.

Revision 1.20  1994/07/01  02:35:24  danwang
Moved control structures into Fox_Basis.

Revision 1.19  1994/06/16  16:46:40  danwang
Updated to use functorized Fox_Basis

Revision 1.18  1994/05/10  07:43:39  esb
adapted to new store.sig.

Revision 1.17  94/04/26  20:10:35  esb
added code to insure the passive open is started before register completes.

Revision 1.16  94/04/26  18:05:01  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.15  94/04/06  23:22:36  esb
adapted to new receive_packet interface.

Revision 1.14  94/03/10  20:15:36  esb
introduced Copy.create and Copy.create_fn.

Revision 1.13  94/02/21  00:03:18  esb
modified send to adapt to new send_packet interface.

Revision 1.12  94/01/17  18:13:14  esb
interface changes.

Revision 1.11  1994/01/13  16:24:01  milnes
Updated fork calls that now return type coroutine to be followed with a unit.

Revision 1.10  93/12/17  02:36:04  esb
improved the error messages.

Revision 1.9  1993/12/04  20:55:07  esb
now provide a handler with a parameter of type connection.

Revision 1.8  1993/10/27  01:19:26  esb
made the types explicit by using datatypes.

Revision 1.7  1993/10/26  22:44:28  esb
added a protocol number argument to bytes_to_lower, which was needed.

Revision 1.6  1993/10/25  19:36:32  cline
removed .U from Byte[421].U

Revision 1.5  1993/10/09  17:49:36  esb
consolidated the protocol state; we now use the new store interface.

Revision 1.4  1993/09/22  19:31:56  milnes
Removed "-----------"s.

Revision 1.3  1993/09/13  22:07:44  cline
deleted '#'s from RCS log

Revision 1.2  1993/09/02  19:40:38  esb
adapted to new PROTOCOL signature.

Revision 1.1  1993/08/24  21:20:29  esb
Initial revision

	iv.	Overview

The trick about Arp is to make it into a virtual protocol, that is, a
protocol that participates in connection setup and teardown but not in
data delivery.  To do this, we define a 1-1 correspondence between Arp
connections and lower-protocol connections.  On connection setup, we
do our part of the work, then set up a connection using the lower
protocol, giving it the handlers that are handed in to Arp.

	1.	functor Arp
*)

functor Arp (structure Lower: PROTOCOL
	     structure Hardware_Address: EXTERN_KEY
	       where type extern_in = Lower.Incoming.T
	         and type extern_out = Lower.Outgoing.T
		 and type cursor = Word.word
	     structure Protocol_Address: ARP_PROTOCOL_EXTERN
	       where type regular_extern_in = Lower.Incoming.T
	         and type extern_out = Lower.Outgoing.T
	         and type cursor = Word.word
	     val null_hardware_address: Hardware_Address.T
	     val local_hardware_address: Lower.session_extension
	                               -> Hardware_Address.T
	     val arp_protocol_number: Word16.word
	     val broadcast_address: Word16.word -> Lower.Address.T
	     val broadcast_pattern: Word16.word -> Lower.Pattern.T
	     val lower_address: Hardware_Address.T * Word16.word
	                      -> Lower.Address.T
	     val lower_pattern: Hardware_Address.T * Word16.word
	                      -> Lower.Pattern.T
	     val min_max_packet: Lower.session_extension
	                       -> (Word.word * Word.word)
	     val hardware_type: Word16.word
	     val arp_timeout: int	(* milliseconds *)
	     val arp_resend: int	(* count *)
	     structure B: FOX_BASIS
	     structure Trace: TRACE) (* : ADDRESS_RESOLUTION_PROTOCOL *) =
 struct

  structure Host = Protocol_Address
  structure Protocol =
   struct
    type T = Word16.word
    val equal: (T * T -> bool) = op=
    val hash = Word.fromLargeWord o Word16.toLargeWord
    val makestring = Integer.toString o Word16.toInt
   end

  structure Arp_Address: ARP_ADDRESS =
   struct
    type host = Host.T
    type protocol = Word16.word
    datatype address = Specific of {self: host, peer: host, protocol: protocol}
                     | Broadcast of protocol
    type T = address
    fun makestring (Specific {self, peer, protocol}) =
         "self = " ^ Protocol_Address.makestring self ^
         ", peer = " ^ Protocol_Address.makestring peer ^
	 ", protocol = " ^ Integer.toString (Word16.toInt protocol)
      | makestring (Broadcast protocol) =
	 "broadcast " ^ Integer.toString (Word16.toInt protocol)
    fun equal (Specific {self = s1, peer = p1, protocol = pr1},
	       Specific {self = s2, peer = p2, protocol = pr2}) =
         Host.equal (s1, s2) andalso Host.equal (p1, p2) andalso pr1 = pr2
      | equal (Broadcast pr1, Broadcast pr2) = pr1 = pr2
      | equal _ = false
    fun hash (Specific {self, peer, protocol}) =
         Host.hash self + Host.hash peer +
	 Word.fromLargeWord (Word16.toLargeWord protocol)
      | hash (Broadcast protocol) =
	 Word.fromLargeWord (Word16.toLargeWord protocol)
   end (* struct *)
  structure Address = Arp_Address

  structure Connection_Key = Lower.Connection_Key

  structure Arp_Pattern: ARP_PATTERN =
   struct
    type host = Host.T
    type protocol = Word16.word
    datatype pattern = Specific of {self: host, protocol: protocol}
                     | Broadcast of protocol
    type T = pattern
    fun makestring (Specific {self, protocol}) =
         Protocol_Address.makestring self ^ "/" ^
	 Integer.toString (Word16.toInt protocol)
      | makestring (Broadcast protocol) =
	 "broadcast " ^ Integer.toString (Word16.toInt protocol)
    fun equal (Specific {self = s1, protocol = p1},
	       Specific {self = s2, protocol = p2}) =
         Host.equal (s1, s2) andalso p1 = p2
      | equal (Broadcast pr1, Broadcast pr2) = pr1 = pr2
      | equal _ = false
    fun hash (Specific {self, protocol}) =
         Host.hash self + Word.fromLargeWord (Word16.toLargeWord protocol)
      | hash (Broadcast protocol) =
	 Word.fromLargeWord (Word16.toLargeWord protocol)
   end (* struct *)
  structure Pattern = Arp_Pattern

  structure Setup = Lower.Setup
  structure Incoming = Lower.Incoming
  structure Outgoing = Lower.Outgoing
  structure Status = Lower.Status
  structure Count = Lower.Count
  structure X = Lower.X

  exception Already_Open of Connection_Key.T

  type connection_extension = Lower.connection_extension
  type listen_extension = unit

  type arp_session_extension = {maximum_packet_size: Word.word,
				minimum_packet_size: Word.word}
  type session_extension = arp_session_extension

  structure Header =
    Arp_Header (structure Hardware_Address = Hardware_Address
		structure Protocol_Address = Protocol_Address
		structure In = Lower.Incoming
		structure Out = Lower.Outgoing
		structure B = B
		val null_hardware_address = null_hardware_address)

(*
	2.	protocol types
*)

  local			(* type connection = Lower.connection *)
   structure S:
    sig
     datatype connection = C of {send: Outgoing.T -> unit,
				 abort: unit -> unit,
				 extension: connection_extension}
    end = Lower
  in
   open S
  end

  datatype listen = L of {stop: unit -> unit, extension: listen_extension}

  datatype handler = H of Connection_Key.T
                           -> {connection_handler: connection -> unit,
			       data_handler: connection * Incoming.T -> unit,
			       status_handler: connection * Status.T -> unit}

  datatype session =
      S of {connect: Address.T * handler -> unit,
	    listen: Pattern.T * handler * Count.T -> listen,
	    extension: session_extension}

(*
	3.	protocol state
*)

  datatype synch = Waiting | Ready

(* These tickle a compiler bug (in 109.25.2) when arpeth.fun is compiled,
   so we still use B.Store instead.
  structure Queries = Imperative_Monomorphic_Store
                          (structure V = B.V
			   type key = Protocol_Address.T
			   type value = Hardware_Address.T option B.Pipe.T
			   val eq = Protocol_Address.equal
			   val hash = Protocol_Address.hash)
*)

(*
  structure Listens = Imperative_Monomorphic_Store
                          (structure V = B.V
			   type key = Pattern.T
			   type value = handler * Count.T * synch
			   val eq = Pattern.equal
			   val hash = Pattern.hash)
*)

  datatype state =
      Arp of {count: int ref,
	      queries: (Protocol_Address.T,
			Hardware_Address.T option B.Pipe.T) B.Store.T ref,
	      listens: (Pattern.T, handler * Count.T * synch) B.Store.T ref,
	      lower_max_size: Word.word,
	      lower_min_size: Word.word,
	      lower_listen: Lower.Pattern.T * Lower.handler * Lower.Count.T
	                  -> Lower.listen,
	      lower_connect: Lower.Address.T * Lower.handler -> unit,
	      complete: unit B.Pipe.T,
	      completed_close: unit B.Pipe.T,
	      lower_send: Lower.Outgoing.T -> unit,
	      lower_stop: unit -> unit,
	      local_address: Hardware_Address.T}

  val state = ref (NONE: state option)

  val session_sem = B.Semaphore.new ()

(*
	4.	address cache
*)

  type key = {protocol_number: Word16.word,
	      sender_hardware: Hardware_Address.T,
	      sender_protocol: Protocol_Address.T,
	      receiver_protocol: Protocol_Address.T}

  fun eq_key ({protocol_number = p1, sender_hardware = sh1,
	       sender_protocol = sp1, receiver_protocol = rp1},
	      {protocol_number = p2, sender_hardware = sh2,
	       sender_protocol = sp2, receiver_protocol = rp2}) =
       p1 = p2 andalso
       Hardware_Address.equal (sh1, sh2) andalso
       Protocol_Address.equal (sp1, sp2) andalso
       Protocol_Address.equal (rp1, rp2)

  fun hash_key ({protocol_number, sender_hardware,
		 sender_protocol, receiver_protocol}) =
       Hardware_Address.hash sender_hardware

  structure Cache = Single_Cache (structure V = B.V
				  type key = key
				  type value = Hardware_Address.T
				  val max_elements = 64
				  val max_space = 64
				  val eq = eq_key
				  val hash = hash_key
				  val debug_level = NONE)

(*
	5.	function request
*)

  fun word_max (a, b: Word.word) = if a > b then a else b

  fun network_request ({self, peer, protocol}, local_address,
		       send, count, time, queries, lower_min_size) =
       (let val req_data = Header.Request {hardware_type = hardware_type,
					   protocol_number = protocol,
					   sender_hardware = local_address,
					   sender_protocol = self,
					   receiver_protocol = peer}
	    val packet_size = word_max (Header.size req_data, lower_min_size)
	    val packet = Lower.Outgoing.uninitialized packet_size
	    val _ = (Header.marshal (packet, req_data) 0w0)
		     handle x =>
		             (Trace.local_print
			      ("allocated packet of size " ^
			       Word.toString packet_size ^
			       ", lower_min_size " ^
			       Word.toString lower_min_size ^
			       ", Header.size " ^
			       Word.toString (Header.size req_data) ^
			       ", address is " ^ 
			       Arp_Address.makestring
			       (Arp_Address.Specific {self = self,
						      peer = peer,
						      protocol = protocol}));
			      Trace.print_raise_again (x, SOME "marshal"))
	    val receive = B.Pipe.new (): Hardware_Address.T option B.Pipe.T
	    fun fail () = B.Pipe.enqueue (receive, NONE)
	    fun timeout time () =
	         (B.Scheduler.sleep time;
		  fail ())
	    fun loop (0, _) = NONE
	      | loop (n, wait_time) =
	         (B.Scheduler.fork (timeout wait_time);
		  Trace.trace_print
		    (fn _ =>
		     Protocol_Address.makestring self ^
		     " sending request for address " ^
		     Protocol_Address.makestring peer ^ "/" ^
		     (Integer.toString (Word16.toInt protocol)));
		  send packet handle x => Trace.print_handled
		                            (x, SOME "lower send");
		  case B.Pipe.dequeue receive of
		     SOME response =>
		      (Cache.add ({protocol_number = protocol,
				   sender_hardware = local_address,
				   sender_protocol = self,
				   receiver_protocol = peer},
				  response, 1);
		       SOME response)
		   | NONE => loop (n - 1, wait_time + wait_time))
        in queries := B.Store.add (! queries, peer, receive);
	   loop (count, time)
	   before queries := B.Store.remove (! queries, peer)
        end)
	 handle x => Trace.print_raise_again (x, SOME "request")

  fun request (args as ({self, peer, protocol}, local_address,
			_, _, _, _, _)) =
       case Cache.look {protocol_number = protocol,
			sender_hardware = local_address,
			sender_protocol = self, receiver_protocol = peer} of
	  SOME response => SOME response
	| NONE => network_request args

(*
	6.	function reply
*)

  fun reply (send, hardware_type, protocol_number, sender_hardware,
	     sender_protocol, receiver_hardware, receiver_protocol,
	     lower_min_size) =
       let val reply_data =
	         Header.Reply {hardware_type = hardware_type,
			       protocol_number = protocol_number,
			       sender_hardware = sender_hardware,
			       sender_protocol = sender_protocol,
			       receiver_hardware = receiver_hardware,
			       receiver_protocol = receiver_protocol}
	   val packet_size = word_max (Header.size reply_data, lower_min_size)
	   val packet = Lower.Outgoing.uninitialized packet_size
       in Trace.trace_print (fn _ =>
			     "sending reply for address " ^
			     Protocol_Address.makestring sender_protocol ^
			     " to " ^
			     Protocol_Address.makestring receiver_protocol);
          Header.marshal (packet, reply_data) 0w0;
          send packet
       end

(*
	7.	function arp_connect
*)

  fun arp_connect (Arp {lower_send, queries, lower_min_size,
			local_address, ...}, lower_connect)
                  (addr as (Address.Specific address), H handler) =
       (case request (address, local_address, lower_send, arp_resend,
		      arp_timeout, queries, lower_min_size) of
	   NONE =>
	    let val failure = "unable to resolve " ^ Address.makestring addr
	    in Trace.print_raise (X.Connection failure, NONE)
	    end
	 | SOME lower_peer =>
	    let val {self, peer, protocol} = address
	        val lower = lower_address (lower_peer, protocol)
	    in Trace.trace_print (fn _ => "connecting to lower " ^
				  Lower.Address.makestring lower ^
				  " for higher address " ^
				  Address.makestring addr);
	       ((lower_connect (lower, Lower.H handler))
		handle Lower.Already_Open lower_key =>
		        raise (Already_Open lower_key))
	    end)
	 (* no ARPing for broadcast addresses. *)
    | arp_connect (Arp {lower_send, queries, lower_min_size, ...},
		   lower_connect)
                  (addr as (Address.Broadcast protocol), H handler) =
       ((Trace.trace_print (fn _ => "connecting to lower " ^
			    Lower.Address.makestring
			       (broadcast_address protocol) ^
			    " for higher address " ^
			    Address.makestring addr);
         lower_connect (broadcast_address protocol, Lower.H handler))
	handle Lower.Already_Open lower_key =>
	        (Trace.local_print ("got already open on lower key " ^
				    Lower.Connection_Key.makestring lower_key ^
				    " for higher address " ^
				    Address.makestring addr);
		 raise X.Connection "unknown connection is open"))

(*
	8.	function listen_lower
*)

  fun listen_lower (lower_listen, protocol, handler, max) =
       let val lower_pattern = broadcast_pattern protocol
	   val lower_handler = Lower.H handler
	   val Lower.L {stop, extension} =
	         lower_listen (lower_pattern, lower_handler, max)
       in stop
       end

(*
	9.	function arp_listen

	An Arp_pattern always specifies that broadcast packets
	will be accepted.
*)

  fun arp_listen (Arp {listens, ...}, lower_listen)
                 (pattern as (Pattern.Specific {protocol, self}),
		  H handler, max) =
       (Trace.trace_print (fn _ => "arp-listen specific " ^
			   Pattern.makestring pattern);
	case B.Store.look (! listens, pattern) of
	   NONE =>
	    let val lower_stop = listen_lower (lower_listen, protocol,
					       handler, max)
		fun arp_stop () =
		     (listens := B.Store.remove (! listens, pattern);
		      lower_stop ())
	    in listens := B.Store.add (! listens, pattern,
				       (H handler, max, Ready));
	       L {stop = arp_stop, extension = ()}
	    end
	 | SOME _ =>
	    Trace.print_raise (X.Listen ("already listening for pattern " ^
					 Pattern.makestring pattern),
			       SOME "arp_listen"))
    | arp_listen (_, lower_listen)
                 (pattern as (Pattern.Broadcast protocol), H handler, max) =
       (Trace.trace_print (fn _ => "arp-listen broadcast " ^
			   Pattern.makestring pattern);
	L {stop = listen_lower (lower_listen, protocol, handler, max),
	   extension = ()})
       handle _ =>
	       Trace.print_raise (X.Listen ("already listening for " ^
					    Pattern.makestring pattern),
				  SOME "arp_listen")

(*
	10.	function try_listen

	For an incoming request, try to start a corresponding listen, and
	return whether we started one.
*)

  fun try_listen (self, peer, protocol, peer_lower, listens, lower_listen) =
       let val pattern = Arp_Pattern.Specific {self = self,
					       protocol = protocol}
       in case B.Store.look (! listens, pattern) of
	     SOME (_, (H upper_handler, max, Ready)) =>
	      let val (new_max, valid, remove) =
	                case max of
			   Count.Unlimited => (max, true, false)
			 | Count.Maximum n =>
			    (Count.Maximum (n - 1), n >= 1, n <= 1)
			 | Count.Incremental f =>
			    (case f () of
			        Count.Continue => (max, true, false)
			      | Count.Done =>     (max, false, true))
		  val listen_pattern = lower_pattern (peer_lower, protocol)
		  fun call_handler key =
		       (Trace.trace_constant_string "listen handler called";
			if remove then
			 listens := B.Store.remove (! listens, pattern)
			else
			 listens := B.Store.add (! listens, pattern,
						 (H upper_handler, new_max,
						  Ready));
			 Trace.debug_constant_string "calling upper handler";
			upper_handler key)
		  val lower_handler = Lower.H call_handler
	      in if valid then		(* listen and reply *)
	          (listens := B.Store.add (! listens, pattern,
					   (H upper_handler, new_max,
					    Waiting));
		   ((lower_listen (listen_pattern, lower_handler,
				   Lower.Count.Maximum 1);
		     ())
	(* we might already be listening, in which case, reply anyway. *)
		    handle _ => ());
		   true)
		 else
		  (if remove then
		    listens := B.Store.remove (! listens, pattern)
		   else ();
		   false)
	      end
	   | SOME _ => true		(* waiting for connection, so reply *)
	   | NONE => false		(* we're not listening, ignore *)
       end

(*
	11.	function arp_data
*)

  fun arp_data (queries, listens, lower_listen, received, local_address,
		lower_min_size) (Lower.C {send, ...}, packet) =
       ((case Header.unmarshal (packet, 0w0) of
	    (Header.Request {hardware_type, protocol_number, sender_hardware,
			     sender_protocol, receiver_protocol}, _) =>
	     if try_listen (receiver_protocol, sender_protocol,
			    protocol_number, sender_hardware, listens,
			    lower_listen) then
	      (Trace.trace_print
	           (fn _ =>
		    "replying to request for " ^
		    Protocol_Address.makestring receiver_protocol ^
		    ", protocol " ^
		    Integer.toString (Word16.toInt protocol_number));
	       reply (send, hardware_type, protocol_number,
		      local_address, receiver_protocol,
		      sender_hardware, sender_protocol, lower_min_size))
	     else			(* not for us, do not reply *)
	      Trace.debug_print
	       (fn _ => "discarding request for " ^
		Protocol_Address.makestring receiver_protocol ^
		(if B.Store.empty (! listens) then
		  (", lower address is " ^
		   Hardware_Address.makestring local_address)
		 else
		  (", local address(es) are " ^
		   B.Store.makestring (! listens,
				       fn (key, _) => Pattern.makestring key,
				       ", "))))
	  | (Header.Reply {hardware_type, protocol_number, sender_hardware,
			   sender_protocol, receiver_hardware,
			   receiver_protocol}, _) =>
	     case B.Store.look (! queries, sender_protocol) of
	        SOME (_, pipe) =>
		 (Trace.trace_print
		    (fn _ => "received reply from " ^
		     Protocol_Address.makestring sender_protocol ^
		     " for " ^
		     Protocol_Address.makestring receiver_protocol);
		  B.Pipe.enqueue (pipe, SOME sender_hardware))
	      | NONE =>
		 Trace.trace_print
		    (fn _ => "discarding reply from " ^
		     Protocol_Address.makestring sender_protocol ^
		     " for " ^
		     Protocol_Address.makestring receiver_protocol))
	handle Header.Extern =>
	        Trace.local_print ("illegal arp packet " ^
				   Lower.Incoming.makestring_max
				   (packet, 0w40));
        case received of
	   NONE => ()
	 | SOME pipe =>
	    (B.Pipe.enqueue (pipe, ());
	     ()))

(*
	12.	function start_lower_session
*)

  fun start_lower_session (complete_session, completed_close, setup) =
       let val initialized = B.Pipe.new (): Lower.session B.Pipe.T
	   fun wait_session argument =
	        (B.Pipe.enqueue (initialized, argument);
		 B.Pipe.dequeue complete_session;
		 ())
	   fun lower_thread () =
	        (Lower.session (setup, wait_session);
		 B.Pipe.enqueue (completed_close, ()))
       in B.Scheduler.fork lower_thread;
	  B.Pipe.dequeue initialized
       end

(*
	13.	functions create_session and close_session

	The broadcast_handler will keep the connection open until
	something is enqueued on "complete", at which point it will
	signal "complete_session" to allow the completion of the
	session handler.  The specific handler
	will close the connection as soon as a packet (presumed
	to be an ARP reply) is received.
*)

  fun create_session setup =
       (case ! state of
	   NONE =>
	    let val complete = B.Pipe.new (): unit B.Pipe.T
	        val complete_session = B.Pipe.new (): unit B.Pipe.T
	        val completed_close = B.Pipe.new (): unit B.Pipe.T
	        val lower_send_pipe = B.Pipe.new ()
		                    : (Lower.Outgoing.T -> unit) B.Pipe.T
	        val queries = ref (B.Store.new (Protocol_Address.hash,
					        Protocol_Address.equal))
	        val listens = ref (B.Store.new (Pattern.hash, Pattern.equal))
	        val lower_session =
	             start_lower_session (complete_session, completed_close,
					  setup)
	        val (Lower.S {connect = lower_connect, listen = lower_listen,
			      extension}) = lower_session
		val local_address = local_hardware_address extension
		val (min_packet, max_packet) = min_max_packet extension
		fun c_handler (Lower.C {send, ...}) =
		     (B.Pipe.enqueue (lower_send_pipe, send);
		      B.Pipe.dequeue complete;
		      B.Pipe.enqueue (complete_session, ());
		      ())
	        fun broadcast_handler key =
		     (Trace.trace_print (fn _ =>
					 "broadcast handler called on key " ^
					 Lower.Connection_Key.makestring key);
	              {connection_handler = c_handler,
		       data_handler = arp_data (queries, listens, lower_listen,
					        NONE, local_address,
					        min_packet),
		       status_handler = (fn _ => ())})
	        fun specific_handler key =
	             let val pipe = B.Pipe.new (): unit B.Pipe.T
		         fun connection _ = B.Pipe.dequeue pipe
		     in Trace.debug_print (fn _ =>
					   "specific handler called on key " ^
					   Lower.Connection_Key.makestring
					   key);
		        {connection_handler = connection,
			 data_handler = arp_data (queries, listens,
						  lower_listen, SOME pipe,
						  local_address, min_packet),
			 status_handler = (fn _ => ())}
		     end
		fun connect_thread () = 
		     (lower_connect (broadcast_address arp_protocol_number,
				     Lower.H broadcast_handler);
		      ())
		val Lower.L {stop, ...} =
		     lower_listen (broadcast_pattern arp_protocol_number,
				   Lower.H specific_handler,
				   Lower.Count.Unlimited)
		val _ = B.Scheduler.fork connect_thread
	        val new_state = Arp {count = ref 1, queries = queries,
				     lower_connect = lower_connect,
				     lower_listen = lower_listen,
				     lower_min_size = min_packet,
				     lower_max_size = max_packet,
				     listens = listens, complete = complete,
				     completed_close = completed_close,
				     lower_send =
				       B.Pipe.dequeue lower_send_pipe,
				     lower_stop = stop,
				     local_address = local_address}
            in state := SOME new_state;
	       new_state
            end
	 | SOME (s as (Arp {count, ...})) =>
	    (count := ! count + 1;
	     s))

  fun close_session () =
       (case ! state of
	   SOME (Arp {count, lower_stop, complete, completed_close, ...}) =>
	    (count := ! count - 1;
	     Trace.trace_print (fn _ => "session reference count is now " ^
				Int.toString (! count));
	     if ! count = 0 then
	      (state := NONE;
	       lower_stop ();
	       B.Pipe.enqueue (complete, ());
	       B.Pipe.dequeue completed_close;
	       ())
	     else ())
	 | NONE => ())

(*
	14.	function session
*)

  fun session (setup, session_fun) =
       let val state_value =
		B.Semaphore.with_lock (session_sem, create_session, setup) 
	   val Arp {lower_max_size, lower_min_size, lower_listen,
		    lower_connect, ...} = state_value
	   val our_extension = {maximum_packet_size = lower_max_size,
				minimum_packet_size = lower_min_size}
	   datatype 'a result = R of 'a | X of exn
	   val session = (S {connect = arp_connect (state_value,
						    lower_connect),
			     listen = arp_listen (state_value, lower_listen),
			     extension = our_extension})
	    val result = ((R (session_fun session)) handle x => X x)
       in B.Semaphore.with_lock (session_sem, close_session, ());
	  case result of
	     R value => value
	   | X x =>
	      Trace.print_raise_again (x, SOME "arp session")
       end

 end (* struct *)


