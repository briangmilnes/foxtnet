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

	user.fun: user-level interface module for protocols

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor User
	2.	structure UPT
	3.	structure CO
	4.	CO.receive
	5.	CO.connect
	6.	CO.passive_open
	7.	CO.close, Co.abort
	8.	structure CL
	9.	connection management for CL
	10.	internal handler
	11.	CL.new
	12.	CL.receive
	13.	top-level declarations of functor User

---------------------------------------------------------------------
	iii.	RCS Log

$Log: user.fun,v $
Revision 1.28  1995/03/12  17:58:17  esb
adapted to new trace.sig.

Revision 1.27  1995/03/10  03:49:57  esb
adapted to new vendor.sig.

Revision 1.26  1994/12/01  18:45:54  esb
renamed parameters to Event_Queue.{signal,wait}.

Revision 1.25  1994/08/12  06:19:48  esb
added type allocation.

Revision 1.24  1994/08/02  20:36:53  esb
adapted to new protocol signature.

Revision 1.23  1994/07/01  02:35:16  danwang
Moved control structures into Fox_Basis.

Revision 1.22  1994/05/10  07:45:23  esb
adapted to new store.sig.

Revision 1.21  94/04/26  17:56:39  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.20  94/01/17  18:12:45  esb
interface changes.

Revision 1.19  1994/01/13  16:23:07  milnes
Updated fork calls that now return type coroutine to be followed with a unit.

Revision 1.18  93/12/17  02:35:45  esb
improved the error messages.

Revision 1.17  1993/12/04  20:54:22  esb
now provide a handler with a parameter of type connection.

Revision 1.16  1993/10/14  18:24:30  milnes
Used implicit sequencing in let bodies.

Revision 1.15  1993/10/13  17:32:55  esb
adapted to change in interface of Pipe.

Revision 1.14  1993/10/06  02:33:17  esb
adapted to new store module.

Revision 1.13  1993/09/22  19:31:56  milnes
Removed "-----------"s.

Revision 1.12  1993/09/18  21:50:47  esb
minor changes

Revision 1.11  1993/09/17  16:44:43  milnes
 Changed default parameters.

Revision 1.10  1993/09/13  22:07:49  cline
deleted '#'s from RCS log

Revision 1.9  1993/09/10  11:36:34  esb
adapted to new event.sig.

Revision 1.8  1993/09/02  15:50:03  esb
adapted to proto.sig version 1.5 and other changes in the basis.

Revision 1.7  1993/08/17  16:18:11  esb
put a band-aid on a race condition, and also improved error messages.

Revision 1.6  1993/08/16  21:46:01  esb
added a do-if-debug for receive and improved some error messages

Revision 1.5  1993/07/27  20:58:09  esb
Fixed a bug in finalize.

Revision 1.4  1993/07/07  16:01:18  esb
modified to compile under the new USER protocol signature

Revision 1.3  1993/06/22  18:00:33  esb
removed some of the debugging messages

Revision 1.2  1993/06/22  17:54:11  esb
added do_if_debug and several debugging statements

Revision 1.1  1993/06/21  19:09:29  esb
Initial revision

*)

(*
---------------------------------------------------------------------
	1.	functor User
*)

functor User (structure Protocol: PROTOCOL
	      val match: Protocol.connection * Protocol.address -> bool
	      type peer
	      val connection_peer: Protocol.connection -> peer
(* the next two parameters are the per-connection queuing parameters
   for the connection-oriented protocol; new incoming messages are
   discarded when the packet queue has more than co_max_queued packets
   and if co_discard_excess is true. *)
	      val co_max_queue: int	(* max queued packets before discard *)
	      val co_discard_excess: bool	(* discard if max exceeded? *)
(* the next two parameters define the connectionless queuing parameters.
   packets are queued for the protocol as a whole (since there are no
   connections), and discarded after reaching cl_max_packet_age or if the
   queue grows to exceed cl_max_queue. The age of a packet is measured
   as the number of user receive operations since the packet was received.
   Unlike the connection-oriented queue, older connectionless packets
   are discarded before newer packets. If cl_discard_excess is false,
   only over-age packets are discarded. *)
	      val cl_max_packet_age: int	(* max "age" before discard *)
	      val cl_max_queue: int	(* max queued packets before discard *)
	      val cl_discard_excess: bool	(* discard if max exceeded *)
(* this parameter determines after how many calls to new, send, or
   receive we can start closing unused connections *)
	      val cl_tick: int
	      structure B: FOX_BASIS
	      val debug_level: int ref option): USER_PROTOCOL =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "user.fun")
  val debug_constant_string = Trace.debug_constant_string

(*
---------------------------------------------------------------------
	2.	structure UPT
*)

   structure UPT: USER_PROTOCOL_TYPES = Protocol

(*
---------------------------------------------------------------------
	3.	structure CO
*)

   structure CO: CONNECTION_ORIENTED =
    struct

     structure UPT = UPT

     type connection = Protocol.connection

     exception Connection_Closed = Protocol.Connection_Closed
     exception Open_Failed = Protocol.Open_Failed

     val allocate_send = Protocol.allocate_send

(*
---------------------------------------------------------------------
	4.	CO.receive
*)

     type pipe_store = (Protocol.connection,
			Protocol.incoming B.Pipe.T) B.Store.T
     val pipe_store = ref ((B.Store.new (Protocol.hash_connection,
					 Protocol.equal_connection)):
			   pipe_store)

     fun receive connection =
          case B.Store.look (! pipe_store, connection) of
             NONE => 
	      (debug_constant_string "user receive called, but no connection";
	       raise Connection_Closed (connection, "from user receive"))
           | SOME (new_store, message_pipe) =>
	      (pipe_store := new_store;
               debug_constant_string "receive requesting message";
	       let val message = B.Pipe.dequeue message_pipe
	       in debug_constant_string "receive returning message";
	          message
	       end)

(*
---------------------------------------------------------------------
	5.	CO.connect
*)

     fun connect address =
          let val message_pipe = B.Pipe.new (SOME co_max_queue)
              fun data_handler message =
                   if co_discard_excess andalso
                      B.Pipe.size message_pipe > co_max_queue then ()
	           else
		    B.Pipe.enqueue (message_pipe, message)
              fun status_handler _ = ()
	      fun connection_handler connection =
	           (data_handler, status_handler)
	      val handler = Protocol.Handler connection_handler
	      val _ = debug_constant_string "opening connection"
	      val conn = Protocol.connect (address, handler)
	      val _ = debug_constant_string "connection open"
          in pipe_store := B.Store.add (! pipe_store, conn, message_pipe);
	     conn
          end

(*
---------------------------------------------------------------------
	6.	CO.passive_open
*)

     fun passive_open address =
          let val receive_pipe = B.Pipe.new (SOME co_max_queue)
              val open_pipe = B.Pipe.new NONE
   	      fun message_handler message =
	           if co_discard_excess andalso
	              B.Pipe.size receive_pipe > co_max_queue then ()
	           else B.Pipe.enqueue (receive_pipe, message)
	      fun data_handler connection =
	           (B.Pipe.enqueue (open_pipe, connection);
		    message_handler)
              fun status_handler connection =
	           (B.Pipe.enqueue (open_pipe, connection);
		    fn (x: Protocol.status) => ())
	      fun connection_handler connection =
	           (data_handler connection, status_handler connection)
	      val handler = Protocol.Handler connection_handler
	      val _ = Protocol.start_passive (address, handler, SOME 1)
	      val conn = B.Pipe.dequeue open_pipe
          in pipe_store := B.Store.add (! pipe_store, conn, receive_pipe);
	     conn
          end (* let *)

(*
---------------------------------------------------------------------
	7.	CO.close, Co.abort
*)

     fun close connection =
      (pipe_store := B.Store.remove (! pipe_store, connection);
       Protocol.close connection)

     fun abort connection =
      (pipe_store := B.Store.remove (! pipe_store, connection);
       Protocol.abort connection)

    end (* struct CO *)

(*
---------------------------------------------------------------------
	8.	structure CL
*)

   structure CL: CONNECTIONLESS =
    struct

     structure UPT = UPT

     type remote = Protocol.connection

     type peer = peer

(*
---------------------------------------------------------------------
	9.	connection management for CL

	We keep two tables of connections: old connections and current
	connections, all of which are open. Every cl_tick sends or
	receives, all connections still in the old table are closed, and
	connections in the current table are transferred to the old table.
	Connections then get moved back from the old to the current table
	when they are used. This implements a form of LRU connection
	management similar to Unix's LRU page replacement strategy on
	the VAX.

	When we send, if we have no connection corresponding to the
	address, we open that connection and add it to the table,
        then send.

	When we receive, if we have no connection for this
	address, we start a passive open if there are none. We then
	search through the queued packets for a matching one, and
	queue our continuation if none are available.
*)

     type conn_store = (Protocol.address, Protocol.connection) B.Store.T
     type passive_store = (Protocol.address,
			   ((unit -> unit)
			    * (unit -> Protocol.connection list))) B.Store.T

     val conn_store = ref ((B.Store.new (Protocol.hash_address,
					 Protocol.equal_address)): conn_store)
     val passive_store = ref ((B.Store.new (Protocol.hash_address,
					    Protocol.equal_address)):
			      passive_store)

     val old_conns = ref ((B.Store.new (Protocol.hash_address,
					Protocol.equal_address)): conn_store)
     val old_passives = ref ((B.Store.new (Protocol.hash_address,
					   Protocol.equal_address)):
			     passive_store)

     val call_count = ref 0

     fun stop_passives (_, (stop, _)) = stop ()

     fun remove_connection (_, connection) =
          (Protocol.close connection;
	   connection)

(* every cl_tick or so calls to "send" or "receive", we close the
   connections in the "old" list, and we transfer the current
   ones to the old list *)
     fun check_connections () =
	  if ! call_count > cl_tick then
	   (call_count := 0;
	    B.Store.map stop_passives (! old_passives);
	    B.Store.map remove_connection (! old_conns);
	    old_conns := ! conn_store;
	    old_passives := ! passive_store;
	    conn_store := B.Store.new (Protocol.hash_address,
				       Protocol.equal_address);
	    passive_store := B.Store.new (Protocol.hash_address,
					  Protocol.equal_address))
	  else
	   call_count := ! call_count + 1

     type message_info = Protocol.connection * Protocol.incoming * int
     val pending_messages = ref ([]: message_info list)

     type receive_queue =
           (Protocol.address,
	    Protocol.connection * Protocol.incoming) B.Event_Queue.T
     val pending_receives = (B.Event_Queue.new ()): receive_queue

(*
---------------------------------------------------------------------
	10.	internal handler
*)

    local 
     fun data_handler connection message =
          let fun match_connection passive =
	           match (connection, passive)
          in case B.Event_Queue.signal {queue = pending_receives,
					match = match_connection,
					value = (connection, message)} of
	        NONE => ()
	      | _ =>
		 pending_messages := rev ((connection, message, 1) ::
					  (rev (! pending_messages)))
          end

     fun status_handler _ _ = ()

    in (* local *)

     val handler = Protocol.Handler (fn c => (data_handler c,
					      status_handler c))
    end (* local *)

(*
---------------------------------------------------------------------
	11.	CL.new
 *)

     fun allocate_send (address, size) =
         (check_connections ();
	  case B.Store.look (! conn_store, address) of
	     NONE =>
	      (case B.Store.look (! old_conns, address) of
	          NONE =>
		   let val conn = Protocol.connect (address, handler)
		   in (conn_store := B.Store.add (! conn_store, address, conn);
		       Protocol.allocate_send (conn, size))
		   end
		| SOME (_, conn) =>
		   (conn_store := B.Store.add (! conn_store, address, conn);
		    old_conns := B.Store.remove (! old_conns, address);
		    Protocol.allocate_send (conn, size)))
	   | SOME (new_store, conn) =>
	      (conn_store := new_store;
	       Protocol.allocate_send (conn, size)))

(*
---------------------------------------------------------------------
	12.	CL.receive
 *)

     fun receive address =
          (case B.Store.look (! passive_store, address) of
	      NONE =>
	       (case B.Store.look (! old_passives, address) of
	           NONE =>
		    passive_store :=
		     B.Store.add (! passive_store, address,
			          Protocol.start_passive (address,
							  handler, NONE))
		 | SOME (new_store, passive) =>
		    (passive_store :=
		      B.Store.add (! passive_store, address,
				   passive);
		     old_passives := B.Store.remove (! old_passives,
						     address)))
	    | SOME _ => ();
           let fun find_match [] = (NONE, [])
		 | find_match ((conn, m, count) :: rest) =
		   if match (conn, address) then (SOME (conn, m), rest)
		   else let val (v, l) = find_match rest
		        in if count > cl_max_packet_age andalso
			      cl_discard_excess then (v, l)
			   else (v, (conn, m, count + 1) :: l)
		        end
	   in check_connections ();
	      case find_match (! pending_messages) of
	         (NONE, _) =>
		  let val (connection, message) =
		           B.Event_Queue.wait {queue = pending_receives,
					       event = address,
					       while_waiting = fn () => ()}
		  in (connection_peer connection, message)
		  end
	       | (SOME (connection, message), new_list) =>
		  (pending_messages := new_list;
		   (connection_peer connection, message))
	   end)

    end (* struct CL *)

(*
---------------------------------------------------------------------
	13.	top-level declarations of functor User
*)

   val initialize = Protocol.initialize
   val finalize = Protocol.finalize

 end (* struct *)


