(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robby Findler (Robert.Findler@cs.cmu.edu)
	Daniel Wang (Daniel.Wang@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	A higher level interface to the zephyr protocol, it keeps
	track of outgoing messages and incoming server
	acknowledgements with an event queue and a data pipe for
	synchronization. A primitive zephyr RPC is also implemented.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor ZEPHYR
	2.	handler to deal with incoming packets
	3.	initialize
	4.	finalize
	5.	new_client
	6.	client_call
	7.	client_send
	8.	kill_client


	iii.	RCS Log

$Log: zephyr.fun,v $
Revision 1.1  1994/09/03  21:14:36  danwang
Initial revision


	iv. Lacunae

	Send and call functions should timeout if they do not receive
	the appropriate acknowledgement after some time and return
	NONE.

		1.	functor ZEPHYR
*)

functor Zephyr (structure B:FOX_BASIS
		structure Z:ZEPHYR_LOWER
		val my_ip : ubyte4
		val do_prints : bool):ZEPHYR =
struct
    exception Zephyr_Error of string

    val debug_print=if do_prints then fn x =>
	(B.V.print("zephyr.fun: " ^ x ^"\n"))
		    else fn _=>()
    fun draise s =
	(B.V.print ("zephyr.fun: exception " ^ s ^ "\n");
	 raise  Zephyr_Error s)

    datatype message_kind =
	UNSAFE
      | UNACKED
      | ACKED
      | SERVACK
      | SERVNACK
      | CLIENTACK
      | STAT

    datatype z_notice =	Notice of
	   {kind:message_kind,
	    auth:ubyte4,
     authent_len:ubyte4,
   authenticator:ByteArray.bytearray,
	   class:string,
        instance:string,
	  opcode:string,
	  sender:string,
        recipient:string,
  default_format:string,
        checksum:ubyte4,
	    data:string list}

(*
	A reference to keep track of initialization and finalization counts.
*)
    val initialization_count = ref 0

    datatype client = Client of Z.port

    type z_packet = Z.z_packet

(*
     	This event queue uses the unique notice id of each zephyr message
	and its kind tag to match outgoing messages with their server
	responses. The server uses notice ids as transaction
	identifiers. Where a transaction is defined to be an
	acknowledgment of message transmission or a reply to a query
	for data. All messages associated with a transaction share the
	same unique notice id. The kind list specifies a union of
	message kinds with a given notice id which a waiting event is
	interested in being notified about.
*)
    type event_queue_id = {kind_list:message_kind list,
			   notice_id:{ip_address:ubyte4,
				      time_sec:ubyte4,
				      time_usec:ubyte4}}

    type event_queue_val = z_notice
    type event_queue = (event_queue_id,event_queue_val) B.Event_Queue.T

    val queue = (B.Event_Queue.new ():event_queue)

(*
	When signaling an event there is no guarantee the waiting
	thread will wake up immediately. So force this to occur by
	using a data pipe to synchronize threads, by doing a blocking
	enqueue operation when signaling. Waiting threads are
	responsible for unblocking the calling thread by dequeuing from
	the pipe.
*)
    type sync_pipe = unit B.Pipe.T
    val sync_pipe = (B.Pipe.new NONE:sync_pipe)

    fun wait_call_done () = (B.Pipe.dequeue sync_pipe)
    fun signal_call_done () = (B.Pipe.enqueue(sync_pipe,()))

(*
	 1.	functions to build and translate packets
*)

    fun build_unique_id () =
	let
	    val B.V.Misc.TIME {sec, usec} = B.V.Misc.gettimeofday ()
	in
	    {ip_address=my_ip,
	     time_sec=Byte4.from_int sec,
	     time_usec=Byte4.from_int usec}
	end

    fun decode_kind 4u0 = UNSAFE
      | decode_kind 4u1 = UNACKED
      | decode_kind 4u2 = ACKED
      | decode_kind 4u5 = SERVACK
      | decode_kind 4u6 = SERVNACK
      | decode_kind 4u7 = CLIENTACK
      | decode_kind 4u8 = STAT
      | decode_kind _ = draise("Got an unknown message kind")

    fun encode_kind UNSAFE    = 4u0
      | encode_kind UNACKED   = 4u1
      | encode_kind ACKED     = 4u2
      | encode_kind SERVACK   = 4u5
      | encode_kind SERVNACK  = 4u6
      | encode_kind CLIENTACK = 4u7
      | encode_kind STAT      = 4u8

    fun packet_to_notice (Z.Packet {kind,auth,authent_len,authenticator,
				    class,instance,opcode,sender,
				    recipient,default_format,checksum,
				    data,...}) =
	(Notice {kind=(decode_kind kind),auth=auth,
		 authent_len=authent_len,authenticator=authenticator,
		 class=class,instance=instance,opcode=opcode,sender=sender,
		 recipient=recipient,default_format=default_format,
		 checksum=checksum,data=data})

    fun notice_to_packet (Notice {kind,auth,authent_len,authenticator,
				  class,instance,opcode,sender,
				  recipient,default_format,checksum,
				  data}) =
	(Z.Packet {kind=(encode_kind kind),auth=auth,
		   authent_len=authent_len,authenticator=authenticator,
		   class=class,instance=instance,opcode=opcode,sender=sender,
		   recipient=recipient,default_format=default_format,
		   checksum=checksum,notice_id=(build_unique_id()),
		   data=data})

(*
	2.	handler to deal with incoming packets
*)


(*
	Whenever we receive a message from the server, see it there is a
	waiting event for that message. If there is an event signal
	the event and synchronize with the waiting thread via a data
	pipe, otherwise pass the message to the client installed
	handler.
*)

    fun dispatch_packet handler (z_packet as (Z.Packet {notice_id,kind,...})) =
	let
	    val z_notice = packet_to_notice z_packet
	    val kind = decode_kind kind
	    val _ = debug_print(" Got a packet.")

	    fun check_kind_list [] = false
	      | check_kind_list (x::xs) = (x = kind)
		orelse (check_kind_list xs)

	    fun check_notice_id id = (id = notice_id)

	    fun check_event {kind_list,notice_id} =
		(check_notice_id notice_id)
		andalso (check_kind_list kind_list)

	    fun check_queue ()  =
		(debug_print("Checking event queue.");
		 B.Event_Queue.signal(queue,check_event,z_notice))
	in
	    case (check_queue()) of
		NONE => (debug_print("Didn't find a waiting event.");
			 handler z_notice)
	      | SOME _ => (debug_print("Found a waiting event.");
			   wait_call_done())
	end

(*
	3.	initialize
*)

    fun initialize () =
	((if ((!initialization_count) = 0) then
	      (B.Event_Queue.clear queue;
	       Z.initialize ();
	       inc initialization_count)
	  else
	      inc initialization_count);
	      (!initialization_count))

(*
	4.	finalize
*)

    fun finalize () = (dec initialization_count;
		       (if ((!initialization_count) = 0) then
			    (Z.finalize();())
			else ());
			(!initialization_count))
(*
	5.	new_client
*)

    fun new_client handler =
	let
	    val port = Z.new_port ()
	in
	    if (Z.port_open port (dispatch_packet handler)) then
		(Client port)
	    else draise("Error installing client handler.")
	end

(*
	6.	client_call
*)

(*
	When doing a "client_call" we are expecting two messages from
	the server. An ack or nack that informs us of the delivery of
	the request, and a "acked" message with the actual reply.

	Since udp packets may be delivered in any order, things get a
	bit tricky, and we resort to building a trivial finite state
	machine, which makes state transitions based on the kind tag
	of the messages fed to it as input.
*)

    fun client_call (Client port) notice =
	let
	    val zpacket = notice_to_packet notice
	    val (Z.Packet {notice_id,...}) = zpacket
	    val event_id = {kind_list=[ACKED,SERVACK,SERVNACK],
			    notice_id=notice_id}
	    fun send () = (Z.port_send port zpacket ; ())
	    fun send_wait () = B.Event_Queue.wait(queue,event_id,send)
	    fun wait_reply () = B.Event_Queue.wait(queue,event_id,
						   signal_call_done)

	    datatype state =
		INITIAL | WAIT_FOR_SERVACK | WAIT_FOR_ACK | FINAL

	    fun fsm (INITIAL,(reply as (Notice {kind,...})),result) =
		(debug_print("Initial state");
		 (case kind of
		      SERVACK =>
			  fsm(WAIT_FOR_ACK,
			      wait_reply(),result)
		    | ACKED =>
			  fsm(WAIT_FOR_SERVACK,
			      wait_reply(),SOME reply)
		    | _ => draise("Got unknown message during zephyr call.")))

	      | fsm (WAIT_FOR_ACK,(reply as (Notice {kind,...})),result) =
		(debug_print("Waiting for reply ack state.");
		 (case kind of
		      SERVACK =>
			  fsm(WAIT_FOR_ACK,
			      wait_reply(),result)
		    | ACKED =>
			  fsm(FINAL,
			      reply,SOME reply)
		    | _ => draise("Got unknown message waiting for ACK.")))

	      | fsm (WAIT_FOR_SERVACK,(reply as (Notice {kind,...})),result) =
		(debug_print("Waiting for server ack state.");
		 (case kind of
		      SERVACK =>
			  fsm(FINAL,
			      reply,result)
		    | ACKED =>
			  fsm(WAIT_FOR_SERVACK,
			      wait_reply(),result)
		    | _ => draise("Got unknown message waiting for SERVACK.")))

	      | fsm (FINAL,_,NONE) =
		(signal_call_done();
		 draise("Didn't get a reply."))

	      | fsm (FINAL,_,SOME reply) =
		(signal_call_done();
		 debug_print("Final state got reply");
		 SOME reply)
	in
	    fsm(INITIAL,send_wait(),NONE)
	end

(*
	7.	client_send
*)

(*
	Client sends are simple. We just install send a message and
	wait for an ack or a nack from the server.
*)

    fun client_send (Client port) (notice as (Notice {kind,...})) =
	let
	    val zpacket = notice_to_packet notice
	    val (Z.Packet {notice_id,...}) = zpacket
	    val event_id = {kind_list=[SERVACK,SERVNACK],
			    notice_id=notice_id}
	    fun send () = (Z.port_send port zpacket ; ())
	    fun send () = (Z.port_send port zpacket ; ())
	in
	    case kind of
		UNSAFE => (send();NONE)
	      | UNACKED => (send();NONE)
	      | ACKED =>
		    let
			val ret_val =
			    B.Event_Queue.wait(queue,event_id,send)
			val _ = signal_call_done()
		    in
			SOME ret_val
		    end
	      | _ => draise("Got unknown message during send.")
	end

(*
	8.	kill_client
*)

(*
	There should also be a way to remove any events in the event
	queue that a client may still be waiting on. Perhaps the best
	solutions is to have a unique event queue per client.
*)
    fun kill_client (Client port) =
       (Z.port_close port;
	())

end



