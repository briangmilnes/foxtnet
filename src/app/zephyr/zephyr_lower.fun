(*

	Foxnet: The Fox Project's Communication Protocol Implementation Effort
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

	This functor implements a reliable means of communicating with
	the zephyr servers, via UDP connections. Because it is
	impossible to talk to the zhm on the local machine, it ends up
	implementing a bare bones zhm which it uses to talk to the zephyr
	servers directly.

	Since it is taking the place of the zhm, it has to manage a zhm
	connection to a zephyr server, and also handle client port
	connections. All requests to send data on a port get sent via
	the zhm to server connection, and server acknowledgments
	received on that same connection are dispatched to the correct
	port handler.

	It also schedules a thread that listens for messages from the
	server(s) addressed to a specific client port, and dispatches
	that message to the correct port handler, and sends
	CLIENTACKs to inform the server of successful delivery of a
	server message.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	iv.	Lacunae
	1.	functor Zephyr_Lower
	2.	functions to manage state
	3.	functions to send CLIENTACKs
	4.	handler to emulate zhm
	5.	udp handler for the zhm server connection
	6.	initialize
	7.	finalize
	8.	new_port
	9.	port_open
	10.	port_send
	11.	port_close

	iii.	RCS Log

$Log: zephyr_lower.fun,v $
Revision 1.1  1994/09/03  21:13:35  danwang
Initial revision


	iv.	Lacunae

	The zhm implementation is incomplete. It does not honor
	requests to attach to a new server or report statistics about
	itself. It currently connects to only one server and does not
	look for a new server if that one dies. It also does not queue
	client request for retransmition when it does not receive a
	server acknowledgement for that message.

	Fragmentation issues are not addressed anywhere in the current
	implementation, small messages under 500 bytes should go
	through fine, but larger ones will undoubtablely
	fail. Fragmentation and reassambly issues should be dealt with
	at this level, since this is the only level where packet ids
	and notice ids are visible. Port allocation is also broken.



		1.	functor Zephyr_Lower
*)
functor Zephyr_Lower(structure Udp:UDP_PROTOCOL
			 sharing type Udp.connection = Udp.address
		     structure Ip:IP_PROTOCOL
		     structure Z:ZEPHYR_EXTERN
		     structure B:FOX_BASIS
		     sharing type Udp.outgoing_message = B.Send_Packet.T
			 and type Udp.incoming_message = B.Receive_Packet.T
			 and type Ip.ip_address = Udp.lower_layer_address
		     val udp_over_ip : ubyte1
		     val zephyr_hm_port : ubyte2
		     val zephyr_srv_port: ubyte2
		     val zephyr_srv_ip: ubyte4
		     val my_ip : ubyte4
		     val do_prints : bool):ZEPHYR_LOWER =
struct
(*
	A generic exception to raise when something goes wrong.
*)
    exception Zephyr_Lower_Error of string

    val debug_print = if do_prints then fn x =>
	(B.V.print("zephyr_lower.fun: " ^ x ^"\n"); B.V.flush ())
		    else fn _=>()

    fun draise s =
	(B.V.print ("zephyr_lower.fun: exception " ^ s ^ "\n");
	 raise  Zephyr_Lower_Error s)

    datatype z_packet = Packet of
	       {kind:ubyte4,
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
	   notice_id:{ip_address:ubyte4,time_sec:ubyte4,time_usec:ubyte4},
		data:string list}

(*
	A port is simply a ubyte2 which represents some udp port on
	the local machine.
*)
    datatype port = Port of ubyte2

    type dispatcher_key = ubyte2
    type dispatcher_packet = z_packet
    type dispatcher = (dispatcher_key, dispatcher_packet) B.Dispatcher.T


(*
	2.	functions to manage state
*)

    datatype state = State of {initialization_count:int,
			       server_conn:Udp.connection,
			       client_ports:ubyte2}

    val state = ref (NONE:state option)

    fun get_state () =
	case (!state) of
	    SOME s => s
	  | NONE   => draise "Zephyr lower uninitialized."

(*
 	Internal functions that keep track of port handlers.
*)
    local
	val dispatcher = (ref (B.Dispatcher.new("zephyr_lower.fun",
						Byte2.to_int,
						op=):dispatcher))
    in
	fun dispatcher_add_handler key handler =
	    (debug_print("Installing client handler");
	     dispatcher := B.Dispatcher.dispatch(!dispatcher,key,handler))

	fun dispatcher_remove_handler key =
    	    dispatcher := B.Dispatcher.disable(!dispatcher,key)

	fun dispatcher_send_packet key packet =
	    (debug_print("Calling port handler.");
	     B.Dispatcher.send(!dispatcher,key,packet);
	     debug_print("Port handler returned."))
    end

(*
	 2.	functions to build and translate packets
*)
    fun build_unique_id () =
       let
	    val B.V.Misc.TIME {sec, usec} = B.V.Misc.gettimeofday ()
       in
	   {ip_address=my_ip,
	    time_sec=Byte4.from_int sec,
	    time_usec=Byte4.from_int usec}
       end

    fun build_hm_request_zpacket opcode =
	(Packet {kind=4u4,
		 auth=4u0,
		 authent_len=4u0,
		 authenticator=ByteArray.array(0,0),
		 class="ZEPHYR_CTL",
		 instance="HM",
		 opcode=opcode:string,
		 sender="HM",
		 recipient="",
		 default_format="",
		 checksum=4u0,
		 notice_id=build_unique_id (),
		 data=([]:string list)})

    fun receive_packet_to_zrec rp =
	let
	    val array = B.Receive_Packet.read rp
	    val (zrec,_) = Z.unmarshall array
	in
	    zrec
	end

    fun zrec_to_zpacket (Z.Zrec {kind,auth,authent_len,authenticator,class,
		 instance,opcode,sender,recipient,default_format,
		 checksum,notice_id,data,...}) =
	(Packet {kind=kind,auth=auth,authent_len=authent_len,
		     authenticator=authenticator,
		     class=class,instance=instance,opcode=opcode,
		     sender=sender,recipient=recipient,
		     default_format=default_format, checksum=checksum,
		     notice_id=notice_id,data=data})

   val receive_packet_to_zpacket = zrec_to_zpacket o receive_packet_to_zrec

    fun zpacket_to_send_packet (Port port)
	(Packet {kind,auth,authent_len,authenticator,class,
		 instance,opcode,sender,recipient,default_format,
		 checksum,notice_id,data})=
	let
	    val packet_id = build_unique_id ()
	    val data_len =
		(B.V.List.fold (fn (str,acc) =>
			       acc + (B.V.String.length str) + 1)
		               data (B.V.List.length data))

	    val frag_count = "0/" ^(makestring data_len)
	    val zrec = (Z.Zrec {version="ZEPH0.2",
				num_fields=4u17,
				kind=kind,
				packet_id=packet_id,
				port=port,
				auth=auth,
				authent_len=authent_len,
				authenticator=authenticator,
				class=class,
				instance=instance,
				opcode=opcode,
				sender=sender,
				recipient=recipient,
				default_format=default_format,
				checksum=checksum,
				frag_count=frag_count,
				notice_id=notice_id,
				data=data})
	    val size = Z.size  zrec
	    val array = Z.marshall (zrec,ByteArray.array(size,0))
	    val sp = B.Send_Packet.create(array,50)
	in
	    sp
	end

(*
	3.	functions to send CLIENTACKs
*)

(*
 	This is not documented in the zephyr specification, but
	careful examination of the code for libzephyr.a makes it clear
	that on receiving a message from the server, except for
	hostmanager and server acks and nacks, all clients should
	send a CLIENTACK message back to the server.
*)
    fun send_ack conn (Z.Zrec {version,num_fields,kind,packet_id,port,
			       auth,authent_len,authenticator,
			       class,instance,opcode,sender,recipient,
			       default_format,checksum,frag_count,notice_id,
			       data}) =
	case kind of
	    4u3 => () (* HMACK *)
	  | 4u5 => () (* SERVACK *)
	  | 4u6 => () (* SERVNACK *)
	  | 4u7 => () (* CLIENTACK *)
	  | _ => let
		     val zrec = (Z.Zrec {version=version,
					 num_fields=num_fields,
					 kind=4u7, (* client ack code *)
					 packet_id=packet_id,
					 port=port,
					 auth=auth,
					 authent_len=authent_len,
					 authenticator=authenticator,
					 class=class,
					 instance=instance,
					 opcode=opcode,
					 sender=sender,
					 recipient=recipient,
					 default_format=default_format,
					 checksum=checksum,
					 frag_count=frag_count,
					 notice_id=notice_id,
					 data=([]:string list)})
		     val size = Z.size zrec
		     val array = Z.marshall (zrec,ByteArray.array(size,0))
		     val sp = B.Send_Packet.create(array,50)
		 in
		     Udp.send conn sp
		 end

(*
	This function relies on the implementation of udp connections.
*)
    fun connection_to_local_port
	((Udp.Address {local_port,...}):Udp.connection) = local_port


(*
	4.	handler to emulate zhm
*)
    fun host_manager_handler
	(packet as (Packet{class,instance,opcode,data,...})) =
	let
	    val _ = debug_print("Dispatching on hm packet")
	    val _ =
		case class of
		    "HM_CTL" =>
			(case instance of
			     "SERVER" =>
			     (case opcode of
				  "SHUTDOWN" =>
				      debug_print("Server shutting down.")
				| "PING" =>
				      debug_print("Got a server ping.")
				| _ =>
				      debug_print("Got unknown opcode: "
						  ^opcode))
			   | "CLIENT" =>
				 (case opcode of
				      "FLUSH" =>
					  debug_print("Got a client flush.")
				    | "NEWSERVER" =>
					  debug_print("Got a find server msg.")
				    | _ =>
					  debug_print("Got unknown opcode: "
						      ^opcode))
			   | _ =>
				 debug_print("Got unknow instance: "
					     ^ instance))
		  | _ =>
			debug_print("Got unknown class: "
				    ^ class)
	in
	    ()
	end
(*
	5.	udp handler for the zhm server connection
*)

    fun dispatch_packet conn rp =
	let
	    val array = B.Receive_Packet.read rp
	    val (zrec,_) = Z.unmarshall array
	    val (Z.Zrec {kind,auth,authent_len,authenticator,class,
		 instance,opcode,sender,recipient,default_format,
		 checksum,notice_id,data,port,...}) = zrec
	    val zpacket =
		(Packet {kind=kind,auth=auth,authent_len=authent_len,
			 authenticator=authenticator,
			 class=class,instance=instance,opcode=opcode,
			 sender=sender,recipient=recipient,
			 default_format=default_format,
			 checksum=checksum,notice_id=notice_id,data=data})
	    val local_port = connection_to_local_port conn

(*
	If the udp local port of this message matches the zhm's and the
	port field of the zephyr messages is not aimed at some client
	port, dispatch this to the zhm packet handler. Otherwise dispatch to
	the appropriate client port handler.

	Send a CLIENTACK acknowledgement back to the server to let it know we
	got the message also.
*)
	    val port =
		if local_port = zephyr_hm_port
		andalso port <> zephyr_hm_port
		    then port
		else
		    local_port
	in
	    (send_ack conn zrec; dispatcher_send_packet port zpacket;
	     ())
	end

(*
	 6.	functions related to the server connection
*)

    fun send_to_srv sp =
	let
	    val (State {server_conn,...}) = get_state()
	in
	    Udp.send server_conn sp
	end

    fun establish_srv_conn () =
	let
	    val _ = Udp.initialize ()
	    val server_address =
 		Udp.Address {remote_peer =
			     Ip.Address {ip=zephyr_srv_ip, proto=udp_over_ip},
			     local_port  = zephyr_hm_port,
			     remote_port = zephyr_srv_port}
	    val server_conn = Udp.active_open(server_address,dispatch_packet)
	    val _ = dispatcher_add_handler zephyr_hm_port host_manager_handler
	    val boot_msg =
		zpacket_to_send_packet (Port zephyr_hm_port)
		(build_hm_request_zpacket "BOOT");
	in
	    Udp.send server_conn boot_msg;
	    server_conn
	end

    fun close_srv_conn () =
	let
	    val (State {server_conn,...}) = get_state()
	in
	    (send_to_srv o
	     (zpacket_to_send_packet (Port zephyr_hm_port)) o
	     build_hm_request_zpacket) "FLUSH";
	    Udp.close server_conn;
	    Udp.finalize ()
	end

(*
	6.	initialize
*)
    fun initialize () =
	let
	    val s =
		case (!state) of
		    SOME s => s
		  | NONE  => (State {initialization_count=0,
				     server_conn=establish_srv_conn(),
				     client_ports=2u32000})

	    val (State {initialization_count,
			server_conn,client_ports}) = s

	    val initialization_count = initialization_count + 1
	in
	    state := (SOME (State
			     {initialization_count=initialization_count,
			      server_conn=server_conn,
			      client_ports=client_ports}));
	    initialization_count
	end

(*
	7.	finalize
*)
    fun finalize () =
	let
	    val (State {initialization_count,server_conn,...}) = get_state ()
	    val initialization_count = initialization_count - 1
	in
	    if (initialization_count = 0) then
		(close_srv_conn ();
		 state := NONE)
	    else ();
		initialization_count
	end


(*
	Most of this code is broken to some extent but works in the
	most common cases. This is a basic attempt to implement
	functionality that the current udp signatures lacks i.e.
	"get_a_random_udp_connection_and_its_local_port_number"
	function. The newer udp signature should fix the problem.

*)


(*
	8.	new_port
*)

(*
	This code is broken, since I do not check if the port I have
	just allocated is actually in use. The new udp signature should clean
	this up and fix the problem.

	But in most cases the udp ports I allocate from are not likely to
	be in use.
*)

    fun new_port () =
	let
	    val (State {initialization_count,
			client_ports,server_conn}) = get_state()
	    val port = Byte2.+(client_ports,2u1)
	in
	    state := (SOME (State
			    {initialization_count=initialization_count,
			     server_conn=server_conn,
			     client_ports=port}));
	    	    (Port port)
	end

(*
	9.	port_open
*)

    fun port_open (Port port) handler =
	(let
	     val client_addr = (Udp.Pattern {local_port=port,
					     remote=NONE})

	     val _ = dispatcher_add_handler port handler

	    fun packet_handler conn receive_packet =
		let
		    val zrec = receive_packet_to_zrec receive_packet
		    val zpacket = zrec_to_zpacket zrec
		    val port = connection_to_local_port conn

		in
		    (send_ack conn zrec; dispatcher_send_packet port zpacket;
		     ())
		end

	    fun listener_thread () =
		(Udp.passive_open (client_addr,packet_handler);
		 listener_thread ();
		 ())

	 in
	     B.Scheduler.fork (B.Scheduler.Normal listener_thread);
	     true
	end handle _ => false)

(*
	10.	port_send
*)
    fun port_send (Port port) zpacket =
	(let
	    val _ = send_to_srv (zpacket_to_send_packet (Port port)
				 zpacket)
	in
	    true
	end handle _ => false)

(*
	11.	port_close
*)

(*
	I probably should close the udp port but I cannot get a hold of
 	the udp connection from the port number. Since I only get a
	udp connection when the passive open completes, and I have to listen
	passively since I might receive messages from different zephyr
	servers running on different hosts, closing the port becomes a
	mess.

	The "Right Thing" to do would be to maintain a store that maps
	udp local port numbers to connections which the udp handlers
	update once they get hold of a connection, but that seems to
	be more work than needed to fix a problem that the new udp
	signature should fix.

	The quick hack is to just remove the packet handler from the
	dispatch table, and ignore the messages.
*)
    fun port_close (Port port) =
	(dispatcher_remove_handler port; true)
end
