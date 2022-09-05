(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	An implementation of UDP.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Udp
	2.	miscellaneous types
	3.	structure Transport_Setup
	4.	structures Udp_Key, Udp_Address, and Udp_Pattern
	5.	extension types
	6.	miscellaneous structures (shared with Lower).
	7.	miscellaneous functions
	8.	structure Connection
	9.	export objects declared in signature

		iii.	RCS Log
	
$Log: udp.fun,v $
Revision 1.61  1996/06/07  20:19:59  cline
fixed usage of before

Revision 1.60  1996/03/12  22:26:31  esb
now sends ICMP port_unreachable.

Revision 1.59  1996/02/23  21:33:49  esb
adated to new NETWORK_PROTOCOL.

Revision 1.58  1996/02/15  19:01:44  esb
standardized Udp_Setup to Transport_Setup.

Revision 1.57  1996/01/19  23:04:07  esb
adapted to the new wordarray signature.

Revision 1.56  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.55  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.54  1995/10/18  01:20:03  esb
inserted the latest connection functor.

Revision 1.53  1995/10/13  15:53:05  cstone
Added Host_Id structure

Revision 1.52  1995/10/04  18:57:28  esb
added new conn functor.

Revision 1.51  1995/10/02  16:19:11  cline
fixed exception handling in connect_lower

Revision 1.50  1995/09/19  18:49:15  esb
changes in some print statements.

Revision 1.49  1995/09/15  16:40:39  cline
work around for representation analysis bug

Revision 1.48  1995/09/14  21:12:11  cline
work around for representation bug

Revision 1.47  1995/09/13  15:31:15  esb
adapted identify to new conn.fun.

Revision 1.46  1995/08/08  18:23:55  esb
separated in and out external structures.

Revision 1.45  1995/07/21  12:51:19  esb
adapted to new conn.fun and improved local port selection.

Revision 1.44  1995/07/14  19:05:25  cline
added code to keep track of protocol state's received_count

Revision 1.43  1995/07/05  17:53:08  cline
use Transport_Ids functor.

Revision 1.42  1995/07/04  00:10:48  esb
made checksums work.

Revision 1.41  1995/06/28  21:12:14  cline
Udp built using the Connection functor. Checksums not yet supported.

		1.	functor Udp
*)

functor Udp (structure Lower: NETWORK_PROTOCOL
	     structure B: FOX_BASIS
	     val compute_checksums: bool
	     val udp_over_ip: Lower.Protocol_Id.T
	     val debug_level: int ref option): UDP_PROTOCOL = 
 struct

  val zero16 = Word16.fromInt 0
  val one64 = Word64.fromInt 1

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "udp.fun"
			   val makestring = Lower.X.makestring)

  val pseudo_check_in = Lower.Network_Incoming.pseudo_header_checksum
  structure Header = Udp_Header (structure In = Lower.Incoming
				 structure Out = Lower.Outgoing
				 structure B = B
				 val compute_checksums = compute_checksums
				 val pseudo_check_in = pseudo_check_in
				 val debug_level = debug_level)

(*
	2.	miscellaneous types
*)

  type protocol_state = {sent_count:        Word64.word ref,
			 received_count:    Word64.word ref,
			 failed_send_count: Word64.word ref,
			 rejected_count:    Word64.word ref}
  type connection_state = {protocol: protocol_state,
			   pseudo_header_sum: Lower.Outgoing.T -> Word16.word}

(*
	3.	structure Transport_Setup
*)

  structure Transport_Setup = Lower.Network_Setup

(*
	4.	structures Udp_Key, Udp_Address, and Udp_Pattern
*)

(*  local *)
   structure Ids = Transport_Ids (structure Lower = Lower)
(*  in *)
   structure Udp_Key = Ids.Key
   structure Udp_Address = Ids.Address
   structure Udp_Pattern = Ids.Pattern
   structure Udp_Host_Id = Ids.Host_Id
   type host_id = Ids.Host_Id.T
   type port = Ids.port
(*  end *)

  type address = Udp_Address.T
  type pattern = Udp_Pattern.T

(*
	5.	extension types
*)

  type additional_listen_extension = unit
  datatype transport_listen_extension =
    Listen_Extension of {local_port: port,
			 additional: additional_listen_extension}
  datatype udp_session_extension =
    Session_Extension of {packets_sent: unit -> Word64.word,
			  packets_received: unit -> Word64.word,
			  failed_sends: unit -> Word64.word,
			  packets_rejected: unit -> Word64.word}

  type connection_extension = unit
  type listen_extension = transport_listen_extension
  type session_extension = udp_session_extension

(*
	6.	miscellaneous structures (shared with Lower).
*)

  structure Udp_Status = Lower.Status
  structure Udp_Count = Lower.Count
  structure Udp_X = Lower.X
  structure Outgoing = Lower.Outgoing
  structure Incoming = Lower.Incoming

(*
	7.	miscellaneous functions
*)

  fun lower_setup setup = setup

  fun init_proto (setup, lower_session, call_status) = 
       let val zero16 = Word16.fromInt 0
	   val zero64 = Word64.fromInt 0

	   val sent_count        = ref zero64
	   val received_count    = ref zero64
	   val failed_send_count = ref zero64
	   val rejected_count    = ref zero64

	   val protocol_state = 
	        {sent_count        = sent_count,
		 received_count    = received_count,
		 failed_send_count = failed_send_count,
		 rejected_count    = rejected_count}

	   val session_extension = 
	        Session_Extension
		  {packets_sent     = fn () => ! sent_count,
		   packets_received = fn () => ! received_count,
		   failed_sends     = fn () => ! failed_send_count,
		   packets_rejected = fn () => ! rejected_count}
       in (protocol_state, session_extension)
       end

  fun fin_proto _ = ()

  fun resolve (_, Udp_Address.Complete {peer, ...}) = 
	SOME (Lower.Network_Address.Address {peer = peer, proto = udp_over_ip})
    | resolve (_, Udp_Address.Remote_Specified {peer, ...}) = 
	SOME (Lower.Network_Address.Address {peer = peer, proto = udp_over_ip})

  (* new_port is supposed to choose an unused local port for
     a Remote_Specified address *)
  local
   val one16 = Word16.fromInt 1
   val first_port = Word16.fromInt 1024	(* RFC 1700, p. 16 *)
   val last_port = Word16.fromInt 0xffff
   val current_port = ref first_port

   fun fold_same _ (_, true) = true
     | fold_same match (key, false) = match key

   fun same_addr port (Udp_Key.Key {local_port, ...}) = port = local_port

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

  fun make_key (_, Udp_Address.Complete addr, _, _) = Udp_Key.Key addr
    | make_key (_, Udp_Address.Remote_Specified {peer, remote_port}, _, info) =
       Udp_Key.Key {peer = peer, remote_port = remote_port,
		    local_port = new_port info}

  fun map_pattern (_, Udp_Pattern.Complete {peer, local_port, ...}, _) = 
	SOME (Listen_Extension {local_port = local_port, additional = ()},
	      Lower.Network_Pattern.Complete {peer = peer,
					      proto = udp_over_ip})
    | map_pattern (_, Udp_Pattern.Remote_Specified {peer, ...}, info) = 
	SOME (Listen_Extension {local_port = new_port info, additional = ()},
	      Lower.Network_Pattern.Complete {peer = peer,
					      proto = udp_over_ip})
    | map_pattern (_, Udp_Pattern.Local_Specified {local_port}, _) = 
	SOME (Listen_Extension {local_port = local_port, additional = ()},
	      Lower.Network_Pattern.Partial {proto = udp_over_ip})
    | map_pattern (_, Udp_Pattern.Unspecified, info) = 
	SOME (Listen_Extension {local_port = new_port info, additional = ()},
	      Lower.Network_Pattern.Partial {proto = udp_over_ip})

  fun match (_, Udp_Pattern.Complete a1, _, a2) = 
       Udp_Key.equal (Udp_Key.Key a1, a2)
    | match (_, Udp_Pattern.Remote_Specified {peer = p1, remote_port = r1},
	     Listen_Extension {local_port = l1, additional},
	     Udp_Key.Key {peer = p2, remote_port = r2, local_port = l2}) = 
       Lower.Host_Id.equal (p1, p2) andalso r1 = r2 andalso l1 = l2
    | match (_, Udp_Pattern.Local_Specified {local_port = l1}, _,
	     Udp_Key.Key {local_port = l2, ...}) = 
       l1 = l2
    | match (_, Udp_Pattern.Unspecified,
	     Listen_Extension {local_port = l1, additional},
	     Udp_Key.Key {local_port = l2, ...}) = 
       l1 = l2

  fun init_connection (protocol_state, _, lower_conn) =
       let val Lower.C {extension, ...} = lower_conn
	   val Lower.Connection_Extension
	         {pseudo_header_checksum, ...} = extension
       in ({protocol = protocol_state,
	    pseudo_header_sum = pseudo_header_checksum}, ())
       end

  fun fin_connection state = ()

  fun send (Udp_Key.Key {peer, local_port, remote_port},
	    {protocol = {sent_count, ...},
	     pseudo_header_sum, ...}: connection_state) packet = 
      let val header = Header.Outgoing_Header
		        {src = local_port, dest = remote_port, data = packet,
			 pseudo_header_sum = pseudo_header_sum}
	  val packet_header = Outgoing.uninitialized (Header.size header)
	  val _ = Header.marshal (packet_header, header) 0w0;
          val lower_packet = Outgoing.join (packet_header, packet)
      in sent_count := Word64.+ (! sent_count, one64);
	 [lower_packet]
      end

    fun identify (lower_key, _) packet =
         ((let val address = Lower.key_to_address lower_key
	       val Lower.Network_Address.Address {peer, proto} = address
	       val {src, dest} = Header.identify (packet, 0w0)
	   in [Udp_Key.Key {peer = peer, local_port = dest, remote_port = src}]
	   end)
	    handle Header.Extern => [])

  fun receive (key, {protocol = {received_count, ...}, ...}: connection_state)
              packet = 
      case Header.unmarshal (packet, 0w0) of
	(Header.Incoming_Header {src, dest, data}, position) =>
	  (received_count := Word64.+ (! received_count, one64);
	   SOME data)
      | _ => let exception Udp_Impossible in raise Udp_Impossible end

  fun undelivered (_, _) (lower_conn, data) =
       let val Lower.C {extension, ...} = lower_conn
	   val Lower.Connection_Extension {port_unreachable, ...} = extension
       in port_unreachable data
       end

  fun lower_status (state, lower_key) status = 
       Trace.local_print ("received lower status " ^
			  Lower.Status.makestring status)

(*
	8.	structure Connection
*)
  structure Connection = 
      Connection (structure Lower = Lower
		  structure Setup = Transport_Setup
		  structure Address = Udp_Address
		  structure Pattern = Udp_Pattern
		  structure Connection_Key = Udp_Key
		  structure Incoming = Lower.Incoming
		  structure Outgoing = Lower.Outgoing
		  structure Status = Udp_Status
		  structure Count = Udp_Count
		  structure X = Udp_X
		  type connection_extension = connection_extension
		  type listen_extension = listen_extension
		  type session_extension = session_extension
		  type connection_state = connection_state
		  type protocol_state = protocol_state
		  val lower_setup = lower_setup
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
		  val module_name = "udp.fun"
		  val debug_level = debug_level)

(*
	9.	export objects declared in signature
*)

  open Connection

  structure Transport_Address = Udp_Address
  structure Transport_Pattern = Udp_Pattern
  structure Transport_Key = Udp_Key
  structure Host_Id = Udp_Host_Id

 end (* struct *)


