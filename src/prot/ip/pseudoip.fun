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

	pseudoip.fun: a layer to permit running transport protocols without IP


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Pseudo_Ip
	2.	structure Host_Id
	3.	structure Protocol_Id
	4.	structure Network_Setup
	5.	structure Network_Address
	6.	structure Network_Pattern
	7.	structures Network_Incoming and Network_Outgoing
	8.	structure Network_Status
	9.	required extensions
	10.	structures from lower protocol
	11.	local declarations
	12.	structure Length_Extern
	13.	session


	iii.	RCS Log

$Log: pseudoip.fun,v $
Revision 1.29  1997/04/22  11:21:30  esb
got rid of warning about value restriction.

Revision 1.28  96/07/22  20:10:14  cline
*** empty log message ***

Revision 1.27  1996/04/18  21:21:05  cline
converted hash from into to word

Revision 1.26  1996/03/12  22:24:31  esb
added port_unreachable.

Revision 1.25  1996/02/23  21:08:27  esb
added key_to_address, as required by the new network.sig

Revision 1.24  1996/01/19  23:02:29  esb
adapted to the new wordarray signature.

Revision 1.23  1996/01/16  22:38:59  cline
*** empty log message ***

Revision 1.22  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.21  1995/11/12  16:33:39  esb
adapted to lower setup type being string.

Revision 1.20  1995/10/02  21:21:37  esb
layered on top of ethernet instead of ARP.

Revision 1.19  1995/09/26  16:25:26  esb
adapted to new network.sig.

Revision 1.18  1995/09/14  21:09:51  cline
work around for representation bug

Revision 1.17  1995/09/13  15:30:46  esb
added parsing of lower addresses.

Revision 1.16  1995/08/29  14:14:54  esb
version that includes IP and ICMP.

Revision 1.15  1995/08/24  00:51:35  esb
matches new network.sig

Revision 1.14  1995/08/08  18:23:28  esb
adapted to new extern with separate in and out.

Revision 1.13  1995/06/29  19:54:29  esb
adapted to new network.sig.

Revision 1.12  1995/06/27  21:45:41  esb
adapted to new network.sig.

Revision 1.11  1995/06/27  19:01:53  cline
adapted to new extern.sig

Revision 1.10  1995/06/23  19:56:23  esb
adapted to new network.sig

Revision 1.9  1995/06/20  17:02:17  esb
adapted to new protocol signature.

Revision 1.8  1995/03/24  01:46:10  esb
adapted to new ip.sig.

Revision 1.7  1995/03/07  20:36:35  esb
updated for new Addressing scheme.

Revision 1.6  1995/02/04  20:40:05  robby
updated to 107

Revision 1.5  1995/01/18  21:00:57  esb
renamed EmptyIcmp to Empty_Icmp.

Revision 1.4  1994/11/07  21:33:00  cline
use V.Print

Revision 1.3  1994/10/27  20:24:41  cline
added SML/NJ 105 compatibility

Revision 1.2  1994/10/26  15:58:02  esb
added comments.

Revision 1.1  1994/10/25  16:36:11  esb
Initial revision

	1.	functor Pseudo_Ip

	This functor takes an address resolution protocol, and
	returns a network protocol.
*)

functor Pseudo_Ip (structure Lower: ETHERNET_PROTOCOL
		   structure B: FOX_BASIS): NETWORK_PROTOCOL =
 struct

(*
	2.	structure Host_Id
*)

  structure Host_Id =
   struct
    open Lower.Eth_Number

    val zero = Word48.fromInt 0
    val ten = Word48.fromInt 10
    val byte_range = Word48.fromInt 256
    exception Bad_Parse
    fun parse_digit (#"0") = Word48.fromInt 0
      | parse_digit (#"1") = Word48.fromInt 1
      | parse_digit (#"2") = Word48.fromInt 2
      | parse_digit (#"3") = Word48.fromInt 3
      | parse_digit (#"4") = Word48.fromInt 4
      | parse_digit (#"5") = Word48.fromInt 5
      | parse_digit (#"6") = Word48.fromInt 6
      | parse_digit (#"7") = Word48.fromInt 7
      | parse_digit (#"8") = Word48.fromInt 8
      | parse_digit (#"9") = Word48.fromInt 9
      | parse_digit _ = raise Bad_Parse
    fun parse_one ([], acc) = (acc, [])
      | parse_one (#"." :: rest, acc) = (acc, rest)
      | parse_one (#":" :: rest, acc) = (acc, rest)
      | parse_one (digit :: rest, acc) =
	 parse_one (rest, Word48.+ (Word48.* (acc, ten),
				       parse_digit digit))
    fun do_parse [] = zero
      | do_parse string =
         let val (result, rest) = parse_one (string, zero)
	 in case rest of
	       [] => result
	     | _ =>
		Word48.+ (Word48.* (result, byte_range),
			     do_parse rest)
	 end
    fun default_parse string =
         ((SOME (Lower.Eth_Number.new (do_parse (B.V.String.explode string))))
	  handle Bad_Parse => NONE)
    val parser = ref default_parse
    fun parse s = (case default_parse s of
		     SOME i => SOME i
		   | NONE => (!parser) s)
    fun install_parse p = parser := p
   end (* struct *)

(*
	3.	structure Protocol_Id
*)

  structure Protocol_Id = Lower.Eth_Protocol

(*
	4.	structure Network_Setup
*)

  structure Network_Setup =
   struct
    type host_id = Host_Id.T
    datatype setup = Setup of {local_id: host_id, interface: string,
			       gateways: host_id list, mtu: int option,
			       mask: (host_id * {serve: bool}) option} list
    type T = setup
    fun makestring_gateways [] = ""
      | makestring_gateways [last] = "g" ^ Host_Id.makestring last
      | makestring_gateways (first :: rest) =
	 "g" ^ Host_Id.makestring first ^ ", " ^ makestring_gateways rest
    fun makestring (Setup []) = ""
      | makestring (Setup
		    ({local_id, interface, gateways, mask, mtu} :: rest)) =
         interface ^ " (" ^ Host_Id.makestring local_id ^ ")" ^
	 makestring_gateways gateways ^
	 (case mask of
	     NONE => ""
	   | SOME (m, {serve}) =>
	      (", mask " ^ Host_Id.makestring m ^ " (" ^
	       (if serve then "" else "do not ") ^ "serve)")) ^
	 (case mtu of
	     NONE => ""
	   | SOME m => ", mtu " ^ Integer.toString m) ^
	 makestring (Setup rest)
    fun equal (Setup [], Setup []) = true
      | equal (Setup ({local_id = id1, interface = if1, gateways = gw1,
		       mask = m1, mtu = mtu1} :: r1),
	       Setup ({local_id = id2, interface = if2, gateways = gw2,
		       mask = m2, mtu = mtu2} :: r2)) =
         Host_Id.equal (id1, id2) andalso if1 = if2
				  andalso equal (Setup r1, Setup r2)
      | equal _ = false
    fun hash (Setup []) = 0w0
      | hash (Setup ({local_id, interface, gateways, mask, mtu} :: rest)) =
         Host_Id.hash local_id + hash (Setup rest)
   end (* struct *)

  structure Setup = Network_Setup

(*
	5.	structure Network_Address
*)

  structure Network_Address =
   struct
    type host_id = Host_Id.T
    type protocol_id = Protocol_Id.T
    (* parameterize address to work around SML/NJ's representation bug *)
    datatype 'a internal_address = Address of {peer: 'a,
					       proto: protocol_id}
    type address = host_id internal_address
    type T = address
    fun makestring (Address {peer, proto}) = 
         Host_Id.makestring peer ^ "/" ^ Protocol_Id.makestring proto
    fun equal (Address {peer = p1, proto = proto1},
	       Address {peer = p2, proto = proto2}) =
         Host_Id.equal (p1, p2) andalso Protocol_Id.equal (proto1, proto2)
    fun hash (Address {peer, proto}) =
         Host_Id.hash peer + Protocol_Id.hash proto
   end (* struct *)
  structure Address = Network_Address
  structure Connection_Key = Network_Address

  fun key_to_address x = x

(*
	6.	structure Network_Pattern
*)

  structure Network_Pattern =
   struct
    type host_id = Host_Id.T
    type protocol_id = Protocol_Id.T
    (* parameterize pattern to work around SML/NJ's representation bug *)
    datatype 'a internal_pattern = Complete of {peer: 'a,
						proto: protocol_id}
			         | Partial of {proto: protocol_id}
    type pattern = host_id internal_pattern
    type T = pattern
    fun makestring (Complete address) = Network_Address.makestring
					  (Network_Address.Address address)
      | makestring (Partial {proto}) = Protocol_Id.makestring proto
    fun equal (Complete a1, Complete a2) =
          Network_Address.equal (Network_Address.Address a1,
				 Network_Address.Address a2)
      | equal (Partial {proto = p1}, Partial {proto = p2}) = p1 = p2
      | equal _ = false
    fun hash (Complete address) = Network_Address.hash
				    (Network_Address.Address address)
      | hash (Partial {proto}) = Protocol_Id.hash proto
   end (* struct *)
  structure Pattern = Network_Pattern

(*
	7.	structures Network_Incoming and Network_Outgoing

	The pseudo-header checksum for this protocol is always zero.
*)

  structure Network_Incoming =
   struct
    type checksum = Word16.word
    type net_option = unit list
    open Lower.Incoming
    fun pseudo_header_checksum _ = Word16.fromInt 0
    fun options _ = []
   end (* struct *)
  structure Incoming = Network_Incoming

  structure Network_Outgoing =
   struct
    type net_option = unit list
    open Lower.Outgoing
    fun new_options (data, _) = new data
    fun put_options (data, _) = data
   end
  structure Outgoing = Lower.Outgoing

(*
	8.	structure Network_Status

	specific_status is a status specific to the implementation of
	IP.  This is used in EXTENDED_IP (see below) and is probably
	of little interest to application writers.
*)

  structure Network_Status =
   struct
    type higher_header = Network_Incoming.T
    datatype problem = Routing
                     | Source_Route_Failed
                     | Time_To_Live_Exceeded
                     | Reassembly_Time_Exceeded
                     | Parameter_Problem of int
                     | Fragmentation_Needed of {mtu: int}
                     | Missing_Option
    datatype inaccessible = Port | Protocol | Other
    datatype status = Unreachable of problem * higher_header
                    | Inaccessible of inaccessible * higher_header
                    | Quench of higher_header * unit
    type T = status
    fun makestring_problem Routing = "routing"
      | makestring_problem Source_Route_Failed = "bad source route"
      | makestring_problem Time_To_Live_Exceeded = "time to live exceeded"
      | makestring_problem Reassembly_Time_Exceeded =
         "reassembly time exceeded"
      | makestring_problem (Parameter_Problem pointer) =
         "parameter problem at position " ^ Integer.toString pointer
      | makestring_problem (Fragmentation_Needed {mtu}) =
         "Fragmentation needed, maximum MTU is " ^ Integer.toString mtu
      | makestring_problem Missing_Option = "missing required option"
    fun makestring_inaccessible Port = "port "
      | makestring_inaccessible Protocol = "protocol "
      | makestring_inaccessible Other = ""
    fun makestring (Unreachable (problem, data)) =
         "unreachable due to " ^ makestring_problem problem ^
	 ", header bits " ^ Network_Incoming.makestring_max (data, 0w8)
      | makestring (Inaccessible (inaccessible, data)) =
	 "destination " ^ makestring_inaccessible inaccessible ^
	 "is inaccessible, header bits " ^
	 Network_Incoming.makestring_max (data, 0w8)
      | makestring (Quench (data, _)) =
	 "quench " ^ Network_Incoming.makestring_max (data, 0w8)
   end (* struct *)
  structure Status = Network_Status

(*
	9.	required extensions
*)

  type specific_connection_extension = unit
  (* parameterize connection_extension to work around SML/NJ's
     representation bug *)
  datatype 'host_id internal_connection_extension =
      Connection_Extension of
        {port_unreachable: Incoming.T -> unit, (* call if can't deliver *)
         max_packet_size: Word.word,
         can_fragment: bool, (* if can_fragment, max_packet_size is advisory *)
	 local_address: 'host_id,
	 remote_address: 'host_id,
	 pseudo_header_checksum: Outgoing.T -> Network_Incoming.checksum,
	 time_to_live: unit -> int,
	 set_time_to_live: int -> unit,
	 type_of_service: unit -> int,
	 set_type_of_service: int -> unit,
	 packets_sent: unit -> Word64.word,
	 packets_received: unit -> Word64.word,
	 specific: specific_connection_extension}
  type network_connection_extension = Host_Id.T internal_connection_extension
  type connection_extension = network_connection_extension

  type listen_extension = unit

  type specific_session_extension = unit
  (* parameterize session_extension to work around SML/NJ's
     representation bug *)
  datatype 'host_id internal_session_extension =
      Session_Extension of
        {packets_sent: unit -> Word64.word,
	 packets_received: unit -> Word64.word,
	 failed_sends: unit -> Word64.word,
	 packets_rejected: unit -> Word64.word,
  (* interfaces, subnet masks, and gateways are normally
     set automatically at session initialization; these are
     manual overrides, and would normally only be used by ICMP. *)
	 interfaces: unit -> (string * 'host_id option) list,
	 set_interface_address: string * 'host_id -> unit,
	 disable_interface: string -> unit,
	 set_subnet_mask: string * 'host_id option -> unit,
	 add_default_gateway: 'host_id -> unit,
	 remove_default_gateway: 'host_id -> unit,
	 add_specific_gateway: {destination: 'host_id,
				gateway: 'host_id} -> unit,
	 remove_specific_gateway: {destination: 'host_id} -> unit,
	 specific: specific_session_extension} 
  type network_session_extension = Host_Id.T internal_session_extension 
  type session_extension = network_session_extension

(*
	10.	structures from lower protocol
*)

  structure X = Lower.X
  structure Count = Lower.Count

(*
	11.	local declarations
*)

  exception Already_Open of Connection_Key.T

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

(*
	12.	structure Length_Extern
*)

  structure Length_Extern =
      Protocol_Extern16_Big (structure In = Lower.Incoming
			     structure Out = Lower.Outgoing
			     structure B = B)

(*
	13.	session
*)

  local
   structure Trace = Trace (structure V = B.V
			    val debug_level = NONE
			    val module_name = "pseudoip.fun"
			    val makestring = fn _ => NONE)

   val header_size = Length_Extern.size (Word16.fromInt 0)

   fun local_send (lower_send, sent, min_send_length) packet =
	(let val packet_length = Outgoing.size packet
	     val length = Word16.fromInt (Word.toInt packet_length)
	     val bytes = Outgoing.uninitialized header_size
	     val total_length = packet_length + header_size
	     val _ = Length_Extern.marshal (bytes, length) 0w0
	     val new_packet = Outgoing.join (bytes, packet)
	 in sent := Word64.+ (! sent, Word64.fromInt 1);
	    if total_length >= min_send_length then
	     lower_send new_packet
	    else
	     lower_send
	      (Outgoing.join
	       (new_packet,
	        Outgoing.uninitialized (min_send_length - total_length)))
	 end)
	  handle x => Trace.print_raise_again (x, SOME "local_send")

   fun local_conn_handler (upper_handler, sent, min_size, ext)
                          (Lower.C {send, abort, extension}) =
        (let val connection = C {send = local_send (send, sent, min_size),
				 abort = abort, extension = ext}
	 in upper_handler connection
	 end)
	  handle x => Trace.print_raise_again (x, SOME "local_conn_handler")

   val one64 = Word64.fromInt 1

   fun local_data_handler (upper_handler, sent, received, min_size, ext)
                          (Lower.C {send, abort, extension}, packet) =
        (let val (length, skip) = Length_Extern.unmarshal (packet, 0w0)
	     val (_, short_packet) = Lower.Incoming.split (packet, skip)
	     val size = Word.fromInt (Word16.toInt length)
	     val (result, discard) = Lower.Incoming.split (short_packet, size)
	     val connection = C {send = local_send (send, sent, min_size),
				 abort = abort, extension = ext}
	 in received := Word64.+ (! received, one64);
            upper_handler (connection, result)
         end)
	  handle x => Trace.print_raise_again (x, SOME "local_data_handler")

   fun local_status_handler _ = ()

   val zero16 = Word16.fromInt 0
   fun local_handler (self, upper_handler, min_size, max_size) lower_key =
        ((let val Lower.Eth_Address.Address {eth, proto} = lower_key
	      val upper_key = Network_Address.Address {peer = eth,
						       proto = proto}
	      val {connection_handler, data_handler,
		   status_handler} = upper_handler upper_key
	      val ttl = ref 0
	      val tos = ref 0
	      val sent = ref (Word64.fromInt 0)
	      val received = ref (Word64.fromInt 0)
	      val extension = Connection_Extension
		               {port_unreachable = fn _ => (),
				max_packet_size = max_size - header_size,
			        can_fragment = false,
			        local_address = self,
			        remote_address = eth,
			        pseudo_header_checksum = fn _ => zero16,
			        time_to_live = fn _ => ! ttl,
			        set_time_to_live = fn new => ttl := new,
			        type_of_service = fn _ => ! tos,
			        set_type_of_service = fn new => tos := new,
			        packets_sent = fn _ => ! sent,
			        packets_received = fn _ => ! received,
			        specific = ()}
	      val c = local_conn_handler (connection_handler, sent,
					  min_size, extension)
	      val d = local_data_handler (data_handler, sent, received,
					  min_size, extension)
	      val s = local_status_handler
	  in {connection_handler = c, data_handler = d, status_handler = s}
	  end)
	   handle x => Trace.print_raise_again (x, SOME "local_handler"))

   fun local_connect (self, connect, min_size, max_size)
                     (Network_Address.Address {peer, proto}, H handler) =
        (connect (Lower.Eth_Address.Address {eth = peer, proto = proto},
		  Lower.H (local_handler (self, handler, min_size, max_size))))
	 handle x => Trace.print_raise_again (x, SOME "local_connect")

   fun local_listen argument (Pattern.Complete {proto, ...}, h, max) =
        local_listen argument (Pattern.Partial {proto = proto}, h, max)
     | local_listen (self, listen, min_size, max_size)
                    (Pattern.Partial {proto}, H handler, max) =
	(let val lower_handler = Lower.H (local_handler (self, handler,
							 min_size, max_size))
	     val Lower.L {stop, extension} =
	          listen (Lower.Eth_Pattern.Partial {proto = proto},
			  lower_handler, max)
	 in L {stop = stop, extension = ()}
	 end)
	  handle x => Trace.print_raise_again (x, SOME "local_listen")

   fun local_session (self, session_fun)
                     (Lower.S {connect, listen,
			       extension =
			       Lower.Eth_Session_Extension
			         {maximum_packet_size,
				  minimum_packet_size, ...}}) =
        (let val c = local_connect (self, connect, minimum_packet_size,
				    maximum_packet_size)
             val l: Pattern.T * handler * Count.T -> listen
                  = local_listen (self, listen, minimum_packet_size,
				  maximum_packet_size)
	     val zero = Word64.fromInt 0
	     val ext = Session_Extension
	                 {packets_sent = fn _ => zero,
		          packets_received = fn _ => zero,
		          failed_sends = fn _ => zero,
		          packets_rejected = fn _ => zero,
		          interfaces = fn _ => [],
		          set_interface_address = fn _ => (),
		          disable_interface = fn _ => (),
		          set_subnet_mask = fn _ => (),
		          add_default_gateway = fn _ => (),
		          remove_default_gateway = fn _ => (),
		          add_specific_gateway = fn _ => (),
		          remove_specific_gateway = fn _ => (),
		          specific = ()}
	 in session_fun (S {connect = c, listen = l, extension = ext})
	 end)
	  handle x => Trace.print_raise_again (x, SOME "local_session")

  in (* local *)
   fun session (Network_Setup.Setup [], session_fun) =
        Trace.print_raise (X.Session "no interfaces", SOME "session")
     | session (Network_Setup.Setup
		  [{local_id, interface, gateways, mask, mtu}], session_fun) =
        ((Lower.session (interface, local_session (local_id, session_fun)))
	 handle x => Trace.print_raise_again (x, SOME "session"))
     | session (_, session_fun) =
        Trace.print_raise (X.Session "too many interfaces", SOME "session")

  end (* local *)

 end (* struct *)

