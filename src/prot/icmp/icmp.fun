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



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Icmp
	2.	type declarations
	3.	internal structure In
	4.	internal structure Out
	5.	type declarations
	6.	makestrings
	7.	State
	8.	Exceptions
	9.	Connection Management
	10.	functions close and abort
	11.	function parse_icmp_packet
	12.	function create_data_packet
	13.	function create_echo_packet
	14.	function new_icmp_packet
	15.	function allocate_send
	16.	function serve_icmp
	17.	function receive
	18.	function connect
	19.	function start_passive
	20.	functions serve_mask and stop_mask
	21.	function finalize
	22.	function initialize

		iii.	RCS Log
	
$Log: icmp.fun,v $
Revision 1.30  1995/03/24  16:03:34  esb
fixed a bug.

Revision 1.29  1995/03/24  01:57:14  esb
made a debugging statement conditional on the debugging level.

Revision 1.28  1995/03/24  01:47:43  esb
major revision.

Revision 1.27  1995/03/12  17:55:39  esb
adapted to new trace.sig.

Revision 1.26  1995/03/10  03:50:23  esb
adapted to new vendor.sig.

Revision 1.25  1995/03/07  23:43:54  esb
updated tracing.

Revision 1.24  1995/02/04  20:39:57  robby
updated to 107

Revision 1.23  1995/01/18  21:06:39  esb
fixed a bug due to explicit byte-ordering combined with dyn_locs reordering
implicitly.

Revision 1.22  1995/01/17  21:06:26  esb
adapted to new icmp.sig

Revision 1.21  1995/01/16  23:46:35  esb
fixed a bug in packet-time-exceeded.

Revision 1.20  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.19  1994/11/11  18:11:52  esb
changed the functor parameters for Debug_Trace_Printer.

Revision 1.18  1994/11/10  16:12:20  milnes
Updated for tcpipeth/addressing and debug_trace structure.

Revision 1.17  1994/11/07  21:36:51  cline
use V.String

Revision 1.16  1994/10/25  16:32:50  esb
added handles around all calls to Conn.functions.

Revision 1.15  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.14  1994/09/30  17:05:58  esb
changed DLOCS to Dyn_Locs.

Revision 1.13  1994/09/23  16:52:36  milnes
Corrected a spaco.

Revision 1.12  1994/09/14  15:31:19  milnes
Fixed a debug_print typo and extended a print.

Revision 1.11  1994/09/12  18:29:21  milnes
Added handle _'s.

Revision 1.10  1994/08/28  21:46:44  milnes
New implementation.

Revision 1.9  1994/08/22  21:10:08  milnes
Saved a revision that is almost working to work on the Ip_Address
can't have a lower IP connection bug.

Revision 1.8  1994/07/04  21:35:47  esb
adapted to Copy/Create split.

Revision 1.7  1994/07/01  02:36:02  danwang
Moved control structures into Fox_Basis.

Revision 1.6  1994/06/29  19:40:54  milnes
tweaked.

Revision 1.5  1994/06/16  18:12:32  danwang
Updated to use functorized Fox_Basis

Revision 1.4  1994/06/15  20:48:05  milnes
Updated for IP subnet routing.

Revision 1.3  1994/06/09  18:36:03  esb
adapted to new incoming_message type for IP.

Revision 1.2  1994/06/05  18:45:28  milnes
Extende and tested.

Revision 1.1  1994/05/23  14:09:03  milnes
Initial revision

		1.	functor Icmp

*)

functor Icmp (structure B: FOX_BASIS
              structure Ip: EXTENDED_IP
	        sharing type Ip.outgoing = B.Dyn_Array.T
                    and type Ip.incoming_data = B.Dyn_Array.T
                    and type Ip.ip_number = FoxWord32.word
                    and type Ip.ip_protocol = FoxWord8.word
              val serve_echos_and_address_masks: bool
	      val debug_level: int ref option): ICMP_PROTOCOL =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "icmp.fun")
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print
  val debug_constant_string = Trace.debug_constant_string

(*
		2.	type declarations
*)

  type ip_number = FoxWord32.word
  type ip_option = Ip.Option.T
  type ip_data = B.Dyn_Array.T

  datatype icmp_address = Icmp_Address of ip_number

  val icmp_over_ip = SW.n8 "1"

  datatype on_off = On | Off

  type icmp_connection = ip_number * on_off ref

  type address = icmp_address
  type connection = icmp_connection

  datatype redirect =
      Network_Redirect
    | Host_Redirect
    | Tos_Network_Redirect
    | Tos_Host_Redirect

  fun makestring_redirect Network_Redirect = "Network_Redirect"
    | makestring_redirect Host_Redirect = "Host_Redirect"
    | makestring_redirect Tos_Network_Redirect = "Tos_Network_Redirect"
    | makestring_redirect Tos_Host_Redirect = "Tos_Host_Redirect"

  datatype unreachable = 
      Network_Unreachable
    | Host_Unreachable
    | Protocol_Unreachable
    | Port_Unreachable
    | Fragmentation_Needed
    | Source_Route_Failed
    | Network_Unknown
    | Host_Unknown 
    | Source_Host_Isolated
    | Communication_With_Network_Prohibited
    | Communication_With_Host_Prohibited
    | Network_Unreachable_for_Tos
    | Host_Unreachable_for_Tos

  fun makestring_unreachable Network_Unreachable = "network unreachable"
    | makestring_unreachable Host_Unreachable = "host unreachable"
    | makestring_unreachable Protocol_Unreachable = "protocol unreachable"
    | makestring_unreachable Port_Unreachable = "port unreachable"
    | makestring_unreachable Fragmentation_Needed = "fragmentation needed"
    | makestring_unreachable Source_Route_Failed = "failed source route"
    | makestring_unreachable Network_Unknown = "network unknown"
    | makestring_unreachable Host_Unknown = "host uknown"
    | makestring_unreachable Source_Host_Isolated = "isolated"
    | makestring_unreachable Communication_With_Network_Prohibited =
       "network not accessible"
    | makestring_unreachable Communication_With_Host_Prohibited =
       "host not accessible"
    | makestring_unreachable Network_Unreachable_for_Tos =
       "network does not support TOS"
    | makestring_unreachable Host_Unreachable_for_Tos =
       "host does not support TOS"

  fun makestring_ip_data data =
       B.Dyn_Array.makestring {data = data, start_print = 0,
			       max_length = 200, base = 16, separator = "."}

(*
	3.	internal structure In
*)

  structure In =
   struct
    (* When a parameter problem message arrives, Icmp parses it out
       and represents it as a problem in the IP header, Ip options,
       or the data of the packet. *)
    datatype problem_specifier = 
        Header of FoxWord8.word
      | Option of ip_option
      | Data of FoxWord8.word * ip_data

    fun makestring_problem (Header pos) =
         "header problem at position " ^ FoxMakestring.word8 pos
      | makestring_problem (Option option) =
	 "option problem with option " ^ Ip.Option.makestring_option option
      | makestring_problem (Data (pos, data)) =
	 "data problem at position " ^ FoxMakestring.word8 pos ^ ", data = " ^
	 makestring_ip_data data

    datatype icmp_message  =
        Unreachable of unreachable
      | Transit_Time_Exceeded
      | Reassembly_Time_Exceeded
      | Parameter_Problem of problem_specifier
      | Source_Quench 
      | Redirect of redirect
      | Echo of {id: FoxWord16.word, sequence: FoxWord16.word, data: ip_data}
      | Echo_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		       data: ip_data}
      | Time_Stamp of {id: FoxWord16.word, sequence: FoxWord16.word,
		       originate: FoxWord32.word}
      | Time_Stamp_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
			     originate: FoxWord32.word,
			     receive: FoxWord32.word,
			     transmit: FoxWord32.word}
      | Mask_Request of {id: FoxWord16.word, sequence: FoxWord16.word}
      | Mask_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		       address_mask: ip_number}

    fun makestring_incoming (Unreachable unreachable) =
         "destination unreachable: " ^ makestring_unreachable unreachable
      | makestring_incoming Transit_Time_Exceeded =
         "transit time exceeded"
      | makestring_incoming Reassembly_Time_Exceeded =
         "reassembly time exceeded"
      | makestring_incoming (Parameter_Problem problem_specifier) =
         "parameter problem: " ^ makestring_problem problem_specifier
      | makestring_incoming Source_Quench = "source quench"
      | makestring_incoming (Redirect redirect) =
	 "redirect: " ^ makestring_redirect redirect
      | makestring_incoming (Echo {id: FoxWord16.word,
				   sequence: FoxWord16.word,
				   data: ip_data}) =
	 "echo (id = " ^ FoxMakestring.word16 id ^
	 ", sequence = " ^ FoxMakestring.word16 sequence ^
	 ", data = " ^ makestring_ip_data data ^ ")"
      | makestring_incoming (Echo_Reply {id: FoxWord16.word,
					 sequence: FoxWord16.word,
					 data: ip_data}) =
	 "echo reply (id = " ^ FoxMakestring.word16 id ^
	 ", sequence = " ^ FoxMakestring.word16 sequence ^
	 ", data = " ^ makestring_ip_data data ^ ")"
      | makestring_incoming (Time_Stamp {id: FoxWord16.word,
					 sequence: FoxWord16.word,
					 originate: FoxWord32.word}) =
	 "time stamp (id = " ^ FoxMakestring.word16 id ^
	 ", sequence = " ^ FoxMakestring.word16 sequence ^
	 ", originate = " ^ FoxMakestring.word32 originate ^ ")"
      | makestring_incoming (Time_Stamp_Reply {id: FoxWord16.word,
					       sequence: FoxWord16.word,
					       originate: FoxWord32.word,
					       receive: FoxWord32.word,
					       transmit: FoxWord32.word}) =
	 "time stamp reply (id = " ^ FoxMakestring.word16 id ^
	 ", sequence = " ^ FoxMakestring.word16 sequence ^
	 ", originate = " ^ FoxMakestring.word32 originate ^
	 ", receive = " ^ FoxMakestring.word32 receive ^
	 ", transmit = " ^ FoxMakestring.word32 transmit ^ ")"
      | makestring_incoming (Mask_Request {id: FoxWord16.word,
					   sequence: FoxWord16.word}) =
	 "address mask request (id = " ^ FoxMakestring.word16 id ^
	 ", sequence = " ^ FoxMakestring.word16 sequence ^ ")"
      | makestring_incoming (Mask_Reply {id: FoxWord16.word,
					 sequence: FoxWord16.word,
					 address_mask: ip_number}) =
	 "address mask reply (id = " ^ FoxMakestring.word16 id ^
	 ", sequence = " ^ FoxMakestring.word16 sequence ^
	 ", address mask = " ^ FoxMakestring.word32 address_mask ^ ")"
    end (* struct *)

(*
	4.	internal structure Out
*)

  structure Out =
   struct 
    datatype allocation =
        Unreachable of unreachable * ip_data
      | Reassembly_Time_Exceeded of ip_data
      | Transit_Time_Exceeded of ip_data
      | Parameter_Problem of {pointer: FoxWord8.word, data: ip_data}
      | Source_Quench of ip_data
      | Echo of {id: FoxWord16.word, sequence: FoxWord16.word, data: ip_data}
      | Echo_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		       data: ip_data}
      | Time_Stamp of {id: FoxWord16.word, sequence: FoxWord16.word,
		       originate: FoxWord32.word}
      | Time_Stamp_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
			     originate: FoxWord32.word,
			     receive: FoxWord32.word,
			     transmit: FoxWord32.word}
      | Mask_Request of {id: FoxWord16.word, sequence: FoxWord16.word}
      | Mask_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		       address_mask: ip_number}

    fun makestring_allocation (Unreachable (unreachable, ip_data)) =
         "destination unreachable: " ^ makestring_unreachable unreachable
      | makestring_allocation (Reassembly_Time_Exceeded ip_data) =
         "reassembly time exceeded"
      | makestring_allocation (Transit_Time_Exceeded ip_data) =
         "transit time exceeded"
      | makestring_allocation (Parameter_Problem {pointer: FoxWord8.word,
						  data: ip_data}) =
         "parameter problem at position " ^ FoxMakestring.word8 pointer ^
	 " in " ^ makestring_ip_data data
      | makestring_allocation (Source_Quench ip_data) = "source quench"
      | makestring_allocation (Echo {id: FoxWord16.word,
				     sequence: FoxWord16.word,
				     data: ip_data}) =
	 "echo (id = " ^ FoxMakestring.word16 id ^
	 ", sequence = " ^ FoxMakestring.word16 sequence ^
	 ", data = " ^ makestring_ip_data data ^ ")"
      | makestring_allocation (Echo_Reply {id: FoxWord16.word,
					   sequence: FoxWord16.word,
					   data: ip_data}) =
	 "echo reply (id = " ^ FoxMakestring.word16 id ^
	 ", sequence = " ^ FoxMakestring.word16 sequence ^
	 ", data = " ^ makestring_ip_data data ^ ")"
      | makestring_allocation (Time_Stamp {id: FoxWord16.word,
					   sequence: FoxWord16.word,
					   originate: FoxWord32.word}) =
	 "time stamp (id = " ^ FoxMakestring.word16 id ^
	 ", sequence = " ^ FoxMakestring.word16 sequence ^
	 ", originate = " ^ FoxMakestring.word32 originate ^ ")"
      | makestring_allocation (Time_Stamp_Reply {id: FoxWord16.word,
						 sequence: FoxWord16.word,
						 originate: FoxWord32.word,
						 receive: FoxWord32.word,
						 transmit: FoxWord32.word}) =
	 "time stamp reply (id = " ^ FoxMakestring.word16 id ^
	 ", sequence = " ^ FoxMakestring.word16 sequence ^
	 ", originate = " ^ FoxMakestring.word32 originate ^
	 ", receive = " ^ FoxMakestring.word32 receive ^
	 ", transmit = " ^ FoxMakestring.word32 transmit ^ ")"
      | makestring_allocation (Mask_Request {id: FoxWord16.word,
					     sequence: FoxWord16.word}) =
	 "address mask request (id = " ^ FoxMakestring.word16 id ^
	 ", sequence = " ^ FoxMakestring.word16 sequence ^ ")"
      | makestring_allocation (Mask_Reply {id: FoxWord16.word,
					   sequence: FoxWord16.word,
					   address_mask: ip_number}) =
	 "address mask reply (id = " ^ FoxMakestring.word16 id ^
	 ", sequence = " ^ FoxMakestring.word16 sequence ^
	 ", address mask = " ^ FoxMakestring.word32 address_mask ^ ")"

    type icmp_message = allocation

   end (* struct *)

(*
		5.	type declarations
*)

  type incoming = In.icmp_message
  type allocation = Out.allocation
  type outgoing = Out.icmp_message
  type status = unit

  datatype handler =
    Handler of connection -> ((incoming -> unit) * (status -> unit))

(*
		6.	makestrings
*)

  fun makestring_address (Icmp_Address ip) = Ip.makestring_ip ip

  fun makestring_connection (ip_number, on_off) =
       Ip.makestring_ip ip_number ^
       (case ! on_off of On => "" | Off => "not ") ^ "serving ICMP"

  fun makestring_incoming (message, _) = In.makestring_incoming message
  fun makestring_outgoing (message, _) = Out.makestring_allocation message
  fun makestring_status () = "ICMP status"

(*
		7.	State
*)

  datatype local_state =
      State of {ip_stop: (unit -> unit),
		address_masks: (string, FoxWord32.word * bool) B.Store.T ref,
		default_service: on_off ref}

(*
		8.	Exceptions
*)

  exception Initialization_Failed of string
  exception Protocol_Finalized
  exception Connection_Closed of connection * string
  exception Illegal_Address of string
  exception Open_Failed of string
  exception Send_Failed of string

(*
		9.	Connection Management
*)

  type key = ip_number
  val key_hash = FoxWord32.wordToInt
  val key_equal: (key * key -> bool) = op=

  fun hash_address (Icmp_Address ip) = FoxWord32.wordToInt ip
  val equal_address = (op= : icmp_address * icmp_address -> bool)

  fun hash_connection (ip, _) = key_hash ip
  fun equal_connection ((ip_1: ip_number, _), (ip_2, _)) = ip_1 = ip_2

  structure Conn =
      Connection (type protocol_state = local_state
		  type key = key
		  type connection = connection
		  type lower_key = key
		  type lower_connection = Ip.connection
		  type message = incoming
		  type status = status
		  val key_hash = key_hash
		  val key_eq = key_equal
		  type passive = ip_number
		  val passive_hash = key_hash
		  val passive_eq = key_equal
		  val lower_hash = key_hash
		  val lower_eq = key_equal
		  val lower_close = Ip.close
		  structure Trace = Trace
		  structure B = Fox_Basis)

  fun connect_lower receive ip =
       Ip.connect (Ip.Address {ip = ip, proto = icmp_over_ip},
		   Ip.Handler (fn c => (receive c, fn _ => ())))

  fun known_lower ip_conn ip = ip_conn

  fun handle_conn (x, name, connection) =
       case x of
	  Conn.Initialization =>
	   (local_print ("error in " ^ name ^ ", not initialized");
	    raise Protocol_Finalized)
	| Conn.Open =>
	   (local_print ("error in " ^ name ^ ", open failed");
	    raise Open_Failed ("in " ^ name))
	| Conn.Missing =>
	   (case connection of
	       NONE =>
		(local_print ("error in " ^ name ^ ", no such connection");
		 raise Open_Failed ("in " ^ name))
	     | SOME c =>
		(local_print ("error in " ^ name ^ ", no such connection");
		 raise Connection_Closed (c, "in " ^ name)))
	| x =>
	   (local_print ("error in " ^ name ^
			 ", exception " ^ System.exn_name x);
	    raise x)

(*
		10.	functions close and abort
*)

  local
   fun close_abort name (ip, _) =
        ((Conn.remove ip)
          handle x =>
                  local_print ("exception " ^ System.exn_name x ^
			       " raised in " ^ name ^ " (" ^
			       Ip.makestring_ip ip ^ "), continuing."))

  in (* local *)
   val close = close_abort "close"
   val abort = close_abort "abort"
  end (* local *)

(*
		11.	function parse_icmp_packet
*)

  exception Bad_Parse

  fun rest_of_packet (bytes, start) =
       B.Dyn_Array.init (B.Dyn_Array.sub (bytes, start,
					  B.Dyn_Array.size bytes - start))

  local
   val icmp_packet_header_length = 8
   val ip_header_protocol_offset = 9 + icmp_packet_header_length
   val ip_header_destination_offset = 16 + icmp_packet_header_length
  in
   fun get_ip_address bytes =
        (B.Dyn_Array.sub4 (bytes, ip_header_destination_offset),
	 B.Dyn_Array.sub1 (bytes, ip_header_protocol_offset))
  end

  val network_unreachable_code = SW.n8 "0"	(* RFC 792 *)
  val host_unreachable_code = SW.n8 "1"		(* RFC 792 *)
  val protocol_unreachable_code = SW.n8 "2"	(* RFC 792 *)
  val port_unreachable_code = SW.n8 "3"		(* RFC 792 *)
  val fragmentation_needed_code = SW.n8 "4"	(* RFC 792 *)
  val source_route_failed_code = SW.n8 "5"	(* RFC 792 *)
  val network_unknown_code = SW.n8 "6"		(* RFC 1122 *)
  val host_unknown_code = SW.n8 "7"		(* RFC 1122 *)
  val source_host_isolated_code = SW.n8 "8"	(* RFC 1122 *)
  val network_prohibited_code = SW.n8 "9"	(* RFC 1122 *)
  val host_prohibited_code = SW.n8 "10"		(* RFC 1122 *)
  val network_tos_code = SW.n8 "11"		(* RFC 1122 *)
  val host_tos_code = SW.n8 "12"		(* RFC 1122 *)

  fun parse_unreachable bytes =
       let val code = B.Dyn_Array.sub1 (bytes, 1)
	   val specific =
	        if code = network_unreachable_code then Network_Unreachable
		else if code = host_unreachable_code then Host_Unreachable
		else if code = protocol_unreachable_code then
		 Protocol_Unreachable
		else if code = port_unreachable_code then Port_Unreachable
		else if code = fragmentation_needed_code then
		 Fragmentation_Needed
		else if code = source_route_failed_code then
		 Source_Route_Failed
		else if code = network_unknown_code then Network_Unknown
		else if code = host_unknown_code then Host_Unknown
		else if code = source_host_isolated_code then
		 Source_Host_Isolated
		else if code = network_prohibited_code then
		 Communication_With_Network_Prohibited
		else if code = host_prohibited_code then
		 Communication_With_Host_Prohibited
		else if code = network_tos_code then
		 Network_Unreachable_for_Tos
		else if code = host_tos_code then Host_Unreachable_for_Tos
		else raise Bad_Parse
       in In.Unreachable specific
       end

  val transit_subcode = SW.n8 "0"	(* RFC 792 *)
  val reassembly_subcode = SW.n8 "1"	(* RFC 792 *)

  fun parse_time_exceeded bytes =
       let val subcode = B.Dyn_Array.sub1 (bytes, 1)
       in if subcode = reassembly_subcode then In.Reassembly_Time_Exceeded
	  else if subcode = transit_subcode then In.Transit_Time_Exceeded
	  else raise Bad_Parse
       end

  val verhlen_in_icmp_index = 8		(* RFC 791, RFC 792 *)
  val ip_min_hlen = SW.n8 "20"		(* RFC 791 *)
  val header_mask = SW.n8 "0xf"		(* RFC 791 *)
  val one = SW.n8 "1"

  fun parse_parameter_problem bytes =
       let val pointer = B.Dyn_Array.sub1 (bytes, 4)
           val hlen_byte = B.Dyn_Array.sub1 (bytes, verhlen_in_icmp_index)
           val ip_header_length =
	         FoxWord8.lshift (FoxWord8.andb (hlen_byte, header_mask), 2)
	   val int_header_length = FoxWord8.wordToInt ip_header_length
	   fun find_option (start, length) =
	        if FoxWord8.> (FoxWord8.+ (start, length),
			       ip_header_length) then
		 raise Bad_Parse
		else
	         case Ip.Extended_Option.parse
		        {data = B.Dyn_Array.read bytes,
			 start = FoxWord8.wordToInt start,
			 length = FoxWord8.wordToInt length} of
		    [] =>
		     find_option (pointer,
				  FoxWord8.+ (length, one))
		  | (option :: rest) =>
		     if FoxWord8.<= (pointer,
				     FoxWord8.+ (start, length)) then
		      option
		     else
		      find_option (FoxWord8.+ (pointer, length), one)
	   val location = if FoxWord8.< (pointer, ip_min_hlen) then
                           In.Header pointer
	                  else if FoxWord8.> (pointer, ip_header_length) then
			   In.Data (FoxWord8.- (pointer, ip_header_length),
				    B.Dyn_Array.tail (bytes,
						      int_header_length +
						      verhlen_in_icmp_index))
			  else
			   In.Option (find_option (ip_min_hlen, one))
       in In.Parameter_Problem location
       end

  fun parse_source_quench bytes = In.Source_Quench

  val network_redirect_code = SW.n8 "0"		(* RFC 792 *)
  val host_redirect_code = SW.n8 "1"		(* RFC 792 *)
  val tos_network_redirect_code = SW.n8 "2"	(* RFC 792 *)
  val tos_host_redirect_code = SW.n8 "3"	(* RFC 792 *)

  fun parse_redirect bytes =
       let val code = B.Dyn_Array.sub1 (bytes, 1)
	   val specific =
	        if code = network_redirect_code then Network_Redirect
		else if code = host_redirect_code then Host_Redirect
	        else if code = tos_network_redirect_code then
		 Tos_Network_Redirect
		else if code = tos_host_redirect_code then Tos_Host_Redirect
		else raise Bad_Parse
       in In.Redirect specific
       end

  fun parse_echo bytes =
       In.Echo {id = B.Order.B2.from_big (B.Dyn_Array.sub2 (bytes, 4)),
		sequence = B.Order.B2.from_big (B.Dyn_Array.sub2 (bytes, 6)),
		data = rest_of_packet (bytes, 8)}

  fun parse_echo_reply bytes =
       In.Echo_Reply
         {id = B.Order.B2.from_big (B.Dyn_Array.sub2 (bytes, 4)),
	  sequence = B.Order.B2.from_big (B.Dyn_Array.sub2 (bytes, 6)),
	  data = rest_of_packet (bytes, 8)}

  fun parse_time_stamp bytes =
       In.Time_Stamp
         {id = B.Order.B2.from_big (B.Dyn_Array.sub2 (bytes, 4)),
	  sequence = B.Order.B2.from_big (B.Dyn_Array.sub2 (bytes, 6)),
	  originate = B.Order.B4.from_big (B.Dyn_Array.sub4 (bytes, 8))}

  fun parse_time_stamp_reply bytes =
       In.Time_Stamp_Reply
         {id = B.Order.B2.from_big (B.Dyn_Array.sub2 (bytes, 4)),
	  sequence = B.Order.B2.from_big (B.Dyn_Array.sub2 (bytes, 6)),
	  originate = B.Order.B4.from_big (B.Dyn_Array.sub4 (bytes, 8)),
	  receive   = B.Order.B4.from_big (B.Dyn_Array.sub4 (bytes, 12)),
	  transmit = B.Order.B4.from_big (B.Dyn_Array.sub4 (bytes, 16))}

  fun parse_mask bytes =
       In.Mask_Request
         {id = B.Order.B2.from_big (B.Dyn_Array.sub2 (bytes, 4)),
	  sequence = B.Order.B2.from_big (B.Dyn_Array.sub2 (bytes, 6))}

  fun parse_mask_reply bytes =
       In.Mask_Reply
         {id = B.Order.B2.from_big (B.Dyn_Array.sub2 (bytes, 4)),
	  sequence = B.Order.B2.from_big (B.Dyn_Array.sub2 (bytes, 6)),
	  address_mask = B.Order.B4.from_big (B.Dyn_Array.sub4 (bytes, 8))}

  val unreachable_code = SW.n8 "3"		(* RFC 792 *)
  val time_exceeded_code = SW.n8 "11"		(* RFC 792 *)
  val parameter_problem_code = SW.n8 "12"	(* RFC 792 *)
  val source_quench_code = SW.n8 "4"		(* RFC 792 *)
  val redirect_code = SW.n8 "5"			(* RFC 792 *)
  val echo_code = SW.n8 "8"			(* RFC 792 *)
  val echo_reply_code = SW.n8 "0"		(* RFC 792 *)
  val time_stamp_code = SW.n8 "13"		(* RFC 792 *)
  val time_stamp_reply_code = SW.n8 "14"	(* RFC 792 *)
  val mask_code = SW.n8 "17"			(* RFC 950 *)
  val mask_reply_code = SW.n8 "18"		(* RFC 950 *)

(* RFC 1122 states that we must ignore ICMP packets that have bad types, so
   this has option as return type. *)
  fun parse_icmp_packet bytes =
       let val code = B.Dyn_Array.sub1 (bytes, 0)
       in if code = echo_code then
	    parse_echo bytes
	   else if code = echo_reply_code then
	    parse_echo_reply bytes
	   else if code = unreachable_code then
	    parse_unreachable bytes
	   else if code = time_exceeded_code then
	    parse_time_exceeded bytes
	   else if code = parameter_problem_code then
	    parse_parameter_problem bytes
	   else if code = source_quench_code then
	    parse_source_quench bytes
	   else if code = redirect_code then
	    parse_redirect bytes
	   else if code = time_stamp_code then
	    parse_time_stamp bytes
	   else if code = time_stamp_reply_code then
	    parse_time_stamp_reply bytes
	   else if code = mask_code then
	    parse_mask bytes
	   else if code = mask_reply_code then
	    parse_mask_reply bytes
	   else
	    raise Bad_Parse
       end

(*
		12.	function create_data_packet
*)

  val data_bytes = 64 div 8		(* RFC 792 *)
  val verhlen_index = 0			(* RFC 791 *)

  fun create_data_packet data =
       let val hlen_byte = B.Dyn_Array.sub1 (data, verhlen_index)
           val ip_header_length =
	         FoxWord8.wordToInt (FoxWord8.lshift
				     (FoxWord8.andb
				      (hlen_byte, header_mask), 2))
	   val data_length = B.Dyn_Array.size data
	   val ip_length = min (ip_header_length + data_bytes, data_length)
	   val data_pos = 8
	   val length = ip_length + data_pos
	   val packet = B.Dyn_Array.new length
       in B.Dyn_Array.update (packet, data_pos,
			      B.Dyn_Array.sub (data, 0, ip_length));
	  packet
       end

(*
		13.	function create_echo_packet
*)

  fun create_echo_packet data =
       let val data_length = B.Dyn_Array.size data
	   val data_pos = 8
	   val length = data_length + data_pos
	   val packet = B.Dyn_Array.new length
       in B.Dyn_Array.update (packet, data_pos, B.Dyn_Array.read data);
	  packet
       end

(*
		14.	function new_icmp_packet

	This code assumes the packet has been initialized with all zeros.
*)

  fun new_icmp_packet (Out.Unreachable (unreachable, data)) =
       let val reason_code = case unreachable of
                                Network_Unreachable =>
				 network_unreachable_code
			      | Host_Unreachable =>
				 host_unreachable_code
			      | Protocol_Unreachable =>
				 protocol_unreachable_code
			      | Port_Unreachable =>
				 port_unreachable_code
			      | Fragmentation_Needed =>
				 fragmentation_needed_code
			      | Source_Route_Failed =>
				 source_route_failed_code
			      | Network_Unknown =>
				 network_unknown_code
			      | Host_Unknown =>
				 host_unknown_code
			      | Source_Host_Isolated =>
				 source_host_isolated_code
			      | Communication_With_Network_Prohibited =>
				 network_prohibited_code
			      | Communication_With_Host_Prohibited =>
				 host_prohibited_code
			      | Network_Unreachable_for_Tos =>
				 network_tos_code
			      | Host_Unreachable_for_Tos =>
				 host_tos_code
	   val packet = create_data_packet data
       in B.Dyn_Array.update1 (packet, 0, unreachable_code);
	  B.Dyn_Array.update1 (packet, 1, reason_code);
	  packet
       end
    | new_icmp_packet (Out.Reassembly_Time_Exceeded data) =
       let val packet = create_data_packet data
       in B.Dyn_Array.update1 (packet, 0, time_exceeded_code);
	  B.Dyn_Array.update1 (packet, 1, reassembly_subcode);
	  packet
       end
    | new_icmp_packet (Out.Transit_Time_Exceeded data) =
       let val packet = create_data_packet data
       in B.Dyn_Array.update1 (packet, 0, time_exceeded_code);
	  B.Dyn_Array.update1 (packet, 1, transit_subcode);
	  packet
       end
    | new_icmp_packet (Out.Parameter_Problem {pointer, data}) =
       let val packet = create_data_packet data
       in B.Dyn_Array.update1 (packet, 0, parameter_problem_code);
          B.Dyn_Array.update1 (packet, 4, pointer);
	  packet
       end
    | new_icmp_packet (Out.Source_Quench data) =
       let val packet = create_data_packet data
       in B.Dyn_Array.update1 (packet, 0, source_quench_code);
	  packet
       end
    | new_icmp_packet (Out.Echo {id, sequence, data}) =
       let val packet = create_echo_packet data
       in B.Dyn_Array.update1 (packet, 0, echo_code);
	  B.Dyn_Array.update2 (packet, 4, B.Order.B2.to_big id);
	  B.Dyn_Array.update2 (packet, 6, B.Order.B2.to_big sequence);
	  packet
       end
    | new_icmp_packet (Out.Echo_Reply {id, sequence, data}) =
       let val echo = Out.Echo {id = id, sequence = sequence, data = data}
           val packet = new_icmp_packet echo
       in B.Dyn_Array.update1 (packet, 0, echo_reply_code);
	  packet
       end
    | new_icmp_packet (Out.Time_Stamp {id, sequence, originate}) =
       let val packet = B.Dyn_Array.new 20
       in B.Dyn_Array.update1 (packet, 0, time_stamp_code);
	  B.Dyn_Array.update2 (packet, 4, B.Order.B2.to_big id);
	  B.Dyn_Array.update2 (packet, 6, B.Order.B2.to_big sequence);
	  B.Dyn_Array.update4 (packet, 8, B.Order.B4.to_big originate);
	  packet
       end
    | new_icmp_packet (Out.Time_Stamp_Reply {id, sequence, originate,
					     receive, transmit}) =
       let val time_stamp = Out.Time_Stamp {id = id, sequence = sequence,
					    originate = originate}
           val packet = new_icmp_packet time_stamp
       in B.Dyn_Array.update1 (packet, 0, time_stamp_reply_code);
	  B.Dyn_Array.update4 (packet, 12, B.Order.B4.to_big receive);
	  B.Dyn_Array.update4 (packet, 16, B.Order.B4.to_big transmit);
	  packet
       end
    | new_icmp_packet (Out.Mask_Request {id, sequence}) =
       let val packet = B.Dyn_Array.new 12
       in B.Dyn_Array.update1 (packet, 0, mask_code);
	  B.Dyn_Array.update2 (packet, 4, B.Order.B2.to_big id);
	  B.Dyn_Array.update2 (packet, 6, B.Order.B2.to_big sequence);
	  packet
       end
    | new_icmp_packet (Out.Mask_Reply {id, sequence, address_mask}) =
       let val mask_request = Out.Mask_Request {id = id, sequence = sequence}
           val packet = new_icmp_packet mask_request
       in B.Dyn_Array.update1 (packet, 0, mask_reply_code);
	  B.Dyn_Array.update4 (packet, 8, B.Order.B4.to_big address_mask);
	  packet
       end

(*
		15.	function allocate_send

	checksum assumes that the original value in the checksum field is zero.

	send may be called even if no ICMP connection exists for this host.
*)

  fun checksum (data, size) =
       B.Order.B2.to_big
        (B.Checksum.one_s_complement (B.Dyn_Array.checksum (data, 0, size)))

  fun send (ip_conn, packet_specifier) () =
       let val data = new_icmp_packet packet_specifier
	   val size = B.Dyn_Array.size data
	   val (ip_packet, ip_send) = Ip.allocate_send (ip_conn, size)
       in B.Dyn_Array.update2 (data, 2, checksum (data, size));
	  B.Dyn_Array.update (ip_packet, 0, B.Dyn_Array.read data);
	  ip_send ()
       end

  fun allocate_send (conn as (key, _), packet_specifier) =
       (let val Conn.Connection {lower_conn, ...} = Conn.get key
	in (packet_specifier, send (lower_conn, packet_specifier))
	end)
	 handle x => handle_conn (x, "allocate_send", SOME conn)

(*
		16.	function serve_icmp
*)

  exception Transport_Icmp

  fun serve_icmp (ip_conn, _) (In.Echo (echo_params as {id, sequence, data})) =
       send (ip_conn, Out.Echo_Reply echo_params) ()
    | serve_icmp (ip_conn, remote) (In.Mask_Request {id, sequence}) =
       let val State {address_masks, ... } =
	        ((Conn.state ())
		 handle x => handle_conn (x, "serve_icmp", NONE))
       in case Ip.local_address remote of
	     NONE => local_print ("weird, IP has no route to remote " ^
				  Ip.makestring_ip remote)
	   | SOME (interface, local_ip) =>
              (case B.Store.look (! address_masks, interface) of
		  NONE => ()
		| SOME (_, (mask, true)) =>
		   send (ip_conn, Out.Mask_Reply {id = id,
						  sequence = sequence,
						  address_mask = mask}) ()
		| _ => ()) (* not serving the mask *)
       end
   | serve_icmp (ip_conn, remote) (In.Mask_Reply {address_mask, ...}) =
      let val State {address_masks, ...} =
               ((Conn.state ())
		handle x => handle_conn (x, "serve_icmp", NONE))
      in case Ip.local_address remote of
	    NONE => local_print ("weird, IP has no route to remote " ^
				 Ip.makestring_ip remote)
	   | SOME (interface, local_ip) =>
              (case B.Store.look (! address_masks, interface) of
		  NONE =>
		   address_masks := B.Store.add (! address_masks, interface,
						 (address_mask, false))
    (* Once a mask has been set, ignore new ones. *)
		| SOME _ => ())
      end
   | serve_icmp _ _ = raise Transport_Icmp

(*
		17.	function receive
*)

  local
   fun print_raised (name, x, address) =
        local_print ("exception " ^ System.exn_name x ^
		     "raised in " ^ name ^
		     " on address " ^ makestring_address address);

   fun try_passive (ip_conn, packet, ip, receive, ip_data) =
        case Conn.passive ip of
	   NONE =>			(* serve the message if any. *)
	    (let val State {default_service, ...} = Conn.state ()
	     in case ! default_service of
	           Off => ()
		 | On =>
		    ((serve_icmp (ip_conn, ip) packet)
		     handle _ => ())
	     end;
	     Ip.close ip_conn)
	 | SOME (handler, keys, max) =>
	    (Conn.create (ip, (ip, ref On), ip, known_lower ip_conn,
			  handler, SOME keys, max);
	     receive ip_conn ip_data)

   val zero_checksum = SW.n16 "0"
   val ones_checksum = SW.n16 "0xffff"

   fun check_checksum packet =
        let val c = B.Dyn_Array.checksum (packet, 0, B.Dyn_Array.size packet)
	in c = zero_checksum orelse c = ones_checksum
	end

  in (* local *)

   fun receive in_passive ip_conn (raw as (Ip.Ip_Packet {data, source, ...})) =
        if check_checksum data then
	 ((let val packet = parse_icmp_packet data
	   in ((let val Conn.Connection {conn, data_handler, ...} =
	                 Conn.get source 
		    val (_, on) = conn
		in if ! on = On then
		    ((serve_icmp (ip_conn, source) packet)
		     handle Transport_Icmp =>
		             (debug_print (fn _ => "no auto-serve for " ^
					   makestring_incoming (packet, NONE));
			      data_handler packet))
		   else			(* do not auto-serve this packet *)
		    data_handler packet
		end)
	       handle Conn.Missing =>
		       if in_passive then
			local_print ("passive loop, dropping message " ^
				     makestring_incoming (packet, NONE))
		       else
			try_passive (ip_conn, packet, source,
				     receive true, raw))
	   end)
	    handle Bad_Parse =>
		    debug_print (fn _ =>
				 "error trying to parse packet " ^
				 Ip.makestring_incoming (raw, NONE))
		 | x =>
		    print_raised ("receive", x, Icmp_Address source))
	else
	 debug_constant_string "checksum error, dropping packet"

  end (* local *)

(*
		18.	function connect
*)

  fun connect (Icmp_Address ip, Handler handler) =
       let val conn = (ip, ref On)
       in Conn.create (ip, conn, ip, connect_lower (receive false),
		       handler, NONE, NONE);
	  conn
       end

(*
		19.	function start_passive
*)

  fun start_passive (Icmp_Address ip, Handler handler, max_connections) =
       let val key_list_ref =
                ((Conn.start (ip, max_connections, handler))
		 handle x => handle_conn (x, "start_passive", NONE))
           fun stop () =
	        ((Conn.stop ip)
		 handle x => handle_conn (x, "stop_passive", NONE))
	   fun connections () = map (fn (key, conn) => conn) (! key_list_ref)
       in (stop, connections)
       end

(*
		20.	functions service and set_service
*)

  fun service (_, ref result) = result

  fun set_service ((_, reference), wanted) = reference := wanted

(*
		20.	functions no_connection*
*)

  fun no_connection_service () =
       let val State {default_service, ...} = Conn.state ()
       in ! default_service
       end

  fun set_no_connection_service wanted =
       let val State {default_service, ...} = Conn.state ()
       in default_service := wanted
       end

(*
		20.	functions serve_mask and stop_mask
*)

  fun serve_mask {interface, mask} =
       (let val State {address_masks, ...} = Conn.state ()
        in address_masks := B.Store.add (! address_masks, interface,
					 (mask, true))
        end)
	 handle x => handle_conn (x, "serve_mask", NONE)

  fun stop_mask interface =
       (let val State {address_masks, ...} = Conn.state ()
        in address_masks := B.Store.remove (! address_masks, interface)
        end)
	 handle x => handle_conn (x, "stop_mask", NONE)

(*
		21.	function finalize
*)

  fun finalize () =
       (let val State {ip_stop, ...} = Conn.state ()
	    fun close_all () =
                 (ip_stop ();
		  Ip.finalize ();
		  ())
	in Conn.finalize close_all
	end)
       handle x => handle_conn (x, "finalize", NONE)

(*
	22.	function initialize
*)

  fun initialize () =
       let fun handler connection =
	        (receive false connection, fn _ => ())
	   val ip_address = Ip.Partial {proto = icmp_over_ip}
	   fun compute_state () =
	        (((Ip.initialize ())
		   handle Ip.Initialization_Failed s =>
		           let val s = "Ip initialization failed: " ^ s
			   in local_print s;
			      raise Initialization_Failed s
			   end
			| x => handle_conn (x, "initialize", NONE));
		  let val (stop, _) = Ip.start_passive (ip_address,
							Ip.Handler handler,
							NONE)
		      fun string_hash s = 0
		      val string_eq = (op =): string * string -> bool
		      val store: (string, FoxWord32.word * bool) B.Store.T =
		            B.Store.new (string_hash, string_eq)
		  in State {ip_stop = stop,
			    address_masks = ref store,
			    default_service = ref On}
		  end)
       in ((Conn.init compute_state)
	   handle x => handle_conn (x, "initialize", NONE))
       end

 end  (* struct *)
