(*
	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Kuo Chiang Chiang (kcchiang@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract
		
	This functor marshals and unmarshals ICMP headers.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Icmp_Redirect
	2.	functor Icmp_Unreachable
	3.	functor Icmp_Types
	4.	signature ICMP_INCOMING_EXTERN
	5.	functor Icmp_In
	6.	signature ICMP_OUTGOING_EXTERN
	7.	functor Icmp_Out

	iii.	RCS Log
	
$Log: icmpheader.fun,v $
Revision 1.10  1995/11/12  16:40:00  esb
adapted to new Word_Array.

Revision 1.9  1995/10/17  22:35:38  esb
removed a fix that was not needed.

Revision 1.8  1995/10/04  21:30:08  esb
added optional checksumming of only the ICMP header on received packets.

Revision 1.7  1995/10/02  21:20:41  esb
changed debugging statements.

Revision 1.6  1995/09/26  16:29:08  esb
added router advertisement messages and obsolete for obsolete messages.

Revision 1.5  1995/09/17  22:05:59  esb
re-implemented get_universal_time

Revision 1.4  1995/08/30  19:37:44  esb
made test program work.

Revision 1.3  1995/08/29  14:12:47  esb
adapted to new ICMP structure (part of IP).

Revision 1.2  1995/08/24  00:52:07  esb
version with IP and ICMP.

Revision 1.1  1995/08/08  18:25:51  esb
Initial revision


	1.	functor Icmp_Redirect
*)

functor Icmp_Redirect (): ICMP_REDIRECT =
 struct
  exception Unknown_Code

  datatype redirect =
      Network_Redirect
    | Host_Redirect
    | Tos_Network_Redirect
    | Tos_Host_Redirect
  type T = redirect

  fun makestring Network_Redirect = "network redirect"
    | makestring Host_Redirect = "host redirect"
    | makestring Tos_Network_Redirect = "Type-of-Service network redirect"
    | makestring Tos_Host_Redirect = "Type-of-Service host redirect"

  type code = FoxWord8.word
(* RFC 792, p. 12 *)
  val network_redirect_code = SW.n8 "0"
  val host_redirect_code = SW.n8 "1"
  val tos_network_redirect_code = SW.n8 "2"
  val tos_host_redirect_code = SW.n8 "3"

  fun code Network_Redirect = network_redirect_code
    | code Host_Redirect = host_redirect_code
    | code Tos_Network_Redirect = tos_network_redirect_code
    | code Tos_Host_Redirect = tos_host_redirect_code

  fun decode code =
       if code = network_redirect_code then Network_Redirect
       else if code = host_redirect_code then Host_Redirect
       else if code = tos_network_redirect_code then Tos_Network_Redirect
       else if code = tos_host_redirect_code then Tos_Host_Redirect
       else raise Unknown_Code
 end

(*
	2.	functor Icmp_Unreachable
*)

functor Icmp_Unreachable (): ICMP_UNREACHABLE =
 struct
  exception Unknown_Code

  datatype unreachable =
      Network_Unreachable
    | Host_Unreachable
    | Protocol_Unreachable
    | Port_Unreachable
    | Fragmentation_Needed of {mtu: FoxWord16.word}
    | Source_Route_Failed
    | Network_Unknown
    | Host_Unknown
    | Source_Host_Isolated
    | Communication_With_Network_Prohibited
    | Communication_With_Host_Prohibited
    | Network_Unreachable_For_Tos
    | Host_Unreachable_For_Tos
  type T = unreachable

  fun makestring Network_Unreachable = "network unreachable"
    | makestring Host_Unreachable = "host unreachable"
    | makestring Protocol_Unreachable = "protocol not available on host"
    | makestring Port_Unreachable = "port unreachable on host"
    | makestring (Fragmentation_Needed {mtu}) =
       "fragmentation needed and don't-fragment bit set, MTU = " ^
       FoxMakestring.word16 mtu
    | makestring Source_Route_Failed = "source route failed"
    | makestring Network_Unknown = "network unknown"
    | makestring Host_Unknown  = "host unknown"
    | makestring Source_Host_Isolated =
       "source host disconnected from network"
    | makestring Communication_With_Network_Prohibited =
       "network unreachable due to administrative decision"
    | makestring Communication_With_Host_Prohibited =
       "host unreachable due to administrative decision"
    | makestring Network_Unreachable_For_Tos =
       "network unreachable due for Type-of-Service"
    | makestring Host_Unreachable_For_Tos =
       "host unreachable due for Type-of-Service"

  type code = FoxWord8.word
(* RFC 792, p. 4 *)
  val network_unreachable_code = SW.n8 "0"
  val host_unreachable_code = SW.n8 "1"
  val protocol_unreachable_code = SW.n8 "2"
  val port_unreachable_code = SW.n8 "3"
  val fragmentation_needed_code = SW.n8 "4"
  val source_route_failed_code = SW.n8 "5"
(* RFC 1122, p. 39 ff. *)
  val network_unknown_code = SW.n8 "6"
  val host_unknown_code = SW.n8 "7"
  val source_host_isolated_code = SW.n8 "8"
  val communication_with_network_prohibited_code = SW.n8 "9"
  val communication_with_host_prohibited_code = SW.n8 "10"
  val network_unreachable_for_tos_code = SW.n8 "11"
  val host_unreachable_for_tos_code = SW.n8 "12"

  val zero16 = SW.n16 "0"

  fun code Network_Unreachable = (network_unreachable_code, zero16)
    | code Host_Unreachable = (host_unreachable_code, zero16)
    | code Protocol_Unreachable = (protocol_unreachable_code, zero16)
    | code Port_Unreachable = (port_unreachable_code, zero16)
    | code (Fragmentation_Needed {mtu}) = (fragmentation_needed_code, mtu)
    | code Source_Route_Failed = (source_route_failed_code, zero16)
    | code Network_Unknown = (network_unknown_code, zero16)
    | code Host_Unknown = (host_unknown_code, zero16)
    | code Source_Host_Isolated = (source_host_isolated_code, zero16)
    | code Communication_With_Network_Prohibited =
       (communication_with_network_prohibited_code, zero16)
    | code Communication_With_Host_Prohibited =
       (communication_with_host_prohibited_code, zero16)
    | code Network_Unreachable_For_Tos =
       (network_unreachable_for_tos_code, zero16)
    | code Host_Unreachable_For_Tos =
       (host_unreachable_for_tos_code, zero16)

  fun decode (code, mtu) =
       if code = network_unreachable_code then Network_Unreachable
       else if code = host_unreachable_code then Host_Unreachable
       else if code = protocol_unreachable_code then Protocol_Unreachable
       else if code = port_unreachable_code then Port_Unreachable
       else if code = fragmentation_needed_code then
	Fragmentation_Needed {mtu = mtu}
       else if code = source_route_failed_code then Source_Route_Failed
       else if code = network_unknown_code then Network_Unknown
       else if code = host_unknown_code then Host_Unknown
       else if code = source_host_isolated_code then Source_Host_Isolated
       else if code = communication_with_network_prohibited_code then
	Communication_With_Network_Prohibited
       else if code = communication_with_host_prohibited_code then
	Communication_With_Host_Prohibited
       else if code = network_unreachable_for_tos_code then
	Network_Unreachable_For_Tos
       else if code = host_unreachable_for_tos_code then
	Host_Unreachable_For_Tos
       else raise Unknown_Code
 end

(*
	3.	functor Icmp_Types
*)

functor Icmp_Types (structure B: FOX_BASIS) =
 struct
(* RFC 792 *)
  val echo_reply_type = SW.n8 "0"
  val unreachable_type = SW.n8 "3"
  val source_quench_type = SW.n8 "4"
  val redirect_type = SW.n8 "5"
  val echo_type = SW.n8 "8"
(* RFC 1256, pp. 5, 6 *)
  val router_ad_type = SW.n8 "9"
  val router_solicitation_type = SW.n8 "10"
  val supported_router_entry_size = SW.n8 "2"
  val mask_high_two = SW.n32 "0xC0000000"
  val time_exceeded_type = SW.n8 "11"
  val transit_time_exceeded_code = SW.n8 "0"
  val reassembly_time_exceeded_code = SW.n8 "1"
  val parameter_problem_type = SW.n8 "12"
  val ptr_parameter_problem_code = SW.n8 "0"
  val missing_option_parameter_problem_code = SW.n8 "1"
  val time_stamp_type = SW.n8 "13"
  val time_stamp_reply_type = SW.n8 "14"
(* RFC 1122: these are obsolete. *)
  val info_request_type = SW.n8 "15"
  val info_reply_type = SW.n8 "16"
(* RFC 950, p. 10, p. 17 *)
  val mask_request_type = SW.n8 "17"
  val mask_reply_type = SW.n8 "18"
(* RFC 1393, p. 4 *)
  val traceroute_type = SW.n8 "30"
  val traceroute_fwd_code = SW.n8 "0"
  val traceroute_discard_code = SW.n8 "1"

  local
   val second_mult = SW.n32 "1000"

  in
   fun get_universal_time () =
        let val time = B.Time.time_of_day ()
	    val B.Time.Time {sec, usec} = time
	    val {years, months, days, weekdays, hours, minutes, seconds} =
	           B.Time.split_time sec
	    val seconds_since_midnight = hours * 3600 + minutes * 60 + seconds
	    val sec32 = FoxWord32.intToWord seconds_since_midnight
	    val usec32 = FoxWord32.intToWord usec
	    val ms_since_midnight =
	           FoxWord32.+ (FoxWord32.* (sec32, second_mult),
				FoxWord32.div (usec32, second_mult))
	in ms_since_midnight
	end
  end
 end

(*
	5.	functor Icmp_In
*)

functor Icmp_In (structure In: EXTERNAL
		 structure Ip_Header: IP_HEADER_EXTERN
		 structure Ip_Extern: EXTERN_KEY
		  sharing type Ip_Header.extern_in = Ip_Extern.extern_in = In.T
		      and type Ip_Header.cursor = Ip_Extern.cursor = int
		      and type Ip_Header.ip_number = Ip_Extern.T
		 structure Redirect: ICMP_REDIRECT
		 structure Unreachable: ICMP_UNREACHABLE
		  sharing type Redirect.code = Unreachable.code = FoxWord8.word
		 structure B: FOX_BASIS
		 val debug_level: int ref option): ICMP_INCOMING =
 struct
  exception Unknown_Code
  exception Bad_Checksum
  exception Bad_Router_Ad
  exception Icmp_In_Op_Not_Implemented

  fun makestring Unknown_Code = SOME "Unknown ICMP code"
    | makestring Bad_Checksum = SOME "Bad ICMP checksum"
    | makestring Icmp_In_Op_Not_Implemented =
       SOME "do not use functions from ICMP In"
    | makestring _ = NONE

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "icmpheader.fun(in)"
			   val makestring = makestring)

  type ip_number = Ip_Extern.T
  type ip_protocol = FoxWord8.word
  type ip_option = Ip_Header.Option.ip_option
  type redirect = Redirect.T
  type unreachable = Unreachable.T

  structure Data = In

  (* When a parameter problem message arrives, Icmp parses it out
     and represents it as a problem in the IP header, Ip options,
     or the data of the packet. *)
  datatype problem_specifier =
      Header of FoxWord8.word
    | Option of ip_option
    | End_Of_Options
    | Missing_Required_Option
    | Data of FoxWord8.word

  fun makestring_problem (Header position) =
       "header problem at byte index " ^ FoxMakestring.word8 position
    | makestring_problem (Option opt) =
       "option problem with option " ^ Ip_Header.Option.makestring opt
    | makestring_problem End_Of_Options =
       "option problem with end of options"
    | makestring_problem Missing_Required_Option = "missing required option(s)"
    | makestring_problem (Data position) =
       "data problem at byte index " ^ FoxMakestring.word8 position

  datatype icmp_in  =
(* these messages are normally passed to the transport layer. *)
      Unreachable of unreachable * Data.T
    | Transit_Time_Exceeded of Data.T
    | Reassembly_Time_Exceeded of Data.T
    | Parameter_Problem of problem_specifier * Data.T
    | Source_Quench of Data.T
    | Echo_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		     data: Data.T}
    | Time_Stamp_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
			   originate: FoxWord32.word,
			   receive: FoxWord32.word,
			   transmit: FoxWord32.word,
			   returned: FoxWord32.word}
    | Traceroute of {forwarded: bool, id: FoxWord16.word,
		     out_hops: FoxWord16.word, return_hops: FoxWord16.word,
		     out_speed: FoxWord32.word, out_mtu: FoxWord32.word}
(* these messages are normally handled automatically by ICMP. *)
    | Redirect of {reason: redirect, new_gateway: ip_number, header: Data.T}
    | Echo of {id: FoxWord16.word, sequence: FoxWord16.word, data: Data.T}
    | Time_Stamp of {id: FoxWord16.word, sequence: FoxWord16.word,
		     originate: FoxWord32.word, receive: FoxWord32.word}
    | Mask_Request of {id: FoxWord16.word, sequence: FoxWord16.word}
    | Mask_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		     address_mask: ip_number}
    | Router_Advertisement of
         {lifetime: FoxWord16.word,
	  addresses: {address: ip_number,
		      preference_level: FoxWord32.word} list}
    | Router_Solicitation
    | Obsolete				(* used for Information messages *)

  type T = icmp_in

  fun makestring_header packet =
       Data.makestring_max (packet, 72)

  fun makestring_gateways [] = ""
    | makestring_gateways ({address, preference_level} :: rest) =
       Ip_Extern.makestring address ^ "(" ^
       FoxMakestring.word32 preference_level ^ ")" ^
       (if rest <> [] then ", " ^ makestring_gateways rest else "")

  fun makestring (Unreachable (unreachable, header)) =
       "packet dropped because " ^  Unreachable.makestring unreachable ^
       ", header " ^ makestring_header header
    | makestring (Transit_Time_Exceeded header) =
       "packet dropped due to excessive time in transit, header " ^
       makestring_header header
    | makestring (Reassembly_Time_Exceeded header) =
       "packet dropped because fragment reassembly time exceeded, header " ^
       makestring_header header
    | makestring (Parameter_Problem (problem, header)) =
       "packet dropped due to " ^ makestring_problem problem ^
       ", header " ^ makestring_header header
    | makestring (Source_Quench header) =
       "source quench (reduce sending rate), header " ^
       makestring_header header
    | makestring (Redirect {reason, new_gateway, header}) =
       Redirect.makestring reason ^ " to " ^
       Ip_Extern.makestring new_gateway ^
       ", header " ^ makestring_header header
    | makestring (Echo {id, sequence, data}) =
       "echo: id " ^ FoxMakestring.word16 id ^
       ", sequence " ^ FoxMakestring.word16 sequence ^
       ", data " ^ Data.makestring_max (data, 20)
    | makestring (Echo_Reply {id, sequence, data}) =
       "echo reply: id " ^ FoxMakestring.word16 id ^
       ", sequence " ^ FoxMakestring.word16 sequence ^
       ", data " ^ Data.makestring_max (data, 20)
    | makestring (Traceroute {forwarded, id, out_hops, return_hops,
			      out_speed, out_mtu}) =
       (if forwarded then "forwarded " else "discarded ") ^
       "traceroute: id " ^ FoxMakestring.word16 id ^
       ", out_hops " ^ FoxMakestring.word16 out_hops ^
       ", return_hops " ^ FoxMakestring.word16 return_hops ^
       ", out_speed " ^ FoxMakestring.word32 out_speed ^
       ", out_mtu " ^ FoxMakestring.word32 out_mtu
    | makestring (Time_Stamp {id, sequence, originate, receive}) =
       "time stamp: id " ^ FoxMakestring.word16 id ^
       ", sequence " ^ FoxMakestring.word16 sequence ^
       ", originate " ^ FoxMakestring.word32 originate ^
       ", receive " ^ FoxMakestring.word32 receive
    | makestring (Time_Stamp_Reply {id, sequence, originate, receive,
				    transmit, returned}) =
       "time stamp reply: id " ^ FoxMakestring.word16 id ^
       ", sequence " ^ FoxMakestring.word16 sequence ^
       ", originate " ^ FoxMakestring.word32 originate ^
       ", receive " ^ FoxMakestring.word32 receive ^
       ", transmit " ^ FoxMakestring.word32 transmit ^
       ", returned " ^ FoxMakestring.word32 returned
    | makestring (Mask_Request {id, sequence}) =
       "mask request: id " ^ FoxMakestring.word16 id ^
       ", sequence " ^ FoxMakestring.word16 sequence
    | makestring (Mask_Reply {id, sequence, address_mask}) =
       "mask reply: id " ^ FoxMakestring.word16 id ^
       ", sequence " ^ FoxMakestring.word16 sequence ^
       ", mask " ^ Ip_Extern.makestring address_mask
    | makestring (Router_Advertisement {lifetime, addresses}) =
       "router advertisement: lifetime " ^ FoxMakestring.word16 lifetime ^
       ", gateways " ^ makestring_gateways addresses
    | makestring Router_Solicitation = "router solicitation"
    | makestring Obsolete = "obsolete"

  local
   structure Types = Icmp_Types (structure B = B)

   structure Word8 = 
       Protocol_Extern8 (structure In = In
			 structure Out = In
			 structure B = B)

   structure Word16 = 
       Protocol_Extern16_Big (structure In = In
			      structure Out = In
			      structure B = B)

   structure Word32 = 
       Protocol_Extern32_Big (structure In = In
			      structure Out = In
			      structure B = B)

   val zero_code = SW.n8 "0"
   val zero_checksum = SW.n16 "0"
   val ffff_checksum = SW.n16 "0xffff"

(* RFC 792, p. 12 *)
   fun parse_redirect (code, front, packet) =
        let val (gateway, _) = Ip_Extern.unmarshal (front, 0)
	    val reason = Redirect.decode code
	in Redirect {reason = reason, new_gateway = gateway,
		     header = packet}
	end

(* RFC 792, p. 8 *)
   fun parse_parameter_problem (front, packet) =
        let val (ptr, _) = Word8.unmarshal (front, 0)
	    val pointer = FoxWord8.wordToInt ptr
	    val (header, cursor) = Ip_Header.unmarshal (packet, 0)
	    val problem = case Ip_Header.identify_pointer (header, pointer) of
	                     Ip_Header.Header =>
			      Header (FoxWord8.intToWord pointer)
	                   | (Ip_Header.Option NONE) => End_Of_Options
	                   | (Ip_Header.Option (SOME option)) => Option option
	                   | Ip_Header.Data =>
			      Data (FoxWord8.intToWord (pointer - cursor))
	in Parameter_Problem (problem, packet)
	end

(* RFC 1122, p. 42 *)
   fun missing_option header =
        Parameter_Problem (Missing_Required_Option, header)

   fun id_seq packet =
        let val (id, cursor) = Word16.unmarshal (packet, 0)
	    val (seq, _) = Word16.unmarshal (packet, cursor)
	in (id, seq)
	end

(* RFC 792, p. 14 *)
   fun build_echo ((id, seq), packet, constructor) =
        constructor {id = id, sequence = seq, data = packet}

(* RFC 792, p. 16 *)
   fun parse_time_stamp ((id, seq), packet) =
        let val (orig, cursor) = Word32.unmarshal (packet, 0)
	in Time_Stamp {id = id, sequence = seq, originate = orig,
		       receive = Types.get_universal_time ()}
	end

   fun parse_time_stamp_reply ((id, seq), header) =
        let val (orig, c1) = Word32.unmarshal (header, 0)
            val (receive, c2) = Word32.unmarshal (header, c1)
            val (transmit, _) = Word32.unmarshal (header, c2)
	in Time_Stamp_Reply {id = id, sequence = seq, originate = orig,
			     receive = receive, transmit = transmit,
			     returned = Types.get_universal_time ()}
	end

(* RFC 950, p. 10 *)
   fun parse_mask_request (id, seq) =
	Mask_Request {id = id, sequence = seq}

   fun parse_mask_reply ((id, seq), header) =
        let val (mask, _) = Ip_Extern.unmarshal (header, 0)
	in Mask_Reply {id = id, sequence = seq, address_mask = mask}
	end

(* RFC 1256, p. 5 *)
(* RFC 1256, p. 14: ignore any data after address/preference. *)
   fun parse_router_addrs (data, _, skip, 0) = []
     | parse_router_addrs (data, pos, skip, count) =
        let val (address, pos1) = Word32.unmarshal (data, pos)
	    val (preference, final_pos) = Word32.unmarshal (data, pos1)
	in {address = address, preference_level = preference} ::
	   parse_router_addrs (data, final_pos + skip, skip, count - 1)
	end

   val zero8 = SW.n8 "0"
   fun parse_router_ad data =
        let val (num_addrs, addr_entry_pos) = Word8.unmarshal (data, 0)
	    val (addr_entry_size, lifetime_pos) =
	            Word8.unmarshal (data, addr_entry_pos)
	    val (lifetime, start_pos) = Word16.unmarshal (data, lifetime_pos)
	    val skip8 = FoxWord8.- (addr_entry_size,
				    Types.supported_router_entry_size)
	    val skip = FoxWord8.wordToInt skip8
	    val addrs = ((parse_router_addrs (data, start_pos, skip,
					      FoxWord8.wordToInt num_addrs))
			 handle x =>
	                         Trace.print_raise_again
				    (x, SOME "parse_router_ad"))
(* check packet: RFC 1256, p. 13 *)
	in if num_addrs = zero8 orelse
	      FoxWord8.< (addr_entry_size,
			  Types.supported_router_entry_size) then
	    Trace.print_raise (Bad_Router_Ad, SOME "parse_router_ad")
	   else
	    Router_Advertisement {lifetime = lifetime, addresses = addrs}
	end

(* RFC 1393, p. 4 *)

   fun parse_traceroute (forwarded, data) =
        let val (id_number, cur1) = Word16.unmarshal (data, 0)
            val (_, cur2) = Word16.unmarshal (data, cur1) (* unused *)
            val (outbound, cur3) = Word16.unmarshal (data, cur2)
            val (return, cur4) = Word16.unmarshal (data, cur3)
            val (speed, cur5) = Word32.unmarshal (data, cur4)
            val (mtu, _) = Word32.unmarshal (data, cur5)
	in Traceroute {forwarded = forwarded, id = id_number,
		       out_hops = outbound, return_hops = return,
		       out_speed = speed, out_mtu = mtu}
	end

  in
   fun unmarshal data =
        ((let fun check array =
	           B.Checksum.complete_partial
		       (In.fold (array, B.Checksum.check_partial,
				 B.Checksum.initial_state))
	      val checksum = check data
	      val (type_val, t_cursor) = Word8.unmarshal (data, 0)
              val (code, c_cursor) = Word8.unmarshal (data, t_cursor)
	      val (_, all_data) = In.split (data, 4)
	      val (id, seq) = id_seq all_data
	      val (front, rest) = In.split (all_data, 4)
	      val (icmp_header, _) = In.split (data, 8)
	  in if checksum <> zero_checksum andalso
		checksum <> ffff_checksum then
	      (Trace.local_print ("icmpheader, total checksum is " ^
				  FoxMakestring.word16 checksum ^
				  ", packet is " ^
				  In.makestring_max (data, 50));
	       Trace.print_raise (Bad_Checksum, NONE))
	     else if type_val = Types.unreachable_type then
	      Unreachable (Unreachable.decode (code, seq), rest)
	     else if type_val = Types.time_exceeded_type then
	      if code = Types.transit_time_exceeded_code then
	       Transit_Time_Exceeded rest
	      else if code = Types.reassembly_time_exceeded_code then
	       Reassembly_Time_Exceeded rest
	      else Trace.print_raise (Unknown_Code,
				      SOME ("time exceeded code is " ^
					    FoxMakestring.word8 code))
	     else if type_val = Types.source_quench_type then
	      Source_Quench rest
	     else if type_val = Types.redirect_type then
	      parse_redirect (code, front, rest)
	     else if type_val = Types.parameter_problem_type then
	      if code = Types.ptr_parameter_problem_code then
	       parse_parameter_problem (front, rest)
	      else if code = Types.missing_option_parameter_problem_code then
	       missing_option rest
	      else
	       Trace.print_raise (Unknown_Code,
				  SOME ("ICMP parameter problem message, " ^
					", illegal code " ^
					FoxMakestring.word8 code))
	     else if type_val = Types.traceroute_type then
	      if code = Types.traceroute_fwd_code then
	       parse_traceroute (true, all_data)
	      else if code = Types.traceroute_discard_code then
	       parse_traceroute (false, all_data)
	      else
	       Trace.print_raise (Unknown_Code,
				  SOME ("ICMP traceroute message, " ^
					", illegal code " ^
					FoxMakestring.word8 code))
(* RFC 792, parameter problem, echo, timestamp all have code 0.
   RFC 950, address mask request/reply have code 0.
   RFC 1122, p. 42, parameter problem can have code 0 or code 1. *)
	     else if code <> zero_code then
	      Trace.print_raise (Unknown_Code,
				 SOME ("ICMP message type " ^
				       FoxMakestring.word8 type_val ^
				       ", non-zero code " ^
				       FoxMakestring.word8 code))
	     else if type_val = Types.echo_type then
	      build_echo (id_seq front, rest, Echo)
	     else if type_val = Types.echo_reply_type then
	      build_echo (id_seq front, rest, Echo_Reply)
	     else if type_val = Types.time_stamp_type then
	      parse_time_stamp (id_seq front, rest)
	     else if type_val = Types.time_stamp_reply_type then
	      parse_time_stamp_reply (id_seq front, rest)
	     else if type_val = Types.mask_request_type then
	      parse_mask_request (id_seq front)
	     else if type_val = Types.mask_reply_type then
	      parse_mask_reply (id_seq front, rest)
	     else if type_val = Types.mask_reply_type then
	      parse_mask_reply (id_seq front, rest)
	     else if type_val = Types.router_ad_type then
	      parse_router_ad all_data
	     else if type_val = Types.router_solicitation_type then
	      Router_Solicitation
	     else if type_val = Types.info_request_type orelse
	             type_val = Types.info_reply_type then Obsolete
	     else
	      Trace.print_raise (Unknown_Code,
				 SOME ("ICMP message type " ^
				       FoxMakestring.word8 type_val))
	  end)
	   handle x =>
	           Trace.print_raise_again (x, SOME "unmarshal"))
  end (* local *)

  fun new _ = Trace.print_raise (Icmp_In_Op_Not_Implemented, NONE)
  val uninitialized = new
  val size = new
  val sub = new
  val update = new
  val join = new
  val split = new
  val fold = new
  fun makestring_max (v, _) = makestring v

 end

(*
	7.	functor Icmp_Out
*)

functor Icmp_Out (structure Out: EXTERNAL
		  structure Ip_Extern: EXTERN_KEY
		   sharing type Ip_Extern.extern_out = Out.T
		       and type Ip_Extern.cursor = int
		  structure Redirect: ICMP_REDIRECT
		  structure Unreachable: ICMP_UNREACHABLE
		   sharing type Redirect.code = Unreachable.code
                              = FoxWord8.word
		  structure B: FOX_BASIS
		  val debug_level: int ref option): ICMP_OUTGOING =
 struct
  exception Unknown_Code
  exception Icmp_Out_Op_Not_Implemented

  fun makestring Unknown_Code = SOME "Unknown ICMP code"
    | makestring Icmp_Out_Op_Not_Implemented =
       SOME "do not use functions from ICMP Out"
    | makestring _ = NONE

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "icmpheader.fun(out)"
			   val makestring = makestring)

  type ip_number = Ip_Extern.T
  type unreachable = Unreachable.T

  structure Data = Out

  datatype icmp_message  =
      Unreachable of unreachable * Data.T
    | Transit_Time_Exceeded of Data.T
    | Reassembly_Time_Exceeded of Data.T
    | Parameter_Problem of {pointer: FoxWord8.word, data: Data.T}
    | Missing_Required_Option of Data.T
    | Source_Quench of Data.T
    | Echo of {id: FoxWord16.word, sequence: FoxWord16.word, data: Data.T}
    | Echo_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		     data: Data.T}
    | Time_Stamp of {id: FoxWord16.word, sequence: FoxWord16.word}
    | Time_Stamp_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
			   originate: FoxWord32.word,
			   receive: FoxWord32.word}
    | Traceroute of {forwarded: bool, id: FoxWord16.word,
		     out_hops: FoxWord16.word, return_hops: FoxWord16.word,
		     out_speed: FoxWord32.word, out_mtu: FoxWord32.word}
    | Mask_Request of {id: FoxWord16.word, sequence: FoxWord16.word}
    | Mask_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		     address_mask: ip_number}
    | Router_Advertisement of
         {lifetime: FoxWord16.word,
	  addresses: {address: ip_number,
		      preference_level: FoxWord32.word} list}
    | Router_Solicitation
  type T = icmp_message

  val max_ip_header = 60
  val max_print = max_ip_header + 8

  fun makestring_gateways [] = ""
    | makestring_gateways ({address, preference_level} :: rest) =
       Ip_Extern.makestring address ^ "(" ^
       FoxMakestring.word32 preference_level ^ ")" ^
       (case rest of [] => ", " ^ makestring_gateways rest | _ => "")

  fun makestring (Unreachable (unreachable, header)) =
       "packet dropped because " ^  Unreachable.makestring unreachable ^
       ", header " ^ Data.makestring_max (header, max_print)
    | makestring (Transit_Time_Exceeded header) =
       "packet dropped due to excessive time in transit, header " ^
       Data.makestring_max (header, max_print)
    | makestring (Reassembly_Time_Exceeded header) =
       "packet dropped because fragment reassembly time exceeded, header " ^
       Data.makestring_max (header, max_print)
    | makestring (Parameter_Problem {pointer, data}) =
       "packet dropped due to problem at index " ^
       FoxMakestring.word8 pointer ^
       " of header " ^ Data.makestring_max (data, max_print)
    | makestring (Missing_Required_Option data) =
       "packet dropped due to missing required option in header " ^
       Data.makestring_max (data, max_print)
    | makestring (Source_Quench header) =
       "source quench (reduce sending rate), header " ^
       Data.makestring_max (header, max_print)
    | makestring (Echo {id, sequence, data}) =
       "echo: id " ^ FoxMakestring.word16 id ^
       ", sequence " ^ FoxMakestring.word16 sequence ^
       ", data " ^ Data.makestring_max (data, 20)
    | makestring (Echo_Reply {id, sequence, data}) =
       "echo reply: id " ^ FoxMakestring.word16 id ^
       ", sequence " ^ FoxMakestring.word16 sequence ^
       ", data " ^ Data.makestring_max (data, 20)
    | makestring (Traceroute {forwarded, id, out_hops, return_hops,
			      out_speed, out_mtu}) =
       (if forwarded then "forwarded " else "discarded ") ^
       "traceroute: id " ^ FoxMakestring.word16 id ^
       ", out_hops " ^ FoxMakestring.word16 out_hops ^
       ", return_hops " ^ FoxMakestring.word16 return_hops ^
       ", out_speed " ^ FoxMakestring.word32 out_speed ^
       ", out_mtu " ^ FoxMakestring.word32 out_mtu
    | makestring (Time_Stamp {id, sequence}) =
       "time stamp: id " ^ FoxMakestring.word16 id ^
       ", sequence " ^ FoxMakestring.word16 sequence
    | makestring (Time_Stamp_Reply {id, sequence, originate, receive}) =
       "time stamp reply: id " ^ FoxMakestring.word16 id ^
       ", sequence " ^ FoxMakestring.word16 sequence ^
       ", originate " ^ FoxMakestring.word32 originate ^
       ", receive " ^ FoxMakestring.word32 receive
    | makestring (Mask_Request {id, sequence}) =
       "mask request: id " ^ FoxMakestring.word16 id ^
       ", sequence " ^ FoxMakestring.word16 sequence
    | makestring (Mask_Reply {id, sequence, address_mask}) =
       "mask reply: id " ^ FoxMakestring.word16 id ^
       ", sequence " ^ FoxMakestring.word16 sequence ^
       ", mask " ^ Ip_Extern.makestring address_mask
    | makestring (Router_Advertisement {lifetime, addresses}) =
       "router advertisement: lifetime " ^ FoxMakestring.word16 lifetime ^
       ", gateways " ^ makestring_gateways addresses
    | makestring Router_Solicitation = "router solicitation"

  local
   structure Types = Icmp_Types (structure B = B)

   structure Word8 = 
       Protocol_Extern8 (structure In = Out
			 structure Out = Out
			 structure B = B)

   structure Word16 = 
       Protocol_Extern16_Big (structure In = Out
			      structure Out = Out
			      structure B = B)

   structure Word32 = 
       Protocol_Extern32_Big (structure In = Out
			      structure Out = Out
			      structure B = B)

   structure Word64 = 
       Protocol_Extern64_Big (structure In = Out
			      structure Out = Out
			      structure B = B)

   val standard_size = 8
   val id_seq_size = 4
   val time_stamp_size = 12
   val checksum_cursor = 2
   val initial_checksum = SW.n16 "0"
   val zero8 = SW.n8 "0"
   val zero32 = SW.n32 "0"

   fun checksum_packet packet =
        let val init = B.Checksum.initial_state
	    val fold = Out.fold (packet, B.Checksum.check_partial, init)
	    val check = B.Checksum.complete_partial fold
	    val invert = B.Checksum.one_s_complement check
	in Word16.marshal (packet, invert) checksum_cursor;
	   packet
	end

   fun marshal_standard_header (type_val, code, second_word, packet) =
        let val header = Out.uninitialized standard_size
	    val marshal = (Word32.marshal (header, second_word) o
			   Word16.marshal (header, initial_checksum) o
			   Word8.marshal (header, code) o
			   Word8.marshal (header, type_val))
	in marshal 0;
	   checksum_packet (Out.join (header, packet))
	end

   fun marshal_zero (type_val, code, packet) =
        marshal_standard_header (type_val, code, zero32, packet)

   fun marshal_pointer (type_val, code, pointer, packet) =
        let val word = Out.uninitialized id_seq_size
	    val _ = (Word8.marshal (word, zero8) o
		     Word8.marshal (word, zero8) o
		     Word8.marshal (word, zero8) o
		     Word8.marshal (word, pointer)) 0
	    val (word_val, _) = Word32.unmarshal (word, 0)
	in marshal_standard_header (type_val, code, word_val, packet)
	end

   fun marshal_unreachable (type_val, (code, mtu), packet) =
        let val word_buf = Out.new (Word_Array.from8
				    (Word_Array.W8.U_Big.F.create (zero8, 4)))
	    val _ = Word16.marshal (word_buf, mtu) 2
	    val (second_word, _) = Word32.unmarshal (word_buf, 0)
	in marshal_standard_header (type_val, code, second_word, packet)
	end

   fun marshal_id_seq (type_val, code, id, seq, packet) =
        let val word = Out.uninitialized id_seq_size
	    val _ = (Word16.marshal (word, seq) o Word16.marshal (word, id)) 0
	    val (word_val, _) = Word32.unmarshal (word, 0)
	in marshal_standard_header (type_val, code, word_val, packet)
	end

   fun marshal_time_stamp (type_val, code, id, seq, t1, t2, t3) =
        let val data = Out.uninitialized time_stamp_size
	    val marshal = (Word32.marshal (data, t3) o
			   Word32.marshal (data, t2) o
			   Word32.marshal (data, t1))
	in marshal 0;
	   marshal_id_seq (type_val, code, id, seq, data)
	end

   fun marshal_address_mask (type_val, code, id, seq, mask) =
        let val zero_ip = Word_Array.from8 (Word_Array.W8.U_Big.F.create
					    (zero8, id_seq_size))
	    val data = Out.new zero_ip
	in case mask of
	      NONE => ()
	    | SOME m => (Ip_Extern.marshal (data, m) 0; ());
	   marshal_id_seq (type_val, code, id, seq, data)
	end

   fun marshal_router_ad  (type_val, lifetime, addresses) =
        let fun compute_address {address, preference_level} =
	         let val buffer = Out.new (Word_Array.from8
					   (Word_Array.W8.U_Big.F.create
					    (zero8, 8)))
		 in (Word32.marshal (buffer, preference_level) o
		     Ip_Extern.marshal (buffer, address)) 0;
		    #1 (Word64.unmarshal (buffer, 0))
		 end
	    fun gen [] = NONE
	      | gen (head :: rest) = SOME (compute_address head, rest)
	    val num_addrs = FoxWord8.intToWord (B.V.List.length addresses)
	    val a64 = Word_Array.W64.U_Big.F.new gen addresses
	    val a8 = Out.new (Word_Array.from64 a64)
	    val word_buf = Out.uninitialized id_seq_size
	    val _ = (Word16.marshal (word_buf, lifetime) o
		     Word8.marshal (word_buf,
				    Types.supported_router_entry_size) o
		     Word8.marshal (word_buf, num_addrs)) 0
	    val (second_word, _) = Word32.unmarshal (word_buf, 0)
	in marshal_standard_header (type_val, zero8, second_word, a8)
	end

   fun marshal_router_sol type_val =
	marshal_standard_header (type_val, zero8, zero32, Out.uninitialized 0)

(* RFC 1393, p. 4 *)
   fun marshal_traceroute (type_val, code, id, out, return, speed, mtu) =
        let val word_buf = Out.new (Word_Array.from8
				    (Word_Array.W8.U_Big.F.create (zero8, 4)))
            val data = Out.uninitialized 12
	    val _ = Word16.marshal (word_buf, id) 0
	    val (second_word, _) = Word32.unmarshal (word_buf, 0)
	in (Word32.marshal (data, mtu) o
	    Word32.marshal (data, speed) o
	    Word16.marshal (data, return) o
	    Word16.marshal (data, out)) 0;
	   marshal_standard_header (type_val, code, second_word, data)
	end

  in
   fun marshal (Unreachable (unreachable, packet)) =
	marshal_unreachable (Types.unreachable_type,
			     Unreachable.code unreachable, packet)
     | marshal (Transit_Time_Exceeded packet) =
	marshal_zero (Types.time_exceeded_type,
		      Types.transit_time_exceeded_code,
		      packet)
     | marshal (Reassembly_Time_Exceeded packet) =
	marshal_zero (Types.time_exceeded_type,
		      Types.reassembly_time_exceeded_code,
		      packet)
     | marshal (Parameter_Problem {pointer, data}) =
        marshal_pointer (Types.parameter_problem_type,
			 Types.ptr_parameter_problem_code,
			 pointer, data)
     | marshal (Missing_Required_Option data) =
        marshal_zero (Types.parameter_problem_type,
		      Types.missing_option_parameter_problem_code, data)
     | marshal (Source_Quench packet) =
	marshal_zero (Types.source_quench_type, zero8, packet)
     | marshal (Echo {id, sequence, data}) =
        marshal_id_seq (Types.echo_type, zero8, id, sequence, data)
     | marshal (Echo_Reply {id, sequence, data}) =
        marshal_id_seq (Types.echo_reply_type, zero8, id, sequence,
			       data)
     | marshal (Traceroute {forwarded, id, out_hops, return_hops,
			    out_speed, out_mtu}) =
	marshal_traceroute (Types.traceroute_type,
			    if forwarded then Types.traceroute_fwd_code
			    else Types.traceroute_discard_code,
			    id, out_hops, return_hops, out_speed, out_mtu)
     | marshal (Time_Stamp {id, sequence}) =
        marshal_time_stamp (Types.time_stamp_type, zero8, id, sequence,
			    Types.get_universal_time (), zero32, zero32)
     | marshal (Time_Stamp_Reply {id, sequence, originate, receive}) =
        marshal_time_stamp (Types.time_stamp_reply_type, zero8, id, sequence,
			    originate, receive, Types.get_universal_time ())
     | marshal (Mask_Request {id, sequence}) =
        marshal_address_mask (Types.mask_request_type, zero8, id, sequence,
			      NONE)
     | marshal (Mask_Reply {id, sequence, address_mask}) =
        marshal_address_mask (Types.mask_reply_type, zero8, id, sequence,
			      SOME address_mask)
     | marshal (Router_Advertisement {lifetime, addresses}) =
        marshal_router_ad (Types.router_ad_type, lifetime, addresses)
     | marshal Router_Solicitation =
        marshal_router_sol Types.router_solicitation_type

  end (* local *)

  fun new _ = Trace.print_raise (Icmp_Out_Op_Not_Implemented, NONE)
  val uninitialized = new
  val size = new
  val sub = new
  val update = new
  val join = new
  val split = new
  val fold = new
  fun makestring_max (v, _) = makestring v

 end
