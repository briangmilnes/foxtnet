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
	4.	functor Icmp_In
	5.	functor Icmp_Out

	iii.	RCS Log
	
$Log: icmpheader.fun,v $
Revision 1.17  1997/12/11  19:49:22  esb
we no longer print an error message for router ads with code 16,
not standard (yet) but found on CMU networks.

Revision 1.16  97/11/19  13:50:11  cline
109.32 compatibility

Revision 1.15  96/05/28  19:08:02  esb
now ignores an experimental protocol used at CMU.

Revision 1.14  1996/04/18  21:18:51  cline
updated to match new TIME signature

Revision 1.13  1996/02/23  21:32:54  esb
modified a print statment.

Revision 1.12  1996/01/19  23:02:29  esb
adapted to the new wordarray signature.

Revision 1.11  1996/01/15  18:00:49  cline
removed FoxWord

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

  type code = Word8.word
(* RFC 792, p. 12 *)
  val network_redirect_code = 0w0 : Word8.word
  val host_redirect_code = 0w1 : Word8.word
  val tos_network_redirect_code = 0w2 : Word8.word
  val tos_host_redirect_code = 0w3 : Word8.word

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
    | Fragmentation_Needed of {mtu: Word16.word}
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
       (Integer.toString o Word16.toInt) mtu
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

  type code = Word8.word
(* RFC 792, p. 4 *)
  val network_unreachable_code = 0w0 : Word8.word
  val host_unreachable_code = 0w1 : Word8.word
  val protocol_unreachable_code = 0w2 : Word8.word
  val port_unreachable_code = 0w3 : Word8.word
  val fragmentation_needed_code = 0w4 : Word8.word
  val source_route_failed_code = 0w5 : Word8.word
(* RFC 1122, p. 39 ff. *)
  val network_unknown_code = 0w6 : Word8.word
  val host_unknown_code = 0w7 : Word8.word
  val source_host_isolated_code = 0w8 : Word8.word
  val communication_with_network_prohibited_code = 0w9 : Word8.word
  val communication_with_host_prohibited_code = 0w10 : Word8.word
  val network_unreachable_for_tos_code = 0w11 : Word8.word
  val host_unreachable_for_tos_code = 0w12 : Word8.word

  val zero16 = Word16.fromInt 0

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
  val echo_reply_type = 0w0 : Word8.word
  val unreachable_type = 0w3 : Word8.word
  val source_quench_type = 0w4 : Word8.word
  val redirect_type = 0w5 : Word8.word
  val echo_type = 0w8 : Word8.word
(* RFC 1256, pp. 5, 6 *)
  val router_ad_type = 0w9 : Word8.word
  val router_solicitation_type = 0w10 : Word8.word
  val supported_router_entry_size = 0w2 : Word8.word
  val router_ad_code = 0w0 : Word8.word
  val router_ad_ignore_code = 0w16 : Word8.word
  val mask_high_two = (* 0wxC0000000 : Word32.word *)
        Word32.* (Word32.fromInt 0xC00000, Word32.fromInt 0x100)
  val time_exceeded_type = 0w11 : Word8.word
  val transit_time_exceeded_code =  0w0 : Word8.word
  val reassembly_time_exceeded_code = 0w1 : Word8.word
  val parameter_problem_type = 0w12 : Word8.word
  val ptr_parameter_problem_code = 0w0 : Word8.word
  val missing_option_parameter_problem_code = 0w1 : Word8.word
  val time_stamp_type = 0w13 : Word8.word
  val time_stamp_reply_type = 0w14 : Word8.word
(* RFC 1122: these are obsolete. *)
  val info_request_type = 0w15 : Word8.word
  val info_reply_type = 0w16 : Word8.word
(* RFC 950, p. 10, p. 17 *)
  val mask_request_type = 0w17 : Word8.word
  val mask_reply_type = 0w18 : Word8.word
(* RFC 1393, p. 4 *)
  val traceroute_type = 0w30 : Word8.word
  val traceroute_fwd_code = 0w0 : Word8.word
  val traceroute_discard_code = 0w1 : Word8.word

  local
   val second_mult = 0w1000 : Word32.word

  in
   fun get_universal_time () =
        let val {years, months, days, weekdays, hours, minutes, seconds,
		 microseconds} = B.V.Time.split (B.V.Time.now ())
	    val seconds_since_midnight = hours * 3600 + minutes * 60 + seconds
	    val sec32 = Word32.fromInt seconds_since_midnight
	    val usec32 = Word32.fromInt microseconds
	    val ms_since_midnight =
	           Word32.+ (Word32.* (sec32, second_mult),
				Word32.div (usec32, second_mult))
	in ms_since_midnight
	end
  end
 end

(*
	4.	functor Icmp_In
*)

functor Icmp_In (structure In: EXTERNAL
		 structure Ip_Header: IP_HEADER_EXTERN
		   where type extern_in = In.T
		     and type cursor = Word.word
		 structure Ip_Extern: EXTERN_KEY
		   where type extern_in = In.T
		     and type cursor = Word.word
		   sharing type Ip_Header.ip_number = Ip_Extern.T
		 structure Redirect: ICMP_REDIRECT
		   where type code = Word8.word
		 structure Unreachable: ICMP_UNREACHABLE
		   where type code = Word8.word
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
  type ip_protocol = Word8.word
  type ip_option = Ip_Header.Option.ip_option
  type redirect = Redirect.T
  type unreachable = Unreachable.T

  structure Data = In

  (* When a parameter problem message arrives, Icmp parses it out
     and represents it as a problem in the IP header, Ip options,
     or the data of the packet. *)
  datatype problem_specifier =
      Header of Word8.word
    | Option of ip_option
    | End_Of_Options
    | Missing_Required_Option
    | Data of Word8.word

  fun makestring_problem (Header position) =
       "header problem at byte index " ^ Word8.fmt StringCvt.DEC position
    | makestring_problem (Option opt) =
       "option problem with option " ^ Ip_Header.Option.makestring opt
    | makestring_problem End_Of_Options =
       "option problem with end of options"
    | makestring_problem Missing_Required_Option = "missing required option(s)"
    | makestring_problem (Data position) =
       "data problem at byte index " ^ Word8.fmt StringCvt.DEC position

  datatype icmp_in  =
(* these messages are normally passed to the transport layer. *)
      Unreachable of unreachable * Data.T
    | Transit_Time_Exceeded of Data.T
    | Reassembly_Time_Exceeded of Data.T
    | Parameter_Problem of problem_specifier * Data.T
    | Source_Quench of Data.T
    | Echo_Reply of {id: Word16.word, sequence: Word16.word,
		     data: Data.T}
    | Time_Stamp_Reply of {id: Word16.word, sequence: Word16.word,
			   originate: Word32.word,
			   receive: Word32.word,
			   transmit: Word32.word,
			   returned: Word32.word}
    | Traceroute of {forwarded: bool, id: Word16.word,
		     out_hops: Word16.word, return_hops: Word16.word,
		     out_speed: Word32.word, out_mtu: Word32.word}
(* these messages are normally handled automatically by ICMP. *)
    | Redirect of {reason: redirect, new_gateway: ip_number, header: Data.T}
    | Echo of {id: Word16.word, sequence: Word16.word, data: Data.T}
    | Time_Stamp of {id: Word16.word, sequence: Word16.word,
		     originate: Word32.word, receive: Word32.word}
    | Mask_Request of {id: Word16.word, sequence: Word16.word}
    | Mask_Reply of {id: Word16.word, sequence: Word16.word,
		     address_mask: ip_number}
    | Router_Advertisement of
         {lifetime: Word16.word,
	  addresses: {address: ip_number,
		      preference_level: Word32.word} list}
    | Experimental_Router_Ad		(* some experiment going on at CMU *)
    | Router_Solicitation
    | Obsolete				(* used for Information messages *)

  type T = icmp_in

  fun makestring_header packet =
       Data.makestring_max (packet, 0w72)

  fun makestring_gateways [] = ""
    | makestring_gateways ({address, preference_level} :: rest) =
       Ip_Extern.makestring address ^ "(" ^
       Word32.fmt StringCvt.DEC preference_level ^ ")" ^
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
       "echo: id " ^ (Integer.toString o Word16.toInt) id ^
       ", sequence " ^ (Integer.toString o Word16.toInt) sequence ^
       ", data " ^ Data.makestring_max (data, 0w20)
    | makestring (Echo_Reply {id, sequence, data}) =
       "echo reply: id " ^ (Integer.toString o Word16.toInt) id ^
       ", sequence " ^ (Integer.toString o Word16.toInt) sequence ^
       ", data " ^ Data.makestring_max (data, 0w20)
    | makestring (Traceroute {forwarded, id, out_hops, return_hops,
			      out_speed, out_mtu}) =
       (if forwarded then "forwarded " else "discarded ") ^
       "traceroute: id " ^ (Integer.toString o Word16.toInt) id ^
       ", out_hops " ^ (Integer.toString o Word16.toInt) out_hops ^
       ", return_hops " ^ (Integer.toString o Word16.toInt) return_hops ^
       ", out_speed " ^ Word32.fmt StringCvt.DEC out_speed ^
       ", out_mtu " ^ Word32.fmt StringCvt.DEC out_mtu
    | makestring (Time_Stamp {id, sequence, originate, receive}) =
       "time stamp: id " ^ (Integer.toString o Word16.toInt) id ^
       ", sequence " ^ (Integer.toString o Word16.toInt) sequence ^
       ", originate " ^ Word32.fmt StringCvt.DEC originate ^
       ", receive " ^ Word32.fmt StringCvt.DEC receive
    | makestring (Time_Stamp_Reply {id, sequence, originate, receive,
				    transmit, returned}) =
       "time stamp reply: id " ^ (Integer.toString o Word16.toInt) id ^
       ", sequence " ^ (Integer.toString o Word16.toInt) sequence ^
       ", originate " ^ Word32.fmt StringCvt.DEC originate ^
       ", receive " ^ Word32.fmt StringCvt.DEC receive ^
       ", transmit " ^ Word32.fmt StringCvt.DEC transmit ^
       ", returned " ^ Word32.fmt StringCvt.DEC returned
    | makestring (Mask_Request {id, sequence}) =
       "mask request: id " ^ (Integer.toString o Word16.toInt) id ^
       ", sequence " ^ (Integer.toString o Word16.toInt) sequence
    | makestring (Mask_Reply {id, sequence, address_mask}) =
       "mask reply: id " ^ (Integer.toString o Word16.toInt) id ^
       ", sequence " ^ (Integer.toString o Word16.toInt) sequence ^
       ", mask " ^ Ip_Extern.makestring address_mask
    | makestring (Router_Advertisement {lifetime, addresses}) =
       "router advertisement: lifetime " ^
       (Integer.toString o Word16.toInt) lifetime ^
       ", gateways " ^ makestring_gateways addresses
    | makestring Experimental_Router_Ad = "experimental router advertisement"
    | makestring Router_Solicitation = "router solicitation"
    | makestring Obsolete = "obsolete"

  local
   structure Types = Icmp_Types (structure B = B)

   structure Marshal_Word8 = 
       Protocol_Extern8 (structure In = In
			 structure Out = In
			 structure B = B)

   structure Marshal_Word16 = 
       Protocol_Extern16_Big (structure In = In
			      structure Out = In
			      structure B = B)

   structure Marshal_Word32 = 
       Protocol_Extern32_Big (structure In = In
			      structure Out = In
			      structure B = B)

   val zero_code = 0w0 : Word8.word
   val zero_checksum = Word16.fromInt 0
   val ffff_checksum = Word16.fromInt 0xffff

(* RFC 792, p. 12 *)
   fun parse_redirect (code, front, packet) =
        let val (gateway, _) = Ip_Extern.unmarshal (front, 0w0)
	    val reason = Redirect.decode code
	in Redirect {reason = reason, new_gateway = gateway,
		     header = packet}
	end

(* RFC 792, p. 8 *)
   fun parse_parameter_problem (front, packet) =
        let val (ptr, _) = Marshal_Word8.unmarshal (front, 0w0)
	    val pointer = Word.fromInt (Word8.toInt ptr)
	    val (header, cursor) = Ip_Header.unmarshal (packet, 0w0)
	    val problem = case Ip_Header.identify_pointer (header, pointer) of
	                     Ip_Header.Header =>
			      Header (Word8.fromInt (Word.toInt pointer))
	                   | (Ip_Header.Option NONE) => End_Of_Options
	                   | (Ip_Header.Option (SOME option)) => Option option
	                   | Ip_Header.Data =>
			      Data (Word8.fromInt
				    (Word.toInt (pointer - cursor)))
	in Parameter_Problem (problem, packet)
	end

(* RFC 1122, p. 42 *)
   fun missing_option header =
        Parameter_Problem (Missing_Required_Option, header)

   fun id_seq packet =
        let val (id, cursor) = Marshal_Word16.unmarshal (packet, 0w0)
	    val (seq, _) = Marshal_Word16.unmarshal (packet, cursor)
	in (id, seq)
	end

(* RFC 792, p. 14 *)
   fun build_echo ((id, seq), packet, constructor) =
        constructor {id = id, sequence = seq, data = packet}

(* RFC 792, p. 16 *)
   fun parse_time_stamp ((id, seq), packet) =
        let val (orig, cursor) = Marshal_Word32.unmarshal (packet, 0w0)
	in Time_Stamp {id = id, sequence = seq, originate = orig,
		       receive = Types.get_universal_time ()}
	end

   fun parse_time_stamp_reply ((id, seq), header) =
        let val (orig, c1) = Marshal_Word32.unmarshal (header, 0w0)
            val (receive, c2) = Marshal_Word32.unmarshal (header, c1)
            val (transmit, _) = Marshal_Word32.unmarshal (header, c2)
	in Time_Stamp_Reply {id = id, sequence = seq, originate = orig,
			     receive = receive, transmit = transmit,
			     returned = Types.get_universal_time ()}
	end

(* RFC 950, p. 10 *)
   fun parse_mask_request (id, seq) =
	Mask_Request {id = id, sequence = seq}

   fun parse_mask_reply ((id, seq), header) =
        let val (mask, _) = Ip_Extern.unmarshal (header, 0w0)
	in Mask_Reply {id = id, sequence = seq, address_mask = mask}
	end

(* RFC 1256, p. 5 *)
(* RFC 1256, p. 14: ignore any data after address/preference. *)
   fun parse_router_addrs (data, _, skip, 0w0) = []
     | parse_router_addrs (data, pos, skip, count) =
        let val (address, pos1) = Marshal_Word32.unmarshal (data, pos)
	    val (preference, final_pos) = Marshal_Word32.unmarshal (data, pos1)
	in {address = address, preference_level = preference} ::
	   parse_router_addrs (data, final_pos + skip, skip, count - 0w1)
	end

   val zero8 = 0w0 : Word8.word
   fun parse_router_ad data =
        let val (num_addrs, addr_entry_pos) =
	            Marshal_Word8.unmarshal (data, 0w0)
	    val (addr_entry_size, lifetime_pos) =
	            Marshal_Word8.unmarshal (data, addr_entry_pos)
	    val (lifetime, start_pos) =
	            Marshal_Word16.unmarshal (data, lifetime_pos)
	    val skip8 = Word8.- (addr_entry_size,
				 Types.supported_router_entry_size)
	    val skip = Word.fromInt (Word8.toInt skip8)
	    val addrs = ((parse_router_addrs (data, start_pos, skip,
					      Word.fromInt
					       (Word8.toInt num_addrs)))
			 handle x =>
	                         Trace.print_raise_again
				    (x, SOME "parse_router_ad"))
(* check packet: RFC 1256, p. 13 *)
	in if num_addrs = zero8 orelse
	      Word8.< (addr_entry_size,
		       Types.supported_router_entry_size) then
	    (* there is some experiment going on at CMU, and we can
	       ignore these packets. *)
(*
	    Trace.print_raise (Bad_Router_Ad,
			       SOME ("parse_router_ad (" ^
				     In.makestring data ^ ")"))
*)
	    Experimental_Router_Ad
	   else
	    Router_Advertisement {lifetime = lifetime, addresses = addrs}
	end

(* RFC 1393, p. 4 *)

   fun parse_traceroute (forwarded, data) =
        let val (id_number, cur1) = Marshal_Word16.unmarshal (data, 0w0)
            val (_, cur2) = Marshal_Word16.unmarshal (data, cur1) (* unused *)
            val (outbound, cur3) = Marshal_Word16.unmarshal (data, cur2)
            val (return, cur4) = Marshal_Word16.unmarshal (data, cur3)
            val (speed, cur5) = Marshal_Word32.unmarshal (data, cur4)
            val (mtu, _) = Marshal_Word32.unmarshal (data, cur5)
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
	      val (type_val, t_cursor) = Marshal_Word8.unmarshal (data, 0w0)
              val (code, c_cursor) = Marshal_Word8.unmarshal (data, t_cursor)
	      val (_, all_data) = In.split (data, 0w4)
	      val (id, seq) = id_seq all_data
	      val (front, rest) = In.split (all_data, 0w4)
	      val (icmp_header, _) = In.split (data, 0w8)
	  in if checksum <> zero_checksum andalso
		checksum <> ffff_checksum then
	      (Trace.local_print ("icmpheader, total checksum is " ^
				  (Integer.toString o Word16.toInt) checksum ^
				  ", packet size " ^
				  (Integer.toString o Word.toInt o
				   In.size) data ^ ", data " ^
				  In.makestring data);
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
					    Word8.fmt StringCvt.DEC code))
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
					Word8.fmt StringCvt.DEC code))
	     else if type_val = Types.traceroute_type then
	      if code = Types.traceroute_fwd_code then
	       parse_traceroute (true, all_data)
	      else if code = Types.traceroute_discard_code then
	       parse_traceroute (false, all_data)
	      else
	       Trace.print_raise (Unknown_Code,
				  SOME ("ICMP traceroute message, " ^
					", illegal code " ^
					Word8.fmt StringCvt.DEC code))
(* RFC 950, address mask request/reply have code 0, but at CMU
   we are seeing a router ad with code 16, so we want to silently ignore it. *)
	     else if type_val = Types.router_ad_type then
	      if code = Types.router_ad_code then
	       parse_router_ad all_data
	      else if code = Types.router_ad_ignore_code then
	       Experimental_Router_Ad
	      else
	       Trace.print_raise (Unknown_Code,
				  SOME ("ICMP router ad, " ^
					", illegal code " ^
					Word8.fmt StringCvt.DEC code))
(* RFC 792, parameter problem, echo, timestamp all have code 0.
   RFC 950, address mask request/reply have code 0.
   RFC 1122, p. 42, parameter problem can have code 0 or code 1. *)
	     else if code <> zero_code then
	      Trace.print_raise (Unknown_Code,
				 SOME ("ICMP message type " ^
				       Word8.fmt StringCvt.DEC type_val ^
				       ", non-zero code " ^
				       Word8.fmt StringCvt.DEC code))
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
				       Word8.fmt StringCvt.DEC type_val))
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
	5.	functor Icmp_Out
*)

functor Icmp_Out (structure Out: EXTERNAL
		  structure Ip_Extern: EXTERN_KEY
		    where type extern_out = Out.T
		      and type cursor = Word.word
		  structure Redirect: ICMP_REDIRECT
		    where type code = Word8.word
		  structure Unreachable: ICMP_UNREACHABLE
		    where type code = Word8.word
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
    | Parameter_Problem of {pointer: Word8.word, data: Data.T}
    | Missing_Required_Option of Data.T
    | Source_Quench of Data.T
    | Echo of {id: Word16.word, sequence: Word16.word, data: Data.T}
    | Echo_Reply of {id: Word16.word, sequence: Word16.word,
		     data: Data.T}
    | Time_Stamp of {id: Word16.word, sequence: Word16.word}
    | Time_Stamp_Reply of {id: Word16.word, sequence: Word16.word,
			   originate: Word32.word,
			   receive: Word32.word}
    | Traceroute of {forwarded: bool, id: Word16.word,
		     out_hops: Word16.word, return_hops: Word16.word,
		     out_speed: Word32.word, out_mtu: Word32.word}
    | Mask_Request of {id: Word16.word, sequence: Word16.word}
    | Mask_Reply of {id: Word16.word, sequence: Word16.word,
		     address_mask: ip_number}
    | Router_Advertisement of
         {lifetime: Word16.word,
	  addresses: {address: ip_number,
		      preference_level: Word32.word} list}
    | Router_Solicitation
  type T = icmp_message

  val max_ip_header = 0w60
  val max_print = max_ip_header + 0w8

  fun makestring_gateways [] = ""
    | makestring_gateways ({address, preference_level} :: rest) =
       Ip_Extern.makestring address ^ "(" ^
       Word32.fmt StringCvt.DEC preference_level ^ ")" ^
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
       Word8.fmt StringCvt.DEC pointer ^
       " of header " ^ Data.makestring_max (data, max_print)
    | makestring (Missing_Required_Option data) =
       "packet dropped due to missing required option in header " ^
       Data.makestring_max (data, max_print)
    | makestring (Source_Quench header) =
       "source quench (reduce sending rate), header " ^
       Data.makestring_max (header, max_print)
    | makestring (Echo {id, sequence, data}) =
       "echo: id " ^ (Integer.toString o Word16.toInt) id ^
       ", sequence " ^ (Integer.toString o Word16.toInt) sequence ^
       ", data " ^ Data.makestring_max (data, 0w20)
    | makestring (Echo_Reply {id, sequence, data}) =
       "echo reply: id " ^ (Integer.toString o Word16.toInt) id ^
       ", sequence " ^ (Integer.toString o Word16.toInt) sequence ^
       ", data " ^ Data.makestring_max (data, 0w20)
    | makestring (Traceroute {forwarded, id, out_hops, return_hops,
			      out_speed, out_mtu}) =
       (if forwarded then "forwarded " else "discarded ") ^
       "traceroute: id " ^ (Integer.toString o Word16.toInt) id ^
       ", out_hops " ^ (Integer.toString o Word16.toInt) out_hops ^
       ", return_hops " ^ (Integer.toString o Word16.toInt) return_hops ^
       ", out_speed " ^ Word32.fmt StringCvt.DEC out_speed ^
       ", out_mtu " ^ Word32.fmt StringCvt.DEC out_mtu
    | makestring (Time_Stamp {id, sequence}) =
       "time stamp: id " ^ (Integer.toString o Word16.toInt) id ^
       ", sequence " ^ (Integer.toString o Word16.toInt) sequence
    | makestring (Time_Stamp_Reply {id, sequence, originate, receive}) =
       "time stamp reply: id " ^ (Integer.toString o Word16.toInt) id ^
       ", sequence " ^ (Integer.toString o Word16.toInt) sequence ^
       ", originate " ^ Word32.fmt StringCvt.DEC originate ^
       ", receive " ^ Word32.fmt StringCvt.DEC receive
    | makestring (Mask_Request {id, sequence}) =
       "mask request: id " ^ (Integer.toString o Word16.toInt) id ^
       ", sequence " ^ (Integer.toString o Word16.toInt) sequence
    | makestring (Mask_Reply {id, sequence, address_mask}) =
       "mask reply: id " ^ (Integer.toString o Word16.toInt) id ^
       ", sequence " ^ (Integer.toString o Word16.toInt) sequence ^
       ", mask " ^ Ip_Extern.makestring address_mask
    | makestring (Router_Advertisement {lifetime, addresses}) =
       "router advertisement: lifetime " ^
       (Integer.toString o Word16.toInt) lifetime ^
       ", gateways " ^ makestring_gateways addresses
    | makestring Router_Solicitation = "router solicitation"

  local
   structure Types = Icmp_Types (structure B = B)

   structure Marshal_Word8 = 
       Protocol_Extern8 (structure In = Out
			 structure Out = Out
			 structure B = B)

   structure Marshal_Word16 = 
       Protocol_Extern16_Big (structure In = Out
			      structure Out = Out
			      structure B = B)

   structure Marshal_Word32 = 
       Protocol_Extern32_Big (structure In = Out
			      structure Out = Out
			      structure B = B)

   structure Word64 = 
       Protocol_Extern64_Big (structure In = Out
			      structure Out = Out
			      structure B = B)

   val standard_size = 0w8
   val id_seq_size = 0w4
   val time_stamp_size = 0w12
   val checksum_cursor = 0w2
   val initial_checksum = Word16.fromInt 0

   fun checksum_packet packet =
        let val init = B.Checksum.initial_state
	    val fold = Out.fold (packet, B.Checksum.check_partial, init)
	    val check = B.Checksum.complete_partial fold
	    val invert = B.Checksum.one_s_complement check
	in Marshal_Word16.marshal (packet, invert) checksum_cursor;
	   packet
	end

   fun marshal_standard_header (type_val, code, second_word, packet) =
        let val header = Out.uninitialized standard_size
	    val marshal = (Marshal_Word32.marshal (header, second_word) o
			   Marshal_Word16.marshal (header, initial_checksum) o
			   Marshal_Word8.marshal (header, code) o
			   Marshal_Word8.marshal (header, type_val))
	in marshal 0w0;
	   checksum_packet (Out.join (header, packet))
	end

   fun marshal_zero (type_val, code, packet) =
        marshal_standard_header (type_val, code, Word32.fromInt 0, packet)

   fun marshal_pointer (type_val, code, pointer, packet) =
        let val word = Out.uninitialized id_seq_size
	    val _ = (Marshal_Word8.marshal (word, 0w0) o
		     Marshal_Word8.marshal (word, 0w0) o
		     Marshal_Word8.marshal (word, 0w0) o
		     Marshal_Word8.marshal (word, pointer)) 0w0
	    val (word_val, _) = Marshal_Word32.unmarshal (word, 0w0)
	in marshal_standard_header (type_val, code, word_val, packet)
	end

   fun marshal_unreachable (type_val, (code, mtu), packet) =
        let val word_buf = Out.new (Word_Array.from8
				    (Word_Array.W8.U_Big.F.create (0w0, 0w4)))
	    val _ = Marshal_Word16.marshal (word_buf, mtu) 0w2
	    val (second_word, _) = Marshal_Word32.unmarshal (word_buf, 0w0)
	in marshal_standard_header (type_val, code, second_word, packet)
	end

   fun marshal_id_seq (type_val, code, id, seq, packet) =
        let val word = Out.uninitialized id_seq_size
	    val _ = (Marshal_Word16.marshal (word, seq)
		     o Marshal_Word16.marshal (word, id)) 0w0
	    val (word_val, _) = Marshal_Word32.unmarshal (word, 0w0)
	in marshal_standard_header (type_val, code, word_val, packet)
	end

   fun marshal_time_stamp (type_val, code, id, seq, t1, t2, t3) =
        let val data = Out.uninitialized time_stamp_size
	    val marshal = (Marshal_Word32.marshal (data, t3) o
			   Marshal_Word32.marshal (data, t2) o
			   Marshal_Word32.marshal (data, t1))
	in marshal 0w0;
	   marshal_id_seq (type_val, code, id, seq, data)
	end

   fun marshal_address_mask (type_val, code, id, seq, mask) =
        let val zero_ip = Word_Array.from8 (Word_Array.W8.U_Big.F.create
					    (0w0, id_seq_size))
	    val data = Out.new zero_ip
	in case mask of
	      NONE => ()
	    | SOME m => (Ip_Extern.marshal (data, m) 0w0; ());
	   marshal_id_seq (type_val, code, id, seq, data)
	end

   fun marshal_router_ad  (type_val, lifetime, addresses) =
        let fun compute_address {address, preference_level} =
	         let val buffer = Out.new (Word_Array.from8
					   (Word_Array.W8.U_Big.F.create
					    (0w0, 0w8)))
		 in (Marshal_Word32.marshal (buffer, preference_level) o
		     Ip_Extern.marshal (buffer, address)) 0w0;
		    #1 (Word64.unmarshal (buffer, 0w0))
		 end
	    fun gen [] = NONE
	      | gen (head :: rest) = SOME (compute_address head, rest)
	    val num_addrs = Word8.fromInt (B.V.List.length addresses)
	    val a64 = Word_Array.W64.U_Big.F.new gen addresses
	    val a8 = Out.new (Word_Array.from64 a64)
	    val word_buf = Out.uninitialized id_seq_size
	    val _ = (Marshal_Word16.marshal (word_buf, lifetime) o
		     Marshal_Word8.marshal (word_buf,
				    Types.supported_router_entry_size) o
		     Marshal_Word8.marshal (word_buf, num_addrs)) 0w0
	    val (second_word, _) = Marshal_Word32.unmarshal (word_buf, 0w0)
	in marshal_standard_header (type_val, 0w0, second_word, a8)
	end

   fun marshal_router_sol type_val =
	marshal_standard_header (type_val, 0w0, Word32.fromInt 0,
				 Out.uninitialized 0w0)

(* RFC 1393, p. 4 *)
   fun marshal_traceroute (type_val, code, id, out, return, speed, mtu) =
        let val word_buf = Out.new (Word_Array.from8
				    (Word_Array.W8.U_Big.F.create (0w0, 0w4)))
            val data = Out.uninitialized 0w12
	    val _ = Marshal_Word16.marshal (word_buf, id) 0w0
	    val (second_word, _) = Marshal_Word32.unmarshal (word_buf, 0w0)
	in (Marshal_Word32.marshal (data, mtu) o
	    Marshal_Word32.marshal (data, speed) o
	    Marshal_Word16.marshal (data, return) o
	    Marshal_Word16.marshal (data, out)) 0w0;
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
	marshal_zero (Types.source_quench_type, 0w0, packet)
     | marshal (Echo {id, sequence, data}) =
        marshal_id_seq (Types.echo_type, 0w0, id, sequence, data)
     | marshal (Echo_Reply {id, sequence, data}) =
        marshal_id_seq (Types.echo_reply_type, 0w0, id, sequence,
			       data)
     | marshal (Traceroute {forwarded, id, out_hops, return_hops,
			    out_speed, out_mtu}) =
	marshal_traceroute (Types.traceroute_type,
			    if forwarded then Types.traceroute_fwd_code
			    else Types.traceroute_discard_code,
			    id, out_hops, return_hops, out_speed, out_mtu)
     | marshal (Time_Stamp {id, sequence}) =
        marshal_time_stamp (Types.time_stamp_type, 0w0, id, sequence,
			    Types.get_universal_time (),
			    Word32.fromInt (*0w*)0, Word32.fromInt (*0w*)0)
     | marshal (Time_Stamp_Reply {id, sequence, originate, receive}) =
        marshal_time_stamp (Types.time_stamp_reply_type, 0w0, id, sequence,
			    originate, receive, Types.get_universal_time ())
     | marshal (Mask_Request {id, sequence}) =
        marshal_address_mask (Types.mask_request_type, 0w0, id, sequence,
			      NONE)
     | marshal (Mask_Reply {id, sequence, address_mask}) =
        marshal_address_mask (Types.mask_reply_type, 0w0, id, sequence,
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
