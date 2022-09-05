(*
	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	ip.fun: a top-level functor for IP.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor header
	2.	structures Header, Option, and Host_Id
	3.	structure Protocol_Id
	4.	structure Network_Setup
	5.	structure Network_Address
	6.	structure Connection_Key
	7.	exported function key_to_address
	8.	structure Network_Pattern
	9.	structure Network_Incoming
	10.	structure Network_Outgoing
	11.	structures Icmp_In and Icmp_Out
	12.	structure Network_Status
	13.	required extensions
	14.	structures from lower protocol
	15.	structure Route
	16.	structure Fragment
	17.	function marshal_icmp_to_lower
	18.	functions is_info_icmp, process_info_icmp
	19.	function convert_packet
	20.	function lower_setup
	21.	type protocol_state
	22.	function init_proto
	23.	function fin_proto
	24.	function resolve
	25.	function make_key
	26.	function map_pattern
	27.	function match
	28.	function undelivered
	29.	type connection_state
	30.	function compute_header
	31.	functions pseudo_check, init_connection
	32.	function fin_connection
	33.	function send
	34.	function identify
	35.	function receive
	36.	function lower_status
	37.	structure Connection
	38.	exported types
	39.	structure Icmp: structures, types
	40.	structure Icmp.Incoming
	41.	structure Icmp.Outgoing
	42.	Icmp types connection, listen, handler, session
	43.	Icmp.session
	44.	joint function session

	iii.	RCS Log

$Log: ip.fun,v $
Revision 1.130  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.129  97/06/04  12:57:30  esb
added a reasonable message and return result for the case where
route tells us to use loopback to reach a host.

Revision 1.128  97/02/13  00:42:27  esb
updated to use imperative monomorphic stores.

Revision 1.127  1997/01/24  14:45:56  cline
eliminated (illegal) signature declaration within functor body.

Revision 1.126  1996/12/20  21:39:34  esb
changed a print statement.

Revision 1.125  1996/10/03  18:34:27  esb
fixed a bug whereby notice of reassembly timeout was trying to
send the entire expired IP packet, which does not usually fit
in a lower-layer packet.

Revision 1.124  1996/07/22  20:06:40  cline
removed obsolete variable "valid"

Revision 1.123  1996/06/11  03:25:27  esb
fixed the returned max packet size, and the "before" problem.

Revision 1.122  1996/05/28  19:08:02  esb
now ignores an experimental protocol used at CMU.

Revision 1.121  1996/05/14  01:18:19  esb
fixed a bug in ICMP session reference counting.

Revision 1.120  1996/05/08  02:02:45  esb
cleaned up so ICMP connection handler only remains alive if the
data handler is responding to ICMP Echo or Timestamp.

Revision 1.119  1996/04/18  21:19:18  cline
converted hash from int to word

Revision 1.118  1996/03/15  22:45:07  esb
pass IP status up to ICMP connections.

Revision 1.117  1996/03/12  22:22:33  esb
added port_unreachable.

Revision 1.116  1996/03/04  21:24:24  esb
made the Host_Id structure a functor parameter.

Revision 1.115  1996/02/29  17:34:18  esb
fixed an initialization bug

Revision 1.114  1996/02/23  21:08:59  esb
initialization now works.

Revision 1.113  1996/01/19  23:02:29  esb
adapted to the new wordarray signature.

Revision 1.112  1996/01/16  22:38:59  cline
*** empty log message ***

Revision 1.111  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.110  1995/11/12  11:29:47  esb
adapted to new ipmux.fun

Revision 1.109  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.108  1995/10/17  22:31:48  esb
changed some debugging statements.

Revision 1.107  1995/10/17  21:47:26  esb
added the latest connection functor, fixed some bugs.

Revision 1.106  1995/10/04  21:30:57  esb
added sending "destination unreachable" messages.

Revision 1.105  1995/10/03  21:03:21  esb
changed print statements and fixed bug.

Revision 1.104  1995/10/03  18:54:16  esb
fixed ICMP listening; also reject packets for other hosts in identify.

Revision 1.103  1995/10/02  21:22:39  esb
temporary version, new connection functor.

Revision 1.102  1995/09/26  15:48:07  esb
temporary working version.

Revision 1.101  1995/09/19  18:49:30  esb
changes in some print statements.

Revision 1.100  1995/09/14  21:08:33  cline
work around for representation bug

Revision 1.99  1995/09/13  15:25:31  esb
added sending mask requests and replies, added ICMP session for IP sessions

Revision 1.98  1995/08/30  19:37:07  esb
made test programs work.

Revision 1.97  1995/08/29  14:14:54  esb
version that includes IP and ICMP.

Revision 1.96  1995/08/24  00:50:00  esb
added some ICMP support.

Revision 1.95  1995/08/08  18:21:53  esb
many small changes.

Revision 1.94  1995/07/21  12:51:00  esb
adapted to new conn.fun (again).

Revision 1.93  1995/07/20  21:33:18  esb
adapted to changed conn.fun

Revision 1.92  1995/07/03  23:22:32  esb
adapted to ill-identified compiler bug.

Revision 1.91  1995/06/29  19:54:29  esb
adapted to new network.sig.

Revision 1.90  1995/06/27  16:55:45  esb
adapted to new conn.fun.

Revision 1.89  1995/06/26  17:29:15  esb
this version seems to work.

Revision 1.88  1995/06/23  19:55:30  esb
first version of IP that compiles with the new PROTOCOL signature.

Revision 1.87  1995/03/24  16:00:28  esb
replaced signExtend with wordToInt.

Revision 1.86  1995/03/24  01:52:46  esb
adapted to new ip.sig and icmp.sig; also now raises Local_Unreachable.

Revision 1.85  1995/03/12  17:50:04  esb
adapted to new trace.sig.

Revision 1.84  1995/03/10  18:04:52  esb
updated a reference.

Revision 1.83  1995/03/10  03:46:34  esb
adapted to new vendor.sig.

Revision 1.82  1995/03/07  20:33:36  esb
updated tracing.

Revision 1.81  1995/02/22  15:22:04  esb
fixed a bug (version number zero was being sent out), added version checks.

Revision 1.80  1995/02/09  19:52:42  esb
now correctly counts incoming and outgoing packets.

Revision 1.79  1995/02/04  20:40:05  robby
updated to 107

Revision 1.78  1995/01/18  21:00:57  esb
renamed EmptyIcmp to Empty_Icmp.

Revision 1.77  1995/01/14  02:25:01  esb
adapted to new filter interface.

Revision 1.76  1995/01/06  23:16:09  esb
clarified a debugging statement.

Revision 1.75  1995/01/06  01:33:56  esb
properly implemented filter.

Revision 1.74  1995/01/03  18:12:06  esb
added a dummy implementation of filter.

Revision 1.73  1994/12/12  11:46:24  esb
now checks for correct destination address before delivering packet.

Revision 1.72  1994/11/22  13:58:03  milnes
Removed addressing functor arguments.

Revision 1.71  1994/11/11  18:10:29  esb
changed the functor parameters for Debug_Trace_Printer.

Revision 1.70  1994/11/10  22:14:12  milnes
Changed the trace name.

Revision 1.69  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.68  1994/10/25  16:32:21  esb
added handles around all calls to Conn.functions.

Revision 1.67  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.66  1994/09/30  17:00:30  esb
now closes lower connection from passive open only if no other references

Revision 1.65  1994/09/23  16:48:50  milnes
Added lower connection close on failed passive open and some
tracing.

Revision 1.64  1994/09/14  15:30:00  milnes
Added traces and fixed a bug with ipfrag in that it was not being initialized.

Revision 1.63  1994/09/12  18:12:25  milnes
Added prints to all of the handle _'s.

Revision 1.62  1994/08/28  21:40:07  milnes
Modified for icmp.

Revision 1.61  1994/08/12  06:22:36  esb
minor changes.

Revision 1.60  1994/08/03  19:41:38  esb
fixed the computation of the pseudo-checksum.

Revision 1.59  1994/08/02  20:32:38  esb
adapted to new protocol signature.

Revision 1.58  1994/07/05  19:54:41  danwang
Fixed minor bug building Ip_Options structure.

Revision 1.57  1994/07/04  21:32:56  esb
adapted to Copy/Create split.

Revision 1.56  1994/07/01  02:27:10  danwang
Moved control structures into Fox_Basis.

Revision 1.55  1994/06/16  16:39:20  danwang
Updated to use functorized Fox_Basis

Revision 1.54  1994/06/15  20:47:10  milnes
Installed subnet routing.

Revision 1.53  1994/06/09  18:36:46  esb
set type incoming_message to ip_receive, to separate out header and data.

Revision 1.52  1994/06/05  18:41:30  milnes
Added interaction with ICMP.

Revision 1.51  1994/05/23  13:59:34  milnes
Added print functions, fixed an options bug, and fixed a passive
open bug.

Revision 1.50  1994/05/10  07:51:26  esb
adapted to new store.sig and receive_packet.sig, optimized.

Revision 1.49  94/05/04  01:40:17  esb
brought up to date with the new coroutine and event signatures.

Revision 1.48  94/05/03  20:55:11  milnes
Added ip option handling.

Revision 1.47  1994/04/16  00:38:49  esb
added fragment timing, extend_uninitialized_trailer.

Revision 1.46  94/04/06  23:11:59  esb
removed an unnecessary relic.

Revision 1.45  94/03/30  22:58:00  milnes
Made ip always print an warning when a packet is discarded.

Revision 1.44  1994/03/29  17:44:40  milnes
Bounds checked format call.

Revision 1.43  1994/03/10  19:42:39  esb
used Copy.create.

Revision 1.42  94/03/09  03:29:50  esb
minor restructuring.

Revision 1.41  94/03/04  02:25:32  milnes
Corrected ip_receive_counter case.

Revision 1.40  1994/02/20  23:59:59  esb
modified send to adapt to new send_packet interface.

Revision 1.39  94/02/18  14:33:35  milnes
Extended the timing calls and made the fast path timing calls non-cumulative.

Revision 1.38  1994/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file
move problems that confused rcs.

Revision 1.36  1994/02/09  18:01:34  esb
added functor parameter for ip_fragment.

Revision 1.35  94/02/08  19:54:31  esb
fixed a bug whereby the lower connection was not being saved.

Revision 1.34  94/02/09  00:40:11  milnes
Changed some prints to not cons on the fast path.

Revision 1.33  1994/01/30  18:30:59  milnes
changed read_offset to read in send_packet.

Revision 1.32  1994/01/30  02:43:45  milnes
No change.

Revision 1.31  1994/01/30  01:41:57  milnes
Broke Edo's lock, locked it myself, copied over Edo's file,
and checked it in so I can benchmark.

Revision 1.30  1994/01/21  21:10:29  esb
added time-to-live access to info and control.

Revision 1.29  1994/01/18  04:20:00  esb
minor change.

Revision 1.28  1994/01/17  19:51:04  milnes
Changes for ip fragmentation.

Revision 1.27  94/01/13  15:00:10  cline
Added fragmentation support.

Revision 1.26  1994/01/08  21:47:53  esb
improved the MTU computation and added control operation to set the MTU.

Revision 1.25  1993/12/17  02:32:56  esb
improved close and error messages.

Revision 1.24  1993/12/14  20:20:11  esb
minor fix.

Revision 1.23  1993/12/14  20:19:35  esb
Added initial interfaces functor parameter; fixed but in passive open process.

Revision 1.22  1993/12/04  21:00:32  esb
now provide a handler with a parameter of type connection.

Revision 1.21  1993/11/09  22:08:29  milnes
Added some tracing and error handling.

Revision 1.20  1993/10/25  19:32:26  cline
removed .U from Byte[421].U

Revision 1.19  1993/10/21  16:15:01  esb
corrected a bug which increased the packet ID only on occasion.

Revision 1.18  93/10/20  20:39:53  milnes
Added trace printing to send.

Revision 1.17  1993/10/15  16:21:41  esb
improved an error message.

Revision 1.16  1993/10/13  18:49:54  esb
brought the error messages up to standard.

Revision 1.15  93/10/13  13:47:37  milnes
Brought the error messages up to standard.

Revision 1.14  1993/10/08  05:23:57  esb
upgraded to have all the state in connections or in a single internal value.

Revision 1.13  1993/09/22  19:31:56  milnes
Removed "-----------"s.

Revision 1.12  1993/09/20  14:22:41  esb
properly implemented abort.

Revision 1.11  1993/09/18  21:50:07  esb
minor changes

Revision 1.10  1993/09/14  15:44:26  cline
added hash_table_size to Dispatcher

Revision 1.9  1993/09/13  22:06:52  cline
deleted '#'s from RCS log

Revision 1.8  1993/09/10  11:40:11  esb
added interfaces and local_address to the info datatype.

Revision 1.7  1993/09/02  22:20:22  esb
adapted to changes in the PROTOCOL signature.

Revision 1.6  1993/08/13  13:38:41  esb
temporary correction to max_packet_size_query.

Revision 1.5  1993/08/12  18:35:50  esb
sender checks for lower protocol raising packet_size and translates it.

Revision 1.4  1993/08/09  16:28:57  esb
Now prints when there is an exception in a passive-open handler.

Revision 1.3  1993/07/30  14:23:43  esb
fixed initialization and termination.

Revision 1.2  1993/07/23  13:55:50  esb
added packet IDs on outgoing packets.

Revision 1.1  1993/07/16  18:43:32  esb
Initial revision

	1.	functor header

	If "gateway" is true, act as a gateway, forwarding packets not
	for us; the routing itself must be determined by redirects or
	by higher-level protocols (using the add/remove gateway
	functions in session_extension).  Also respond to router
	solicitations.
*)

functor Ip (structure Lower: IP_MULTIPLEXER where type ip_number = Word32.word
	    structure Host_Id: NETWORK_HOST_ID where type T = Word32.word
	    structure B: FOX_BASIS
	    val icmp_protocol: Word8.word
	    val gateway: bool
            val debug_level: int ref option): IP_PROTOCOL =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ip.fun"
			   val makestring = Lower.X.makestring)

  val zero16 = Word16.fromInt 0
  val zero32 = Word32.fromInt 0
  val zero64 = Word64.fromInt 0
  val one16 = Word16.fromInt 1
  val one32 = Word32.fromInt 1
  val one64 = Word64.fromInt 1

(*
	2.	structures Header, Option, and Host_Id
*)

  structure Header = Ip_Header (structure In = Lower.Incoming
				structure Out = Lower.Outgoing
				val debug_level = debug_level
				structure B = B)

  structure Option = Header.Option

  structure Host_Id = Host_Id

(*
	3.	structure Protocol_Id
*)

  structure Protocol_Id =
   struct
    type T = Word_Array.W8.element
    val makestring = Word8.fmt StringCvt.DEC
    fun equal (a, b: T) = a = b
    val hash = Word.fromLargeWord o Word8.toLargeWord
   end (* struct *)

(*
	4.	structure Network_Setup
*)

  structure Network_Setup = Lower.Mux_Setup
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

(*
	6.	structure Connection_Key
*)

  structure Connection_Key =
   struct
    datatype T = CK of {peer: Host_Id.T, proto: Protocol_Id.T,
			interface: string}
    fun makestring (CK {peer, proto, interface}) = 
         Host_Id.makestring peer ^ "/" ^ Protocol_Id.makestring proto ^
	 "/" ^ interface
    fun equal (CK {peer = p1, proto = proto1, interface = i1},
	       CK {peer = p2, proto = proto2, interface = i2}) =
         Host_Id.equal (p1, p2) andalso Protocol_Id.equal (proto1, proto2)
	 andalso i1 = i2
    fun hash (CK {peer, proto, interface}) =
         Host_Id.hash peer + Protocol_Id.hash proto
   end

(*
	7.	exported function key_to_address
*)

  fun key_to_address (Connection_Key.CK {peer, proto, interface}) =
       Network_Address.Address {peer = peer, proto = proto}

(*
	8.	structure Network_Pattern
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
	9.	structure Network_Incoming

	The pseudo-header checksum for this protocol is always zero
	if the packet is created using the functions in this structure.
	To create a packet with an authentic pseudo-checksum, create
	it directly, i.e. by pairing a Lower.Incoming.T with the
	corresponding checksum.
*)

  structure Network_Incoming =
   struct
    type checksum = Word16.word
    type net_option = Option.ip_option
    datatype info = None | Info of {pseudo_check: Word16.word,
				    raw_packet: Lower.Incoming.T,
				    options: net_option list}
    datatype T = Incoming of Lower.Incoming.T * info
    fun new array = Incoming (Lower.Incoming.new array, None)
    fun uninitialized count =
          Incoming (Lower.Incoming.uninitialized count, None)
    fun size (Incoming (data, _)) = Lower.Incoming.size data
    fun sub (Incoming (data, _), args) = Lower.Incoming.sub (data, args)
    fun update (Incoming (data, _), index, value) =
         Lower.Incoming.update (data, index, value)
    fun join (Incoming (d1, o1), Incoming (d2, o2)) =
         Incoming
	   (Lower.Incoming.join (d1, d2), case o1 of None => o2 | _ => o1)
    fun split (Incoming (data, other), args) =
         let val (d1, d2) = Lower.Incoming.split (data, args)
	 in (Incoming (d1, other), Incoming (d2, other))
	 end
    fun fold (Incoming (data, _), f, init) =
          Lower.Incoming.fold (data, f, init)
    fun makestring (Incoming (data, _)) = Lower.Incoming.makestring data
    fun makestring_max ((Incoming (data, _)), args) =
         Lower.Incoming.makestring_max (data, args)
    fun pseudo_header_checksum (Incoming (_, None)) = Word16.fromInt 0
      | pseudo_header_checksum (Incoming (_, Info {pseudo_check, ...})) =
          pseudo_check
    fun options (Incoming (_, None)) = []
      | options (Incoming (_, Info {options, ...})) = options
   end (* struct *)
  structure Incoming = Network_Incoming

(*
	10.	structure Network_Outgoing

	Sending at most one source route is required by RFC 1122, p. 36.
	We implement it by deleting the later source route.
*)

  fun is_route (Option.Loose_Route _) = true
    | is_route (Option.Strict_Route _) = true
    | is_route _ = false

  fun eliminate_duplicate_routes ([], _) = []
    | eliminate_duplicate_routes (head :: rest, false) =
       head :: eliminate_duplicate_routes (rest, is_route head)
    | eliminate_duplicate_routes (head :: rest, true) =
       if is_route head then
	(Trace.local_print ("IP packet has multiple source route options, " ^
			    "dropping one source route");
	 eliminate_duplicate_routes (rest, true))
       else head :: eliminate_duplicate_routes (rest, true)

  structure Network_Outgoing =
   struct
    type net_option = Option.ip_option
    datatype T = Outgoing of (Lower.Outgoing.T * net_option list)
    fun new array = Outgoing (Lower.Outgoing.new array, [])
    fun put_options (Outgoing (data, _), options) =
         Outgoing (data, eliminate_duplicate_routes (options, false))
    fun new_options (array, options) =
	 Outgoing
           (Lower.Outgoing.new array,
	    eliminate_duplicate_routes (options, false))
    fun uninitialized count = Outgoing (Lower.Outgoing.uninitialized count, [])
    fun size (Outgoing (data, _)) = Lower.Outgoing.size data
    fun sub (Outgoing (data, _), args) = Lower.Outgoing.sub (data, args)
    fun update (Outgoing (data, _), index, value) =
         Lower.Outgoing.update (data, index, value)
    fun join (Outgoing (d1, o1), Outgoing (d2, o2)) =
	 Outgoing (Lower.Outgoing.join (d1, d2),
		   case o1 of [] => o2 | _ => o1)
    fun split (Outgoing (data, options), args) =
         let val (d1, d2) = Lower.Outgoing.split (data, args)
	 in (Outgoing (d1, options), Outgoing (d2, options))
	 end
    fun fold (Outgoing (data, _), f, init) =
          Lower.Outgoing.fold (data, f, init)
    fun makestring (Outgoing (data, _)) = Lower.Outgoing.makestring data
    fun makestring_max (Outgoing (data, _), args) =
         Lower.Outgoing.makestring_max (data, args)
   end (* struct *)

  structure Outgoing = Network_Outgoing

(*
	11.	structures Icmp_In and Icmp_Out
*)

  structure Ip_Extern: EXTERN_KEY =
         Protocol_Extern32_Big (structure In = Network_Incoming
				structure Out = Network_Outgoing
				structure B = B)

  structure Icmp_Ip_Header = Ip_Header (structure In = Network_Incoming
					structure Out = Network_Outgoing
					val debug_level = debug_level
					structure B = B)

  structure Redirect = Icmp_Redirect ()
  structure Unreachable = Icmp_Unreachable ()


  structure Icmp_In = Icmp_In (structure In = Network_Incoming
			       structure Ip_Header = Icmp_Ip_Header
			       structure Ip_Extern = Ip_Extern
			       structure Redirect = Redirect
			       structure Unreachable = Unreachable
			       structure B = B
			       val debug_level = debug_level)

  structure Icmp_Out = Icmp_Out (structure Out = Network_Outgoing
				 structure Ip_Extern = Ip_Extern
				 structure Redirect = Redirect
				 structure Unreachable = Unreachable
				 structure B = B
				 val debug_level = debug_level)

(*
	12.	structure Network_Status
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
	 Network_Incoming.makestring_max (data, 0w8) ^
	 (case data of
	     Network_Incoming.Incoming (_, Network_Incoming.None) => ""
	   | Network_Incoming.Incoming (_, Network_Incoming.Info
					    {raw_packet, ...}) =>
	      " (IP/ICMP bits " ^
	      Lower.Incoming.makestring_max (raw_packet, 0w36) ^
	      ")")
      | makestring (Quench (data, _)) =
	 "quench " ^ Network_Incoming.makestring_max (data, 0w8)
   end (* struct *)
  structure Status = Network_Status

(*
	13.	required extensions
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

  type ip_session_extension = unit
  type specific_session_extension = ip_session_extension

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
	14.	structures from lower protocol
*)

  structure X = Lower.X
  structure Count = Lower.Count

(*
	15.	structure Route
*)

  structure Route = Ip_Route (val makestring_ip = Host_Id.makestring
			      structure B = B
			      val debug_level = debug_level)

(*
	16.	structure Fragment
*)

  structure Fragment = Ip_Fragment (structure B = B
				    structure Header = Header
				    structure Incoming = Lower.Incoming
				    structure Outgoing = Lower.Outgoing
				    val debug_level = debug_level)

(*
	17.	function marshal_icmp_to_lower
*)

  local
   val mask_request =
        Icmp_Out.marshal
	 (Icmp_Out.Mask_Request {id = Word16.fromInt 3,
				 sequence = Word16.fromInt 0})
   val info_icmp_tos = 0w0 : Word8.word (* RFC 1700, pp. 65-66 *)
   val info_icmp_id = ref (Word16.fromInt 0x1234)
   val one16 = Word16.fromInt 1
   fun get_next id =
        (! id before id := Word16.+ (! id, one16))
   val info_icmp_flags = Header.Flag.None
   val info_icmp_ttl = 0w1 : Word8.word (* one hop only *)
   val broadcast_dest = Word32.notb (Word32.fromInt 0)

   fun info_icmp_header (source, destination, tos,
			 Network_Outgoing.Outgoing (data, options), ttl) =
        let val data_length = Word16.fromInt (Word.toInt
					      (Lower.Outgoing.size data))
	in (Header.V4 {tos = tos, data_length = data_length,
		       identification = get_next info_icmp_id,
		       flags = info_icmp_flags, ttl = ttl,
		       protocol = icmp_protocol,
		       source = source, destination = destination,
		       options = options},
	    data)
	end

  in
   datatype ttl = Local_Ttl | Given_Ttl of Word8.word

   fun marshal_icmp_to_lower (icmp, min_size, source, destination, tos, ttl) =
        let val ttl_value = case ttl of
	                       Local_Ttl => info_icmp_ttl
			     | Given_Ttl ttl => ttl
	    val (ip_header, icmp_data) =
	           info_icmp_header (source, destination, tos, icmp, ttl_value)
	    val ip_data = Lower.Outgoing.uninitialized (Header.size ip_header)
	    val _ = Header.marshal (ip_data, ip_header) 0w0
	    val short_data = Lower.Outgoing.join (ip_data, icmp_data)
	    val short_size = Lower.Outgoing.size short_data
	in if short_size >= min_size then short_data
	   else Lower.Outgoing.join (short_data,
				     Lower.Outgoing.uninitialized
				     (min_size - short_size))
	end
  end

(*
	18.	functions is_info_icmp, process_info_icmp
*)

  datatype info_icmp = Info_Icmp
                     | Not_Info_Icmp of Icmp_In.icmp_in

  val router_ad_interval = 600 (* seconds -- RFC 1256, p. 7 *)

  local
   fun eq (a: string, b) = a = b

   val hash_map = Word.fromInt o B.V.Char.ord

   fun hash a =
        B.V.List.fold Word.+ (B.V.List.map hash_map (B.V.String.explode a)) 0w0
  in
   structure Record_String = Imperative_Monomorphic_Store (structure V = B.V
							   type key = string
							   type value = unit
							   val eq = eq
							   val hash = hash)
  end

  local
   val router_ad_lifetime = Word16.fromInt (3 * 600) (* RFC 1256, p. 8 *)

   fun process_router_ad (route, lifetime, []) = ()
     | process_router_ad (route, lifetime,
			  {address, preference_level} :: rest) =
        (Trace.trace_print (fn _ => "adding route " ^
			    Host_Id.makestring address ^
			    ", preference level " ^
			    Word32.fmt StringCvt.DEC preference_level ^
			    ", timeout " ^
			    (Integer.toString o Word16.toInt) lifetime);
	 route := Route.add_preference_gateway (! route, address,
					        preference_level,
					        Word16.toInt lifetime);
         Trace.trace_print (fn _ => "result is " ^ Route.makestring (! route));
	 process_router_ad (route, lifetime, rest))

   fun serving_mask (_, Setup.Setup []) = NONE
     | serving_mask (wanted_interface,
		     Setup.Setup ({mask, interface, ...} :: rest)) =
        case mask of
	    NONE => serving_mask (wanted_interface, Setup.Setup rest)
	  | SOME (m, {serve}) =>
	     if serve andalso wanted_interface = interface then
	      SOME (interface, m)
	     else serving_mask (wanted_interface, Setup.Setup rest)

   fun local_gateways (_, Setup.Setup []) = []
     | local_gateways (wanted_interface,
		       Setup.Setup ({local_id, gateways,
				     interface, ...} :: rest)) =
        if wanted_interface = interface then
	 B.V.List.map (fn g => {address = g, preference_level = zero32})
	              (local_id :: gateways)
	else local_gateways (wanted_interface, Setup.Setup rest)

  in
   fun is_info_icmp icmp_packet =
        case Icmp_In.unmarshal icmp_packet of
	   Icmp_In.Mask_Reply {id, sequence, address_mask} => Info_Icmp
	 | Icmp_In.Mask_Request {id, sequence = sequence} => Info_Icmp
	 | Icmp_In.Router_Solicitation => Info_Icmp
	 | Icmp_In.Router_Advertisement {lifetime, addresses} => Info_Icmp
	 | Icmp_In.Experimental_Router_Ad => Info_Icmp
	 | icmp => Not_Info_Icmp icmp

   (* to add serving router solicitations, modify this function. *)
   fun process_info_icmp (setup, route, instance_id, mask_db, gateway_db,
			  interface, icmp_packet, send, signal_done,
			  min_size, source, destination, tos) =
        case Icmp_In.unmarshal icmp_packet of
	   Icmp_In.Mask_Reply {id, sequence, address_mask} =>
	    (Trace.trace_print (fn _ => "received mask reply, mask = " ^
			        Host_Id.makestring address_mask ^
				", protocol instance = " ^
				Integer.toString instance_id);
	     route := Route.set_interface_mask (! route, interface,
					        address_mask);
	     Record_String.add (mask_db, interface, ());
	     Info_Icmp)
	 | Icmp_In.Mask_Request {id, sequence = sequence} =>
	    (Trace.trace_constant_string "received mask request";
	     case serving_mask (interface, setup) of
	        NONE => ()
	      | SOME (interface, mask) =>
		 send (marshal_icmp_to_lower
		       (Icmp_Out.marshal
			(Icmp_Out.Mask_Reply {id = id,
					      sequence = sequence,
					      address_mask = mask}),
			min_size, source, destination, tos, Local_Ttl));
	     B.Pipe.enqueue (signal_done, ()); (* close the connection *)
	     Info_Icmp)
	 | Icmp_In.Router_Solicitation =>
	    (Trace.trace_constant_string "received router solicitation";
	     if gateway then
	      send (marshal_icmp_to_lower
		    (Icmp_Out.marshal
		     (Icmp_Out.Router_Advertisement
		      {lifetime = router_ad_lifetime,
		       addresses = local_gateways (interface, setup)}),
		     min_size, source, destination, tos, Local_Ttl))
	     else (); (* not a router: silently discard, RFC 1256 p. 14 *)
	     B.Pipe.enqueue (signal_done, ()); (* close the connection *)
	     Info_Icmp)
	 | Icmp_In.Router_Advertisement {lifetime, addresses} =>
	    (Trace.trace_print (fn _ => "received router ad, " ^
				"protocol instance = " ^
				Integer.toString instance_id);
	     Record_String.add (gateway_db, interface, ());
	     process_router_ad (route, lifetime, addresses);
	     Info_Icmp)
	 | Icmp_In.Experimental_Router_Ad => Info_Icmp (* ignore *)
	 | icmp => Not_Info_Icmp icmp
  end

(*
	19.	function convert_packet

	Convert incoming to ICMP outgoing.
*)

   fun convert_packet data =
        let fun convert_single (item, rest) = 
	         Network_Outgoing.join (Network_Outgoing.new item, rest)
	in Lower.Incoming.fold (data, convert_single,
				Network_Outgoing.uninitialized 0w0)
	end

(*
	20.	function lower_setup
*)

  fun lower_setup setup = setup

(*
	21.	type protocol_state
*)

  datatype protocol_state =
      Protocol_State of
        {setup: Setup.T,
	 valid_instance: bool ref,
	 instance_id: int,
	 route: Route.T ref,
	 lower_connect: Lower.Address.T * Lower.handler -> unit,
	 lower_session_ext: Lower.mux_session_extension,
	 call_status: (Connection_Key.T * Status.T -> unit),
	 received_address_mask: Record_String.T,
	 received_router_ad: Record_String.T,
	 sent_list: (Word64.word * Word64.word ref list) ref,
	 received_list: (Word64.word * Word64.word ref list) ref,
	 failed_send_count: Word64.word ref,
	 rejected_count: Word64.word ref}

(*
	22.	function init_proto
*)

  local
   val mask_request =
        Icmp_Out.marshal
	 (Icmp_Out.Mask_Request {id = Word16.fromInt 3,
				 sequence = Word16.fromInt 0})
   val info_icmp_tos = Word8.fromInt 0	(* RFC 1700, pp. 65-66 *)
   val broadcast_dest = Word32.notb (Word32.fromInt 0)

   fun send_one_icmp (icmp, self, remote, min_size)
                     (conn as (Lower.C {send, ...})) =
	let val data = marshal_icmp_to_lower (Icmp_Out.marshal icmp, min_size,
					      self, remote, info_icmp_tos,
					      Local_Ttl)
	 in Trace.trace_print (fn _ => "sending " ^ Icmp_Out.makestring icmp);
	    send data
	 end

   (* All data should be received by function receive given as
      a parameter to the Connection functor, or by broadcast_data. *)
   fun request_data_handler (conn, data) =
	Trace.local_print ("icmp_request received unexpected data " ^
			   Lower.Incoming.makestring data)

   fun icmp_request (_, _, _, _, 0) = ()
     | icmp_request (ref false, _, _, _, _) = () (* protocol is shut down *)
     | icmp_request (valid_protocol, interface, connect, data, count) =
        case data () of
	   [] => ()			(* completed, stop requesting *)
	 | list =>
	    let fun send_request icmp =
	             let fun sender (Lower.C {send, ...}) = send icmp
		         fun handler key =
			      {connection_handler = sender,
			       status_handler = fn _ => (),
			       data_handler = request_data_handler}
		     in Trace.trace_print (fn _ => "icmp_request sending " ^
					   Lower.Outgoing.makestring icmp);
		        connect (Lower.Mux_Address.Broadcast interface,
				 Lower.H handler)
		     end
	in ((B.V.List.app send_request list)
	    handle x =>
	            Trace.print_handled (x, SOME "icmp_request"));
	   B.Scheduler.sleep 1000;
	   icmp_request (valid_protocol, interface, connect, data, count - 1)
	end

   (* to add sending out an initial router ad, modify this function. *)
   fun serve_mask (mask, valid, setup, route, mask_db, gateway_db, interface,
		   self, min_size, connect) =
        let val id = Word16.fromInt 0x33
	    fun null _ = ()
	    val mask_icmp = Icmp_Out.Mask_Reply {id = id, sequence = id,
						 address_mask = mask}
	    val mask_data =
		  marshal_icmp_to_lower (Icmp_Out.marshal mask_icmp,
					 min_size, self, broadcast_dest,
					 info_icmp_tos, Local_Ttl)
	    fun connection_handler (Lower.C {send, ...}) = send mask_data
	    fun conn_handler _ =
	         {connection_handler = connection_handler,
		  data_handler = null, status_handler = null}
	in connect (Lower.Mux_Address.Broadcast interface,
		    Lower.H conn_handler)
	end

   fun init_interface (valid_protocol, interface, setup, local_id,
		       mask, minimum_packet_size, route, connect,
		       received_address_mask, received_router_ad) () =
        let val min_size = minimum_packet_size interface
	    val rs = Icmp_Out.marshal Icmp_Out.Router_Solicitation
	    val router_solicitation =
	         marshal_icmp_to_lower (rs, min_size, local_id,
					broadcast_dest, info_icmp_tos,
					Local_Ttl)
	    fun router_solicitation_if_not_received () =
	         if not (Route.has_default_gateway (! route, interface)) then
		  case Record_String.look (received_router_ad, interface) of
		     NONE => [router_solicitation]
		   | _ => []
		 else []
        in case mask of
	      SOME (m, {serve = true}) => (* serve_mask returns immediately *)
	       (Trace.trace_print (fn _ => "serving mask for " ^ interface);
		serve_mask (m, valid_protocol, setup, route,
			    received_address_mask, received_router_ad,
			    interface, local_id, min_size, connect))
	    | _ => ();
	   case mask of
	      SOME _ =>	(* do not request mask, only routers *)
	       (Trace.trace_print (fn _ => "requesting router for " ^
				   interface);
		icmp_request (valid_protocol, interface, connect,
			      router_solicitation_if_not_received, 4))
	    | NONE => (* request mask and routers *)
	       let val request_data =
		        marshal_icmp_to_lower (mask_request, min_size,
					       local_id, broadcast_dest,
					       info_icmp_tos, Local_Ttl)
		   fun mask_request_if_not_received () =
		        case Record_String.look (received_address_mask,
						 interface) of
		           NONE => [request_data]
			 | _ => []
		   fun data () =
		        mask_request_if_not_received () @
		        router_solicitation_if_not_received ()
	       in Trace.trace_print (fn _ => "requesting mask and router");
	          icmp_request (valid_protocol, interface, connect, data, 4)
	       end
	end

   val instance_count = ref 0

  in

   fun init_proto (setup as (Setup.Setup setup_list),
		   lower_session, call_status) =
        let fun map_setup {local_id, interface, gateways, mask, mtu} =
	         {interface = interface, address = local_id,
		  gateways = gateways,
		  mask = case mask of SOME (m, _) => SOME m | NONE => NONE}
	    val route = ref (Route.new (B.V.List.map map_setup setup_list))
	    val Lower.S {extension, connect, listen} = lower_session
	    val Lower.Mux_Session_Extension
	          {interfaces = lower_interfaces,
		   set_interface = lower_set_interface,
		   maximum_packet_size, minimum_packet_size} = extension
	    val valid_protocol = ref true
	    val received_address_mask = Record_String.new ()
	    val received_router_ad = Record_String.new ()
	    fun fork_init ({local_id, interface, gateways, mask, mtu}) =
	         if interface <> "lo0" then
	          B.Scheduler.fork
		     (init_interface (valid_protocol, interface, setup,
				      local_id, mask, minimum_packet_size,
				      route, connect, received_address_mask,
				      received_router_ad))
		 else ()
	    val _ = B.V.List.app fork_init setup_list
	    val sent_list = ref (zero64, []: Word64.word ref list)
	    val received_list = ref (zero64, []: Word64.word ref list)
	    fun fold_list base_list =
	         let val (base, list) = ! base_list
		     fun add (a, b) = Word64.+ (! a, b)
		 in B.V.List.fold add list base
		 end
	    fun packets_sent () = fold_list sent_list
	    fun packets_received () = fold_list received_list
	    val failed_send_count = ref zero64
	    val rejected_count = ref zero64
	    fun failed_sends () = ! failed_send_count
	    fun packets_rejected () = ! rejected_count
	    fun map_interface (Lower.Enabled (inf, ip)) = (inf, SOME ip)
	      | map_interface (Lower.Disabled inf) = (inf, NONE)
	    val interfaces = (B.V.List.map map_interface) o lower_interfaces
	    fun set_interface_address (i,a) =
                 lower_set_interface (Lower.Enabled (i, a))
	    fun disable_interface inf =
	         lower_set_interface (Lower.Disabled inf)
	    fun set_subnet_mask (interface, NONE) = 
	         route := Route.unset_interface_mask (! route, interface)
	      | set_subnet_mask (interface, SOME mask) = 
	         route := Route.set_interface_mask (! route, interface, mask)
	    fun add_default_gateway gateway_ip =
	         route := Route.add_default_gateway (! route, gateway_ip)
	    fun remove_default_gateway gateway_ip =
	         route := Route.remove_default_gateway (! route, gateway_ip)
	    fun add_specific_gateway {destination, gateway} =
	         route := Route.add_specific_gateway
		           (! route, {destination = destination,
				      gateway = gateway})
	    fun remove_specific_gateway arg =
	         route := Route.remove_specific_gateway (! route, arg)
	    val new_ext = Session_Extension
			  {packets_sent = packets_sent,
			   packets_received = packets_received,
			   failed_sends = failed_sends,
			   packets_rejected = packets_rejected,
			   interfaces = interfaces,
			   set_interface_address = set_interface_address,
			   disable_interface = disable_interface,
			   set_subnet_mask = set_subnet_mask,
			   add_default_gateway = add_default_gateway,
			   remove_default_gateway = remove_default_gateway,
			   add_specific_gateway = add_specific_gateway,
			   remove_specific_gateway = remove_specific_gateway,
			   specific = ()}
        in instance_count := ! instance_count + 1;
	   Trace.trace_print (fn _ => "initialized protocol instance " ^
			      Integer.toString (! instance_count));
	   (Protocol_State {setup = setup,
			    valid_instance = valid_protocol,
			    instance_id = ! instance_count,
			    route = route,
			    lower_connect = connect,
			    lower_session_ext = extension,
			    call_status = call_status,
			    received_address_mask = received_address_mask,
			    received_router_ad = received_router_ad,
			    sent_list = sent_list,
			    received_list = received_list,
			    failed_send_count = failed_send_count,
			    rejected_count = rejected_count},
	    new_ext)
        end
  end

(*
	23.	function fin_proto
*)

  fun fin_proto (Protocol_State {valid_instance, instance_id, ...}) =
       (Trace.trace_print (fn _ => "invalidating protocol instance " ^
			   Integer.toString instance_id);
	valid_instance := false)

(*
	24.	function resolve
*)

  fun resolve (Protocol_State {route, instance_id, ...}: protocol_state,
	       Network_Address.Address {peer, proto}) =
       case Route.resolve (! route, peer) of
	  NONE =>
	   (Trace.local_print ("unable to resolve " ^ Host_Id.makestring peer ^
			       ", state is " ^ Route.makestring (! route) ^
			       " protocol instance " ^
			       Integer.toString instance_id);
	    NONE)
	| SOME {interface, interface_ip, next_hop} =>
	   case next_hop of
	      Route.Unicast {next_hop} =>
	       SOME (Lower.Mux_Address.Unicast {interface = interface,
						peer = next_hop})
	    | Route.Broadcast =>
	       SOME (Lower.Mux_Address.Broadcast interface)
	    | Route.Loopback =>
               (Trace.trace_print (fn _ =>
				   "resolve returned loopback for peer " ^
				   Host_Id.makestring peer ^
				   " over interface " ^ interface ^
				   ", routing state is " ^
				   Route.makestring (! route));
		SOME (Lower.Mux_Address.Unicast {interface = interface,
						 peer = peer}))

(*
	25.	function make_key
*)

  fun make_key (_, Network_Address.Address {peer, proto}, lower_key, _) =
       Connection_Key.CK {peer = peer, proto = proto,
			  interface = Lower.Mux_Connection_Key.interface
			                  lower_key}

(*
	26.	function map_pattern
*)

  fun map_pattern _ = SOME ((), Lower.Mux_Pattern.All)

(*
	27.	function match
*)

  fun match (_, Network_Pattern.Complete address, _,
	     Connection_Key.CK {peer, proto, interface}) =
       Network_Address.equal (Network_Address.Address address,
			      Network_Address.Address {peer = peer,
						       proto = proto})
    | match (_, Network_Pattern.Partial {proto = proto1}, _,
	     Connection_Key.CK {peer, proto = proto2, interface}) =
       proto1 = proto2

(*
	28.	function undelivered
*)

  local
   val default_icmp_tos = 0w0 : Word8.word (* RFC 1122, p. 73;
					      RFC 1700, p. 66 *)

   val default_unreachable_ttl = Given_Ttl 0w255 (* try to deliver the info *)

  (* we return the first 72 bytes of data or the entire packet, whicheve
     is less.  72 bytes is guaranteed to include the IP header (max 64
     bytes) and at least 8 bytes beyond that, as required (RFC 792). *)
   fun send_unreachable (reason, packet, dlen, source, dest, tos, min_size,
			 Lower.C {send, ...}) =
	let val max_len = Word.fromInt 72
	    val len = if dlen > max_len then max_len else dlen
	    val (data, _) = Lower.Incoming.split (packet, len)
	    val raw = Lower.Incoming.sub (data, {start = 0w0, length = len})
	    val response = Icmp_Out.Unreachable
	                     (reason,
			      Network_Outgoing.new raw)
	    val out = marshal_icmp_to_lower (Icmp_Out.marshal response,
					     min_size, dest, source,
					     default_icmp_tos,
					     default_unreachable_ttl)
	in send out
	end

  in
   fun undelivered_reason (reason, interface,
			   ps as Protocol_State {route, lower_connect,
						 lower_session_ext, ...})
	                  (lower_conn, data) =
       ((let val (header, _) =
	           ((Header.unmarshal (data, 0w0))
		     handle x =>
		             Trace.print_raise_again (x, SOME "undelivered"))
	     val Header.V4 {source, destination, protocol, tos,
			    data_length, flags, ...} = header
	     val dest_addr = Network_Address.Address
	                       {peer = source, proto = icmp_protocol}
	     val Lower.Mux_Session_Extension {minimum_packet_size, ...} =
	          lower_session_ext
	     val min_size = minimum_packet_size interface
	     val dlen = Word.fromInt (Word16.toInt data_length)
	                + Header.size header
	     val respond_flags =
	          case flags of
		     Header.Flag.None => true
		   | Header.Flag.Dont_Fragment => true
		   | Header.Flag.Fragment offset => offset = zero16
		   | Header.Flag.Last_Fragment _ => false
	 in if respond_flags andalso
	       Route.valid_incoming (! route, interface, destination) andalso
	       Route.is_unicast_address (! route, destination) andalso
	       Route.is_unicast_address (! route, source) andalso
	       not (Protocol_Id.equal (protocol, icmp_protocol)) then
	     send_unreachable (reason, data, dlen, source, destination,
			       tos, min_size, lower_conn)
	    else ()
	 end)
	  handle x => Trace.print_handled (x, SOME "undelivered"))

   fun undelivered (lower_key, protocol_state) =
        undelivered_reason (Unreachable.Protocol_Unreachable,
			    Lower.Mux_Connection_Key.interface lower_key,
			    protocol_state)
  end

(*
	29.	type connection_state
*)

  datatype connection_state =
      Connection_State of {protocol_state: protocol_state,
			   lower_conn: Lower.connection,
			   local_address: Host_Id.T,
			   interface: string,
			   fragment: Fragment.T ref,
			   fragment_semaphore: B.Semaphore.T,
			   valid: bool ref,
			   sent: Word64.word ref,
			   received: Word64.word ref,
			   identification: Word16.word ref,
			   ttl: Word8.word ref,
			   tos: Word8.word ref,
			   min_packet: Word.word,
			   max_packet: Word.word}

(*
	30.	function compute_header
*)

  val ip_min_header_size = 0w20

  fun compute_header (packet, tos, identification, flags, ttl,
		      proto, source, dest, options) =
	let val data_length = Lower.Outgoing.size packet
	    val header = Header.V4 {tos = tos,
				    data_length =
				      Word16.fromInt (Word.toInt data_length),
				    identification = identification,
				    flags = flags,
				    ttl = ttl,
				    protocol = proto,
				    source = source,
				    destination = dest,
				    options = options}
	    val header_length = case options of
	                           [] => ip_min_header_size
				 | _ => Header.size header
	    val total_length = data_length + Header.size header
	in (header, header_length, total_length)
	end

(*
	31.	functions pseudo_check, init_connection
*)

  local
   val default_ttl = 0w64 : Word8.word		(* RFC 1700, p. 64 *)
   val default_tos = 0w0 : Word8.word		(* default *)
   val low16_mask32 = Word32.fromInt 0xffff
   val ip_max_header_plus_eight = 0w68

   fun expire lower_conn {header, fragment, raw_packet} =
        let val icmp = Icmp_Out.Reassembly_Time_Exceeded
	                (convert_packet raw_packet)
	    val Network_Outgoing.Outgoing (full_data, opts) =
                      Icmp_Out.marshal icmp
            val length = Lower.Outgoing.size full_data
            val send_size = if length > ip_max_header_plus_eight then
                             ip_max_header_plus_eight
                            else length
	    val (data, _) =
                Lower.Outgoing.split (full_data, send_size)
	    val Header.V4 {source, destination, ...} = header
	    val (new_header, header_length, length) =
	          compute_header (data, default_tos,
				  zero16, Header.Flag.None,
				  default_ttl, icmp_protocol,
				  source, destination, opts)
	    val hdata = Lower.Outgoing.uninitialized header_length
	    val _ = Header.marshal (hdata, new_header) 0w0
	    val packet = Lower.Outgoing.join (hdata, data)
(* we can use Lower.C's send because the connection is to a gateway
   that will take us to the host that sent the fragment, or to the
   host itself if that host is on the same network as us. *)
	    val Lower.C {send, ...} = lower_conn
	in send packet
	end

   fun port_unreachable (interface, protocol_state, lower_conn) data =
        let val Network_Incoming.Incoming (_, info) = data
	in case info of
	      Network_Incoming.None =>
	       Trace.local_print "received port_unreachable but no raw data"
	    | Network_Incoming.Info {raw_packet, ...} =>
	       undelivered_reason (Unreachable.Port_Unreachable, interface,
				   protocol_state) (lower_conn, raw_packet)
	end

  in
   fun pseudo_check (source, destination, data_length, protocol) =
        let val add = B.Checksum.one_s_add
	    fun w32_pseudo value =
	         add (Word16.fromInt
		      (Word32.toInt
		       (Word32.andb (value, low16_mask32))),
		      Word16.fromInt
		      (Word32.toInt
		       (Word32.>> (value, 0w16))))
	    val make16 = Word16.fromInt o Word8.toInt
        in add (add (w32_pseudo source, w32_pseudo destination),
	        add (data_length, make16 protocol))
        end

   fun init_connection (state,
			key as (Connection_Key.CK {peer, proto, interface}),
			lower_conn) =
        let val _ = Trace.trace_print (fn _ =>
				       "init_connection called for key " ^
				       Connection_Key.makestring  key)
	    val Protocol_State
	           {valid_instance, route, lower_connect, lower_session_ext,
		    call_status, sent_list, received_list, ...} = state
	    val local_ip =
	           case Route.address_for_interface (! route, interface) of
		      SOME ip => ip
		    | NONE =>
		       let val s = "no local IP on interface " ^ interface
			   val x = X.Connection s
			   val name = SOME "init_connection"
		       in Trace.local_print (s ^  ", router state is " ^
					     Route.makestring (! route));
			  Trace.print_raise (x, name)
		       end
	    val semaphore = B.Semaphore.new ()
	    fun null_fragment_fun {header: Fragment.ip_header,
				   fragment: Fragment.incoming,
				   raw_packet: Fragment.incoming} = ()
	    val fragment_fun = if Protocol_Id.equal (proto, icmp_protocol) then
	                        null_fragment_fun
			       else expire lower_conn
	    val fragment_state = ref (Fragment.new fragment_fun)
	    val continue_thread = ref true
	    fun do_gc () =
		 (fragment_state := Fragment.gc (! fragment_state);
		  route := Route.gc (! route))
	    fun gc_thread () =
	         (B.Scheduler.sleep 1000;
		  if ! continue_thread then
		   (((B.Semaphore.with_lock (semaphore, do_gc, ()))
		     handle x =>
		             Trace.print_handled (x, SOME "gc_thread"));
		    gc_thread ())
		  else ())
	    val Lower.Mux_Session_Extension
	        {maximum_packet_size, minimum_packet_size, interfaces,
		 set_interface} = lower_session_ext
	    val max_packet = maximum_packet_size interface
	    val min_packet = minimum_packet_size interface
	    fun pseudo_header packet =
	         let val length = Word16.fromInt (Word.toInt
						  (Outgoing.size packet))
		 in pseudo_check (local_ip, peer, length, proto)
		 end
	    val ttl = ref default_ttl
	    fun time_to_live () = Word8.toInt (! ttl)
	    fun set_time_to_live new_ttl =
	(* ttl must be at least one: RFC 1122, p. 34 *)
	         if new_ttl <= 255 andalso new_ttl >= 1 then
		  ttl := Word8.fromInt new_ttl
		 else ()
	    val tos = ref default_tos
	    fun type_of_service () = Word8.toInt (! tos)
	    fun set_type_of_service new_tos =
	         if new_tos <= 255 then tos := Word8.fromInt new_tos
		 else ()
	    val sent = ref zero64
	    val received = ref zero64
	    val unreachable = port_unreachable (interface, state, lower_conn)
	    val extension = Connection_Extension
			    {port_unreachable = unreachable,
			     max_packet_size = max_packet - 0w20,
			     can_fragment = true,
			     local_address = local_ip,
			     remote_address = peer,
			     pseudo_header_checksum = pseudo_header,
			     time_to_live = time_to_live,
			     set_time_to_live = set_time_to_live,
			     type_of_service = type_of_service,
			     set_type_of_service = set_type_of_service,
			     packets_sent = fn _ => ! sent,
			     packets_received = fn _ => ! received,
			     specific = ()}
	    val (sent_base, sent_values) = ! sent_list
	    val (received_base, received_values) = ! received_list
        in sent_list := (sent_base, sent :: sent_values);
	   received_list := (received_base, received :: received_values);
	   B.Scheduler.fork gc_thread;
           (Connection_State {protocol_state = state, lower_conn = lower_conn,
			      interface = interface,
			      fragment = fragment_state,
			      fragment_semaphore = semaphore,
			      valid = continue_thread, sent = sent,
			      received = received,
			      identification = ref zero16,
			      local_address = local_ip,
			      ttl = ttl, tos = tos,
			      min_packet = min_packet,
			      max_packet = max_packet},
	    extension)
        end
  end

(*
	32.	function fin_connection
*)

  local
   fun remove_from_list (value, []) = []
     | remove_from_list (value, head :: rest) =
        if value = head then rest else head :: remove_from_list (value, rest)

   fun update_list (value, (base, list)) =
        (Word64.+ (base, ! value), remove_from_list (value, list))

  in
   fun fin_connection (Connection_State {protocol_state, valid,
					 sent, received, ...}) =
        let val Protocol_State {sent_list, received_list, ...} =
	          protocol_state
        in sent_list := update_list (sent, ! sent_list);
           received_list := update_list (received, ! received_list);
	   valid := false
        end
  end

(*
	33.	function send
*)

  local
   fun fail_send (Protocol_State {failed_send_count, ...}, reason) =
	(failed_send_count := Word64.+ (! failed_send_count, one64);
	 Trace.print_raise (X.Send reason, SOME "send"))

   fun join (packet, header, header_size) = 
        let val header_space = Lower.Outgoing.uninitialized header_size
	in Header.marshal (header_space, header) 0w0;
	   Lower.Outgoing.join (header_space, packet)
	end

   fun extend (packet, total_size, min_size) =
        Lower.Outgoing.join (packet,
			     Lower.Outgoing.uninitialized
			                           (min_size - total_size))

   val ip_packet_limit = 0wxffff

  in
   fun send (Connection_Key.CK {peer, proto, interface},
	     Connection_State {protocol_state, sent, identification,
			       local_address, ttl, tos, min_packet,
			       max_packet, ...})
            (Network_Outgoing.Outgoing (packet, options)) =
	let val _ = Trace.debug_print (fn _ => "sending options " ^
				       Option.makestrings options)
	    val (header, header_size, total_length) =
	           compute_header (packet, ! tos,
				   ! identification, Header.Flag.None,
				   ! ttl, proto, local_address, peer, options)
	    val _ = if total_length > ip_packet_limit then
	             fail_send (protocol_state, "packet too large")
		    else ()
	    val packet_header_list =
	         if total_length > max_packet then
		  Fragment.fragment {packet = packet, header = header,
				     max_size = max_packet,
				     min_size = min_packet}
		 else if total_length < min_packet then
		  [(extend (packet, total_length, min_packet),
		    header, header_size)]
		 else [(packet, header, header_size)]
	    val packet_list = B.V.List.map join packet_header_list
	in sent := Word64.+ (! sent, one64);
	   identification := Word16.+ (! identification, one16);
	   Trace.debug_print (fn _ => "sending " ^
			      B.V.List.fold (fn (a, b) => a ^ ", " ^ b)
			        (B.V.List.map Lower.Outgoing.makestring
				              packet_list) "");
	   packet_list
	end
  end

(*
	34.	function identify

	Note that if we're a gateway, packets to be forwarded are
	identified as being protocol 255, which is reserved (RFC 1700,
	p. 10).
*)

  val forward_protocol = Word8.fromInt 255

  local
   fun error_dest (data, route, interface) =
        case (Icmp_Ip_Header.identify (data, 0w0),
	      Route.address_for_interface (! route, interface)) of
	   (SOME {source, dest, protocol, fragment}, SOME iaddr) =>
	            (* check that we're the source. *)
	    if iaddr = source then
	     [Connection_Key.CK {peer = dest, proto = protocol,
				 interface = interface}]
	    else
	     (Trace.local_print ("got ICMP for higher protocol " ^
				 Protocol_Id.makestring protocol ^
				 ", higher protocol header " ^
				 Network_Incoming.makestring_max (data, 0w30) ^
				 " apparently from " ^
				 Host_Id.makestring dest ^
				 " on interface " ^ interface ^
				 " supposedly sent from IP " ^
				 Host_Id.makestring source ^
				 " but interface IP is " ^
				 Host_Id.makestring iaddr);
	      [])
	 | (NONE, _) =>
	    (Trace.local_print ("No IP header on ICMP error message " ^
				Network_Incoming.makestring data);
	     [])
	 | (_, NONE) =>
	    (Trace.local_print ("got ICMP from inactive interface " ^
				interface);
	     [])

   fun identify_icmp (packet, default, route, interface) =
        let val (ip_header, cursor) =
	           ((Header.unmarshal (packet, 0w0))
		    handle x =>
		            Trace.print_raise_again
			    (x, SOME ("identify_icmp, packet " ^
				      Lower.Incoming.makestring packet)))
	    val Header.V4 {data_length, options, ...} = ip_header
	    val info = Network_Incoming.Info {pseudo_check = zero16,
					      raw_packet = packet,
					      options = options}
	    val higher = Network_Incoming.Incoming (packet, info)
	    val (_, no_head) = Network_Incoming.split (higher, cursor)
	    val (restricted, _) =
	          Network_Incoming.split (no_head,
					  Word.fromInt
					    (Word16.toInt data_length))
	in case is_info_icmp restricted of
	      Info_Icmp => default
	    | Not_Info_Icmp icmp =>
	       case icmp of
	          Icmp_In.Unreachable (_, data) =>
		   error_dest (data, route, interface)
	        | Icmp_In.Transit_Time_Exceeded data =>
		   error_dest (data, route, interface)
	        | Icmp_In.Reassembly_Time_Exceeded data =>
		   error_dest (data, route, interface)
	        | Icmp_In.Parameter_Problem (_, data) =>
		   error_dest (data, route, interface)
	        | Icmp_In.Source_Quench data =>
		   error_dest (data, route, interface)
	        | Icmp_In.Redirect {header, ...} =>
		   error_dest (header, route, interface)
	        | Icmp_In.Echo_Reply _ => default
	        | Icmp_In.Traceroute _ => default
	        | Icmp_In.Time_Stamp_Reply _ => default
	        | Icmp_In.Echo _ => default
	        | Icmp_In.Time_Stamp _ => default
		| Icmp_In.Obsolete => [] (* silently discard, RFC 1122 p. 43 *)
		| _ => 
		   (Trace.local_print ("identify_icmp: unknown ICMP " ^
				       Icmp_In.makestring icmp);
		    [])
	end

   fun valid_dest (Protocol_State {route, ...}, interface, destination) =
        Route.valid_incoming (! route, interface, destination)
  in
   fun identify (lower_key, protocol_state) packet =
        case Header.identify (packet, 0w0) of
	   SOME {source, dest, protocol, fragment} =>
	    let val interface = Lower.Mux_Connection_Key.interface lower_key
	    in if valid_dest (protocol_state, interface, dest) then
	        if protocol = icmp_protocol then
		 let val default = [Connection_Key.CK {peer = source,
						       proto = protocol,
						       interface = interface}]
	             val Protocol_State {setup, route, received_address_mask,
					 ...} = protocol_state
		 in if fragment then
		     default
		    else
		     ((identify_icmp (packet, default, route, interface))
		      handle x =>
		              (Trace.print_handled
			       (x, SOME ("identify " ^
					 Host_Id.makestring source ^
					 "->" ^ Host_Id.makestring dest ^
					 "@" ^
					 Protocol_Id.makestring protocol));
			       []))
		 end
		else			(* not ICMP protocol *)
		 [Connection_Key.CK {peer = source, proto = protocol,
				     interface = interface}]
		else if gateway then
		 [Connection_Key.CK {peer = source, proto = forward_protocol,
				     interface = interface}]
		else			(* packet is not for us *)
		 (Trace.trace_print (fn _ => "invalid destination " ^
				     Host_Id.makestring dest ^
				     ", dropping packet");
		  [])
	    end
	 | NONE =>
	    (Trace.local_print ("received non-IP packet " ^
				Lower.Incoming.makestring packet);
	     [])
  end

(*
	35.	function receive

	If we're a gateway, packets to be forwarded are identified as
	being for the forward protocol.
*)

  local
   fun receive_exn (Protocol_State {rejected_count, ...}, x) =
	(rejected_count := Word64.+ (! rejected_count, one64);
	 Trace.print_handled (x, SOME "receive");
	 NONE)

   open Network_Status			(* many, many constructors *)

   fun deliver_unreachable (key, data, call_status,
			    Unreachable.Network_Unreachable) =
        call_status (key, Unreachable (Routing, data))
     | deliver_unreachable (key, data, call_status,
			    Unreachable.Host_Unreachable) =
	call_status (key, Unreachable (Routing, data))
     | deliver_unreachable (key, data, call_status,
			    Unreachable.Protocol_Unreachable) =
	call_status (key, Inaccessible (Protocol, data))
     | deliver_unreachable (key, data, call_status,
			    Unreachable.Port_Unreachable) =
	call_status (key, Inaccessible (Port, data))
     | deliver_unreachable (key, data, call_status,
			    Unreachable.Source_Route_Failed) =
	call_status (key, Unreachable (Source_Route_Failed, data))
     | deliver_unreachable (key, data, call_status,
			    Unreachable.Fragmentation_Needed {mtu}) =
	let val mtu = Word16.toInt mtu - Word.toInt ip_min_header_size
	    val unreachable = Unreachable (Fragmentation_Needed {mtu = mtu},
					   data)
	in call_status (key, unreachable)
	end
     | deliver_unreachable (key, data, call_status, _) =
	call_status (key, Inaccessible (Other, data))

   fun print_problem (problem, data) =
        Trace.local_print ("received ICMP Parameter Problem " ^
			   Icmp_In.makestring_problem problem ^
			   " on IP packet " ^
			   Network_Incoming.makestring data)

   fun make_higher data =
        case Icmp_Ip_Header.lengths (data, 0w0) of
	   SOME {header = hlen, data = dlen} =>
	    let val (_, higher_data) = Network_Incoming.split (data, hlen)
	        val (real_data, _) =
	              if Network_Incoming.size higher_data > dlen then
		       Network_Incoming.split (higher_data, dlen)
		      else (higher_data, higher_data)
	    in Trace.trace_print (fn _ => Word.toString
			          (Network_Incoming.size real_data) ^
			          " bytes being delivered as ICMP data");
	       real_data
	    end
	 | NONE => 
	    Network_Incoming.uninitialized 0w0

   fun deliver_problem (key, data, call_status, problem) =
        case problem of
	   Icmp_In.Header pointer => print_problem (problem, data)
	 | Icmp_In.Option pointer => print_problem (problem, data)
	 | Icmp_In.End_Of_Options => print_problem (problem, data)
	 | Icmp_In.Missing_Required_Option =>
	    call_status (key, Unreachable (Missing_Option, make_higher data))
	 | Icmp_In.Data pointer =>
	    call_status (key,
			 Unreachable (Parameter_Problem
				      (Word8.toInt pointer),
				      make_higher data))

(* RFC 1122, p. 40
    A Redirect message SHOULD be silently discarded if the new
    gateway address it specifies is not on the same connected
    (sub-) net through which the Redirect arrived [INTRO:2, Appendix A],
    or if the source of the Redirect is not the current first-hop
    gateway for the specified destination (see Section 3.3.1).
 *)
   fun execute_redirect (protocol_state,
			 Connection_Key.CK {peer = old_gateway, proto,
					    interface},
			 {reason, new_gateway, header}) =
        let val Protocol_State {route, ...} = protocol_state
	    val (Icmp_Ip_Header.V4 {destination, ...}, _) = 
	          ((Icmp_Ip_Header.unmarshal (header, 0w0))
		   handle x =>
		           Trace.print_raise_again (x,
						    SOME "execute_redirect"))
	    val args = {destination = destination, gateway = new_gateway}
	in case Route.resolve (! route, destination) of
	      NONE =>
	       Trace.local_print ("error, got redirect from " ^
				  Host_Id.makestring old_gateway ^
				  " for destination " ^
				  Host_Id.makestring destination ^
				  " to " ^
				  Host_Id.makestring new_gateway ^
				  " but unable to resolve destination")
	    | SOME {interface, interface_ip,
		    next_hop = Route.Unicast {next_hop}} =>
	       if next_hop = old_gateway andalso next_hop <> destination then
			     (* we can use this redirect *)
		(case Route.resolve (! route, new_gateway) of
		    SOME {interface, interface_ip,
			  next_hop = Route.Unicast {next_hop}} =>
		     if next_hop = new_gateway then
		      route := Route.add_specific_gateway (! route, args)
		     else
		      Trace.local_print ("error, got redirect from " ^
					 Host_Id.makestring old_gateway ^
					 " for destination " ^
					 Host_Id.makestring destination ^
					 " to " ^
					 Host_Id.makestring new_gateway ^
					 " but new gateway is not on subnet")
		  | _ =>
		     Trace.local_print ("error, got redirect from " ^
				        Host_Id.makestring old_gateway ^
				        " for destination " ^
				        Host_Id.makestring destination ^
				        " to " ^
				        Host_Id.makestring new_gateway ^
					" but new gateway is unreachable"))
	       else
		Trace.local_print ("error, got redirect from " ^
				   Host_Id.makestring old_gateway ^
				   " for destination " ^
				   Host_Id.makestring destination ^
				   " to " ^
				   Host_Id.makestring new_gateway ^
				   " but old gateway is " ^
				   Host_Id.makestring next_hop)
	    | SOME {interface, interface_ip, next_hop} =>
	       Trace.local_print ("error, got redirect from " ^
				  Host_Id.makestring old_gateway ^
				  " for destination " ^
				  Host_Id.makestring destination ^
				  " to " ^
				  Host_Id.makestring new_gateway ^
				  " but old gateway is " ^
				  (if next_hop = Route.Broadcast then
				    "broadcast"
				   else "loopback"))
	end

   (* see RFC 1122, pp 38 and following for details *)
   fun deliver_icmp (key, icmp,
		     (ps as (Protocol_State {call_status, ...}))) =
	case icmp of
	   Icmp_In.Unreachable (unreachable, data) =>
	    deliver_unreachable (key, make_higher data,
				 call_status, unreachable)
	 | Icmp_In.Transit_Time_Exceeded data =>
	    call_status (key, Unreachable (Time_To_Live_Exceeded,
					   make_higher data))
	 | Icmp_In.Reassembly_Time_Exceeded data =>
	    call_status (key, Unreachable (Reassembly_Time_Exceeded,
					   make_higher data))
	 | Icmp_In.Parameter_Problem (problem, data) =>
	    deliver_problem (key, data, call_status, problem)
	 | Icmp_In.Source_Quench data =>
	    call_status (key, Quench (make_higher data, ()))
	 | Icmp_In.Redirect args => execute_redirect (ps, key, args)
	 | _ =>
	    Trace.print_raise (X.Connection "internal error",
			       SOME "deliver_icmp")

   fun compute_pseudo_check header =
        let val Header.V4 {data_length, protocol, source,
			   destination, ...} = header
	in pseudo_check (source, destination, data_length, protocol)
        end

   fun forward_packet (Header.V4 {tos, data_length, identification, flags,
				  ttl, protocol, source, destination,
				  options},
		       interface, data, protocol_state) =
        Trace.local_print ("should be forwarding packet from " ^
			   Host_Id.makestring source ^ " to " ^
			   Host_Id.makestring destination ^ " received on " ^
			   interface ^ ", not implemented")

   fun process (key as (Connection_Key.CK {proto, ...}), data, header,
		raw_packet, state, protocol_state) =
        let val Connection_State {received, ...} = state
	    val Header.V4 {data_length, options, source,
			   protocol, ...} = header
	    val pseudo_check = compute_pseudo_check header
	    val higher_info = Network_Incoming.Info
	                         {pseudo_check = pseudo_check,
				  raw_packet = raw_packet,
				  options = options}
	    val actual_data = Network_Incoming.Incoming (data, higher_info)
        in received := Word64.+ (! received, one64);
	   if proto = protocol andalso proto <> icmp_protocol then
	    SOME actual_data
	   else				(* ICMP or forward packet *)
	    let val Protocol_State {setup, route, instance_id,
				    received_address_mask,
				    received_router_ad, ...} = protocol_state
		val Connection_State {min_packet, lower_conn, ...} = state
		val Lower.C {send, ...} = lower_conn
		val signal_done = B.Pipe.new (): unit B.Pipe.T
		val Header.V4 {destination, tos, ...} = header
		val Connection_Key.CK {peer, proto, interface} = key
		val local_id =
		      case Route.address_for_interface (! route, interface) of
		         NONE =>
			  (Trace.local_print ("recvd on inactive interface " ^
					      interface ^
					      ", assuming dest address " ^
					      Host_Id.makestring destination ^
					      " is correct");
			   destination)
		       | SOME ip => ip
	    in if protocol = icmp_protocol then
	        case process_info_icmp (setup, route, instance_id,
					received_address_mask,
					received_router_ad, interface,
					actual_data, send, signal_done,
					min_packet, local_id, peer, tos) of
	           Info_Icmp =>
		    (* send the message up to the Icmp layer so we can
		       use it to close the Icmp connection that was
		       opened by the result of identify. *)
		     SOME actual_data
		 | Not_Info_Icmp icmp =>
		    if proto = icmp_protocol then (* "user" ICMPs. *)
		     SOME actual_data
		    else		(* some ICMP error *)
		     (deliver_icmp (key, icmp, protocol_state);
		      NONE)
	       else
		(forward_packet (header, interface, data, protocol_state);
		 NONE)
	    end
        end

   fun reassemble (fragment, key, data, header, raw_packet,
		   connection_state, protocol_state, sem) =
        (B.Semaphore.acquire sem;
         case Fragment.reassemble (! fragment, (data, header, raw_packet)) of
	    (NONE, state) =>
	     (fragment := state;
	      B.Semaphore.release sem;
	      NONE)
	  | (SOME (new_data, new_header, new_raw), state) =>
	     (fragment := state;
	      B.Semaphore.release sem;
	      process (key, new_data, new_header, new_raw,
		       connection_state, protocol_state)))

  in
   fun receive (key, state as
		(Connection_State {protocol_state, local_address, fragment,
				   fragment_semaphore, ...})) packet =
	((let val (header, position) =
	             ((Header.unmarshal (packet, 0w0))
		      handle x => Trace.print_raise_again (x, SOME "receive"))
	      val Header.V4 {flags, data_length, ...} = header
	      val (_, long_data) = Lower.Incoming.split (packet, position)
	      val (data, _) = Lower.Incoming.split
	                        (long_data, 
				 Word.fromInt (Word16.toInt data_length))
	      fun no_frag () = process (key, data, header, packet,
					state, protocol_state)
	  in case flags of
	        Header.Flag.None => no_frag ()
	      | Header.Flag.Dont_Fragment => no_frag ()
	      | _ =>
		 let val Connection_Key.CK {peer, proto, interface} = key
		 in if proto = forward_protocol then
		     no_frag () (* forward this packet, so don't reassemble *)
		    else
		     reassemble (fragment, key, data, header, packet,
				 state, protocol_state, fragment_semaphore)
		 end
	  end)
	   handle x => receive_exn (protocol_state, x))
  end

(*
	36.	function lower_status
*)

  fun lower_status _ status =
       Trace.local_print ("received lower status " ^
			  Lower.Status.makestring status)

(*
	37.	structure Connection
*)

  structure Connection =
      Connection (structure Lower = Lower
		  structure Setup = Setup
		  structure Address = Address
		  structure Pattern = Pattern
		  structure Connection_Key = Connection_Key
		  structure Incoming = Incoming
		  structure Outgoing = Outgoing
		  structure Status = Status
		  structure Count = Count
		  structure X = X
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
		  val module_name = "ip.fun"
		  val debug_level = debug_level)

(*
	38.	exported types
*)
  local
   structure Export_Connection:
    sig
     structure Count: COUNT
     structure X: PROTOCOL_EXCEPTIONS
     exception Already_Open of Connection_Key.T
     type connection_extension
     type listen_extension
     type session_extension
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
    end
   = Connection
  in
   open Export_Connection
  end

(*
	39.	structure Icmp: structures, types
*)

  structure Icmp =
   struct
    structure Setup = Setup
    structure Address = Host_Id
    structure Connection_Key = Address
    structure Count = Count
    structure X = X
    structure Unit: KEY =
     struct
      type T = unit
      fun makestring () = ""
      fun equal _ = true
      fun hash _ = 0w0
     end
    structure Pattern = Unit
    structure Status = Status

    exception Already_Open of Connection_Key.T

    type connection_extension = network_connection_extension
    type listen_extension = unit
    type session_extension = unit

(*
	40.	structure Icmp.Incoming
*)

    exception Icmp_Data_Operation_Not_Supported
    fun not_supp _ =
         Trace.print_raise (Icmp_Data_Operation_Not_Supported, SOME "icmp")

    type ip_number = Host_Id.T
    type data_in = Incoming.T
    type data_out = Outgoing.T
    type id = Word16.word
    type seq = Word16.word
    type timestamp = Word32.word
    datatype icmp_in =
        Echo_Reply of {id: id, sequence: seq, data: data_in}
      | Timestamp_Reply of {id: id, sequence: seq, originate: timestamp,
			    receive: timestamp, transmit: timestamp,
			    returned: timestamp}
      | Traceroute of {forwarded: bool, id: Word16.word,
		       out_hops: Word16.word, return_hops: Word16.word,
		       out_speed: Word32.word, out_mtu: Word32.word}
    structure Incoming =
     struct
      type T = icmp_in
      val new = not_supp
      val uninitialized = not_supp
      val size = not_supp
      val sub = not_supp
      val update = not_supp
      val join = not_supp
      val split = not_supp
      val fold = not_supp
      fun makestring_in (Echo_Reply {id, sequence, data}, makestring) =
           "echo reply (id = " ^ (Integer.toString o Word16.toInt) id ^
           ", sequence = " ^ (Integer.toString o Word16.toInt) sequence ^
           ", data = " ^ makestring data ^ ")"
        | makestring_in (Timestamp_Reply {id, sequence, originate,
					  receive, transmit, returned}, _) =
           "timestamp reply (id = " ^ (Integer.toString o Word16.toInt) id ^
           ", sequence = " ^ (Integer.toString o Word16.toInt) sequence ^
           ", originate = " ^ Word32.fmt StringCvt.DEC originate ^
           ", receive = " ^ Word32.fmt StringCvt.DEC receive ^
           ", transmit = " ^ Word32.fmt StringCvt.DEC transmit ^
           ", returned = " ^ Word32.fmt StringCvt.DEC returned ^ ")"
        | makestring_in (Traceroute {forwarded, id, out_hops, return_hops,
				     out_speed, out_mtu}, _) =
           "traceroute (id = " ^ (Integer.toString o Word16.toInt) id ^
           ", out_hops = " ^ (Integer.toString o Word16.toInt) out_hops ^
           ", return_hops = " ^ (Integer.toString o Word16.toInt) return_hops ^
           ", out_speed = " ^ Word32.fmt StringCvt.DEC out_speed ^
           ", out_mtu = " ^ Word32.fmt StringCvt.DEC out_mtu ^
	   (if forwarded then "," else ", not") ^ " forwarded)"

      fun makestring x = makestring_in (x, Incoming.makestring)

      fun makestring_max (x, count) =
           makestring_in (x, fn d => Incoming.makestring_max (d, count))

     end

(*
	41.	structure Icmp.Outgoing
*)

    datatype icmp_out =
        Echo_Request of {id: id, sequence: seq, data: data_out}
      | Timestamp_Request of {id: id, sequence: seq}
      | Source_Quench of data_in
      | Parameter_Problem of data_in * int
      | Missing_Required_Option of data_in
    structure Outgoing =
     struct
      type T = icmp_out
      val new = not_supp
      val uninitialized = not_supp
      val size = not_supp
      val sub = not_supp
      val update = not_supp
      val join = not_supp
      val split = not_supp
      val fold = not_supp

      fun makestring_out (Echo_Request {id, sequence, data}, makestring, _) =
           "echo request (id = " ^ (Integer.toString o Word16.toInt) id ^
           ", sequence = " ^ (Integer.toString o Word16.toInt) sequence ^
           ", data = " ^ makestring data ^ ")"
        | makestring_out (Timestamp_Request {id, sequence}, _, _) =
           "timestamp request (id = " ^ (Integer.toString o Word16.toInt) id ^
           ", sequence = " ^ (Integer.toString o Word16.toInt) sequence ^ ")"
        | makestring_out (Source_Quench data, _, makestring) =
           "source quench " ^ makestring data
        | makestring_out (Parameter_Problem (data, pos), _, makestring) =
           "parameter problem at position " ^ Integer.toString pos ^
	   " of " ^ makestring data
        | makestring_out (Missing_Required_Option data, _, makestring) =
           "missing required option" ^
	   (case data of
	       Network_Incoming.Incoming (_, Network_Incoming.None) => ""
	     | Network_Incoming.Incoming
		 (_, Network_Incoming.Info {options, ...}) =>
		   ", options are " ^ Option.makestrings options)

      fun makestring x =
           makestring_out (x, Outgoing.makestring,
			   Network_Incoming.makestring)

      fun makestring_max (x, count) =
           makestring_out (x, fn d => Outgoing.makestring_max (d, count),
			   fn d => Network_Incoming.makestring_max (d, count))

     end

(*
	42.	Icmp types connection, listen, handler, session
*)

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
	43.	Icmp.session
*)

    local
     fun serving_mask (_, Setup.Setup []: Setup.T) = NONE
       | serving_mask (wanted_interface,
		       Setup.Setup ({mask, interface, ...} :: rest)) =
          case mask of
	     NONE => serving_mask (wanted_interface, Setup.Setup rest)
           | SOME (m, {serve}) =>
	      if serve andalso wanted_interface = interface then
	       SOME (interface, m)
	      else serving_mask (wanted_interface, Setup.Setup rest)

     fun original (Network_Incoming.Incoming (data, Network_Incoming.None)) =
          Network_Outgoing.uninitialized 0w0
       | original (Network_Incoming.Incoming
		   (data, Network_Incoming.Info {raw_packet, ...})) =
	  let val (ip_header, pos) =
	            ((Header.unmarshal (raw_packet, 0w0))
		     handle x => Trace.print_raise_again (x, SOME "original"))
	      val Header.V4 {data_length, ...} = ip_header
	      val tlen = Word.fromInt (Word16.toInt data_length) + pos
	      val (new_data, _) =
	            if tlen < Lower.Incoming.size raw_packet then
		     Lower.Incoming.split (raw_packet, tlen)
		    else (raw_packet, raw_packet)
	  in convert_packet new_data
	  end

   (* 255 is the TTL for ICMP ttls used by BSD tahoe 4.3 and CISCO
      routers, according to the traceroute man page.
      The pipe is signaled with true to tell the (listen) connection
      handler to hang around for a while, with false to tell it to
      go away immediately.  So for ICMP messages directed at a user
      process we ignore the pipe, for messages that are most likely
      the first in a sequence we signal true, and for all other
      messages we send false. *)
     fun dispatch_icmp (data_handler, set_ttl, conn, send, data, setup, pipe) =
          ((set_ttl 255;
            case Icmp_In.unmarshal data of
	       Icmp_In.Echo_Reply {id, sequence, data} =>
		data_handler (conn, Echo_Reply {id = id, sequence = sequence,
						data = data})
	     | Icmp_In.Time_Stamp_Reply x =>
		data_handler (conn, Timestamp_Reply x)
	     | Icmp_In.Traceroute x =>
		(data_handler (conn, Traceroute x);
		 B.Pipe.enqueue (pipe, true))
	     | Icmp_In.Echo {id, sequence, data} =>
		let fun make_single (item, rest) = 
	                 Network_Outgoing.join (Network_Outgoing.new item,
						rest)
		    val empty = Network_Outgoing.uninitialized 0w0
		    fun make_packet data =
		         Network_Incoming.fold (data, make_single, empty)
		    val out_data = make_packet data
		    val out = {id = id, sequence = sequence, data = out_data}
		in set_ttl 255;
		   send (Icmp_Out.marshal (Icmp_Out.Echo_Reply out));
		   B.Pipe.enqueue (pipe, true)
		end
	     | Icmp_In.Time_Stamp x =>
		(send (Icmp_Out.marshal (Icmp_Out.Time_Stamp_Reply x));
		 B.Pipe.enqueue (pipe, true))
(* ignore the following four (five), they have been processed.
   However, send false on the pipe so the connection handler will complete. *)
	     | Icmp_In.Mask_Reply {id, sequence, address_mask} =>
		B.Pipe.enqueue (pipe, false)
	     | Icmp_In.Mask_Request {id, sequence = sequence} =>
		B.Pipe.enqueue (pipe, false)
	     | Icmp_In.Router_Solicitation =>
		B.Pipe.enqueue (pipe, false)
	     | Icmp_In.Router_Advertisement {lifetime, addresses} =>
		B.Pipe.enqueue (pipe, false)
	     | Icmp_In.Experimental_Router_Ad =>
		B.Pipe.enqueue (pipe, false)
	     | icmp =>
		(Trace.local_print ("got unexpected icmp " ^
				    Icmp_In.makestring icmp);
		B.Pipe.enqueue (pipe, false)))
	  handle x => Trace.print_handled (x, SOME "dispatch_icmp"))

     fun send_wrap send (Echo_Request {id, sequence, data}) =
          send (Icmp_Out.marshal
		(Icmp_Out.Echo {id = id, sequence = sequence, data = data}))
       | send_wrap send (Timestamp_Request {id, sequence}) =
          send (Icmp_Out.marshal
		(Icmp_Out.Time_Stamp {id = id, sequence = sequence}))
       | send_wrap send (Source_Quench data) =
          send (Icmp_Out.marshal (Icmp_Out.Source_Quench (original data)))
       | send_wrap send (Parameter_Problem (data, pos)) =
          send (Icmp_Out.marshal
		(Icmp_Out.Parameter_Problem ({pointer =
					      Word8.fromInt pos,
					      data = original data})))
       | send_wrap send (Missing_Required_Option data) =
          send (Icmp_Out.marshal
		(Icmp_Out.Missing_Required_Option (original data)))

     fun connect_wrap (Setup.Setup setup, connect) (peer, H handler) =
          let val {connection_handler, data_handler,
		   status_handler} = handler peer
	      val pipe = B.Pipe.new ()	(* unused, except by dispatch_icmp *)
		       : bool B.Pipe.T
	      fun my_conn_handler (Connection.C {send, abort, extension}) =
	           connection_handler (C {send = send_wrap send,
					  abort = abort,
					  extension = extension})
	      fun my_data_handler (Connection.C {send, abort, extension},
				   data) =
	           let val conn = C {send = send_wrap send,
				     abort = abort, extension = extension}
		       val Connection_Extension
			      {set_time_to_live = set_ttl, ...} = extension
	           in dispatch_icmp (data_handler, set_ttl, conn, send, data,
				     Setup.Setup setup, pipe)
		   end
	      fun my_status_handler (Connection.C {send, abort, extension},
				     status) =
	           let val conn = C {send = send_wrap send,
				     abort = abort, extension = extension}
	           in status_handler (conn, status)
		   end
	      val handlers = {connection_handler = my_conn_handler,
			      data_handler = my_data_handler,
			      status_handler = my_status_handler}
	  in ((connect (Network_Address.Address {peer = peer,
						 proto = icmp_protocol},
			Connection.H (fn _ => handlers)))
	      handle Already_Open key =>
	              Trace.local_print ("ICMP connect_wrap connection failed"
					 ^ ", connection " ^
					 Connection_Key.makestring key ^
					 " already open")
		   | Connection.Already_Open key =>
	              Trace.local_print ("ICMP connect_wrap connection failed"
					 ^ ", IP connection " ^
					 Connection.Connection_Key.makestring
					 key ^ " already open")
		   | x =>
		      Trace.print_raise_again (x, SOME "ICMP connect_wrap"))
	  end

     datatype listen_record =
	 Not_Listening
       | Listening of {stop: unit -> unit, count: int ref}
     val listen_state = ref Not_Listening
     val listen_pattern = Network_Pattern.Partial {proto = icmp_protocol}

     fun empty_pipe pipe =
          if B.Pipe.size pipe > 0 then
	   (B.Pipe.dequeue pipe;
	    empty_pipe pipe)
	  else ()

     fun conn_handler_loop (pipe, key) =
          (empty_pipe pipe;
	   Trace.trace_print (fn _ => "ICMP conn_handler_loop for " ^
			      Connection.Connection_Key.makestring key ^
			      " sleeping");
	   case B.Pipe.dequeue_timeout (pipe, 2345) of
	      NONE =>
	       Trace.trace_print (fn _ => "ICMP conn_handler_loop for " ^
				  Connection.Connection_Key.makestring key ^
				  " timed out")
	    | SOME sleep =>
	       (Trace.trace_print (fn _ => "ICMP conn_handler for " ^
				   Connection.Connection_Key.makestring key ^
				   " done");
		if sleep then conn_handler_loop (pipe, key) else ()))

     fun conn_handler (pipe, key) arg =
          (Trace.trace_print (fn _ => "ICMP conn_handler for " ^
			      Connection.Connection_Key.makestring key ^
			      " sleeping");
	   case B.Pipe.dequeue_timeout (pipe, 50) of
	      NONE =>
	       Trace.trace_print (fn _ => "ICMP conn_handler for " ^
				  Connection.Connection_Key.makestring key ^
				  " timed out")
	    | SOME sleep =>
	       (Trace.trace_print (fn _ => "ICMP conn_handler for " ^
				   Connection.Connection_Key.makestring key ^
				   " done");
		if sleep then conn_handler_loop (pipe, key) else ()))

     fun listen_data_handler (pipe, setup)
                             (Connection.C {send, abort, extension}, data) =
          let val conn = C {send = send_wrap send, abort = abort,
			    extension = extension}
	      val Connection_Extension {set_time_to_live = set_ttl,
					...} = extension
	  in dispatch_icmp (fn _ => (), set_ttl, conn, send, data, setup, pipe)
	  end

     fun listen_handler setup key =
          let val pipe = B.Pipe.new (): bool B.Pipe.T
	  in {connection_handler = conn_handler (pipe, key),
	      data_handler = listen_data_handler (pipe, setup),
	      status_handler = fn _ => ()}
	  end

     fun do_listen (setup, listen) =
          let val _ = Trace.trace_print (fn _ => "starting ICMP listen")
	      val Connection.L {stop, ...} =
		    listen (listen_pattern,
			    Connection.H (listen_handler setup),
			    Count.Unlimited);
	  in Trace.trace_print (fn _ => "ICMP listen started");
	     if not gateway then stop
	     else
	      let val forward_pattern =
                       Network_Pattern.Partial {proto = forward_protocol}
(* it doesn't much matter what handler we give for this connection;
   the connection handler will be called, but the data and status handlers
   never will. *)
		  val Connection.L {stop = forward_stop, ...} =
		       listen (forward_pattern,
			       Connection.H (listen_handler setup),
			       Count.Unlimited);
		  fun stop_all () = (stop (); forward_stop ())
	      in stop_all
	      end
	  end

     fun find_next [] = NONE
       | find_next (first :: rest) =
          case ((first ()) handle _ => NONE) of
	     NONE => find_next rest
	   | SOME stop_fun => SOME (stop_fun, rest)

     fun icmp_register_listen (setup, listen) =
           case ! listen_state of
	     Not_Listening =>
	       (listen_state := Listening {stop = do_listen (setup,
							     listen),
					   count = ref 1};
		Trace.trace_print
		(fn _ => "starting listen for ICMP session"))
	   | Listening {stop, count} =>
	       (count := ! count + 1;
		Trace.trace_print (fn _ =>
				   "adding listen for ICMP session"))

     fun icmp_deregister_listen valid =
           (case ! listen_state of
	      Not_Listening =>
	       Trace.local_print ("fin_icmp called, but no session")
	    | Listening {stop, count} =>
	       (Trace.trace_print (fn _ => "stopping ICMP session");
		if ! count > 1 then
		 (Trace.trace_print (fn _ => "count = " ^
				     Int.toString (! count) ^ ", continuing");
		  count := ! count - 1)
		else
		 (Trace.trace_print (fn _ => "no more ICMP sessions");
		  stop ())))

     val icmp_listen_sem = B.Semaphore.new ()
    in
     fun init_icmp arg =
          B.Semaphore.with_lock (icmp_listen_sem, icmp_register_listen, arg)

     fun fin_icmp arg =
          B.Semaphore.with_lock (icmp_listen_sem, icmp_deregister_listen, arg)

     fun icmp_listen _ =
          (Trace.local_print "ICMP listen is not implemented";
	   not_supp ())

     fun session_wrap (setup, handler)
                      (Connection.S {connect, listen, extension}) =
	  let val listen_id = init_icmp (setup, listen);
	  in (handler (S {connect = connect_wrap (setup, connect),
			  listen = icmp_listen, extension = ()})
	      before fin_icmp listen_id)
	     handle x => (fin_icmp listen_id; raise x)
	  end

     fun session (setup, handler) =
          (Trace.trace_constant_string "starting ICMP/IP session";
	   Connection.session (setup, session_wrap (setup, handler))
	   before Trace.trace_constant_string "done with ICMP/IP session")
    end

   end (* struct ICMP *)

(*
	44.	joint function session
*)

  local
   fun execute_session (setup, f) (args as Connection.S {listen, ...}) =
        let val id = Icmp.init_icmp (setup, listen);
	    datatype 'a result = Result of 'a | Exception of exn
	    val result = ((Result (f args))
			  handle x => Exception x)
	in Icmp.fin_icmp id;
	   case result of
	      Result a => a
	    | Exception x => raise x
	end
	 
  in
   fun session (setup, handler) =
        (Trace.trace_constant_string "starting IP session";
	 Connection.session (setup, execute_session (setup, handler))
	 before Trace.trace_constant_string "done with IP session")
  end

 end (* struct *)
