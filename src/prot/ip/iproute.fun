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

	A functor to manage routing information for IP.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Ip_Route
	2.	internal structure Default_Gateway
	3.	internal functions address_*
	4.	internal function get_interface
	5.	function address_for_interface
	6.	internal function entry_for_address
	7.	function interface_for_address
	8.	internal function build_entry
	9.	function new
	10.	function set_interface_address
	11.	function disable_interface
	12.	function set_interface_mask
	13.	function unset_interface_mask
	14.	function add_default_gateway
	15.	function add_preference_gateway
	16.	function add_specific_gateway
	17.	function remove_default_gateway
	18.	function remove_specific_gateway
	19.	function has_default_gateway
	20.	function resolve
	21.	function valid_incoming
	22.	function is_unicast_address
	23.	function gc

		iii.	RCS Log
	
$Log: iproute.fun,v $
Revision 1.21  1997/01/24  14:52:21  cline
eliminated (illegal) signature declaration within functor body.

Revision 1.20  1996/04/18  21:20:47  cline
converted hash from int to word

Revision 1.19  1996/02/29  17:34:18  esb
major rewrite (code was buggy)

Revision 1.18  1996/02/23  21:13:31  esb
added makestring and has_default_gateway.

Revision 1.17  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.16  1995/09/26  16:25:04  esb
added function to support ICMP router info message.

Revision 1.15  1995/08/29  14:14:54  esb
version that includes IP and ICMP.

Revision 1.14  1995/08/24  00:51:04  esb
added some support for IP.

Revision 1.13  1995/06/23  19:55:30  esb
first version of IP that compiles with the new PROTOCOL signature.

Revision 1.12  1995/06/20  17:02:17  esb
adapted to new protocol signature.

Revision 1.11  1995/03/12  17:50:04  esb
adapted to new trace.sig.

Revision 1.10  1995/03/07  20:36:12  esb
updated large numbers.

Revision 1.9  1995/02/04  20:40:05  robby
updated to 107

Revision 1.8  1994/11/11  18:10:29  esb
changed the functor parameters for Debug_Trace_Printer.

Revision 1.7  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.6  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.5  1994/08/28  21:41:13  milnes
Added a print.

Revision 1.4  1994/08/02  20:32:38  esb
adapted to new protocol signature.

Revision 1.3  1994/07/25  17:27:56  milnes
Added do_prints debugging prints.

Revision 1.2  1994/06/29  19:42:38  milnes
*** empty log message ***

Revision 1.1  1994/06/15  20:46:08  milnes
Initial revision

	1.	functor Ip_Route
*)

functor Ip_Route (val makestring_ip: Word32.word -> string
		  structure B: FOX_BASIS
		  val debug_level: int ref option): IP_ROUTE =
 struct

  type ip_number = Word32.word
  type interface = string
  type preference = Word32.word

  datatype resolution = Unicast of {next_hop: ip_number}
                      | Broadcast
                      | Loopback

(*
	2.	internal structure Default_Gateway
*)

  structure Default_Gateway:
   sig
    type T
    val init: ip_number list -> T
    val add: T * {gateway: ip_number, preference: preference, ttl: int} -> T
    val default: T -> ip_number option
    val has_default: T -> bool
    val gc: T -> T
(* check_network only keeps those gateways that, anded with the mask,
   match the network number and removes the rest. *)
    val check_network: T * {network: ip_number, mask: ip_number} -> T
    val makestring: T -> string
   end
  =
   struct
(* gateways are stored in order of non-increasing preference. *)
    datatype gateway = Static of ip_number
                     | Dynamic of ip_number * preference * int (* ttl *)
    type T = gateway list

    fun init [] = []
      | init (first :: rest) = Static first :: init rest

(* preferences are signed 32-bit numbers. We use an equivalent preference
   of zero (actually, ~0.5) for statically-configured information. *)
    val min_pref = Word32.<< (0w1, 0w31)
    fun pref_less_than_zero pref = Word32.>= (pref, min_pref)
    fun pref_less (a, b) =
         Word32.< (Word32.- (a, min_pref), Word32.- (b, min_pref))

    exception Lower_Priority_Remove
    fun remove_dynamic ([], _, _, _) = []
      | remove_dynamic ((first as (Static _)) :: rest,
			gateway, preference, ttl) =
         first :: remove_dynamic (rest, gateway, preference, ttl)
      | remove_dynamic ((first as (Dynamic (g, p, t))) :: rest,
			gateway, preference, ttl) =
         if g = gateway then
	  if pref_less (p, preference) orelse
	     (p = preference andalso t < ttl) then rest
	  else raise Lower_Priority_Remove
	 else first :: remove_dynamic (rest, gateway, preference, ttl)

    fun add_dynamic ([], (gateway, preference, ttl)) =
         [Dynamic (gateway, preference, ttl)]
      | add_dynamic ((first as (Static _)) :: rest,
		     new as (gateway, preference, ttl)) =
	 if pref_less_than_zero preference then
	  first :: add_dynamic (rest, new)
	 else Dynamic (gateway, preference, ttl) :: first :: rest
      | add_dynamic ((first as (Dynamic (g, p, t))) :: rest,
		     new as (gateway, preference, ttl)) =
	 if pref_less (preference, p) then
	  Dynamic (gateway, preference, ttl) :: first :: rest
	 else first :: add_dynamic (rest, new)

    fun add (list, {gateway, preference, ttl}) =
         ((add_dynamic (remove_dynamic (list, gateway, preference, ttl),
			(gateway, preference, ttl)))
	  handle Lower_Priority_Remove => list)

    fun default [] = NONE
      | default (Static g :: _) = SOME g
      | default (Dynamic (g, _, _) :: _) = SOME g

    fun has_default [] = false 
      | has_default _ = true

    fun gc [] = []
      | gc (Dynamic (g, p, t) :: rest) =
         if t > 0 then Dynamic (g, p, t - 1) :: gc rest
	 else gc rest
      | gc (first :: rest) = first :: gc rest (* static *)

    fun check_network ([], _) = []
      | check_network (Static g :: rest, reference as {network, mask}) =
         if Word32.andb (g, mask) = network then
	  Static g :: check_network (rest, reference)
	 else check_network (rest, reference)
      | check_network ((first as (Dynamic (g, p, t))) :: rest,
		       reference as {network, mask}) =
         if Word32.andb (g, mask) = network then
	  first :: check_network (rest, reference)
	 else check_network (rest, reference)

    fun makestring [] = ""
      | makestring [Static g] = makestring_ip g
      | makestring [Dynamic (g, pref, ttl)] =
         makestring_ip g ^ "/" ^ Word32.toString pref ^ "/" ^
	 Integer.toString ttl
      | makestring (first :: rest) =
	 makestring [first] ^ "+" ^ makestring rest

   end

  (* all information except for per-host-specific-gateways is stored
     as interface_info; the entire list of interface_infos is searched
     on every resolution of an IP address. *)
  type interface_info = {local_ip: ip_number, network_number: ip_number,
			 mask: ip_number, mask_set: bool,
			 gateways: Default_Gateway.T}
  type T = (interface, interface_info) B.Store.T *
           ( (* peer *) ip_number,
	    interface * (* gateway *) ip_number) B.Store.T *
           (interface * ip_number) option (* global default gateway, if any *)

  local
   fun list (makestring, value) =
        let fun loop [] = ""
	      | loop [last] = makestring last
	      | loop (first :: rest) =
	         makestring first ^ ", " ^ loop rest
	in "[" ^ loop value ^ "]"
	end

   fun special_gateway (peer, (interface, gateway)) =
        interface ^ "/" ^ makestring_ip gateway ^ "=>" ^ makestring_ip peer

   fun interface_info (interface,
		       {local_ip, network_number, mask, mask_set, gateways}) =
        interface ^ "/" ^ makestring_ip local_ip ^ ": " ^
	"mask " ^ makestring_ip mask ^
	" (" ^ (if mask_set then "" else "not ") ^ "set), " ^
	"network " ^ makestring_ip network_number ^ ", " ^
	"gateways = " ^ Default_Gateway.makestring gateways

  in
   fun makestring ((interfaces, special, default): T) =
        "interfaces = " ^
	B.Store.makestring (interfaces, interface_info, ", ") ^ "; " ^
        "special gateways = " ^
	B.Store.makestring (special, special_gateway, "+") ^ ", " ^
        "default gateway = " ^
	(case default of
	    NONE => "none"
	  | SOME (i, g) => i ^ "/" ^ makestring_ip g)
  end

  exception Unknown_Class of ip_number
  exception Gateway_Not_Connected of ip_number

  fun makestring_exn (Unknown_Class ip) =
       SOME ("unknown class for " ^ makestring_ip ip)
    | makestring_exn (Gateway_Not_Connected ip) =
       SOME ("gateway " ^ makestring_ip ip ^
	     " not found on directly-connected network(s)")
    | makestring_exn _ = NONE

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "iproute.fun"
			   val makestring = makestring_exn)

  val zero32 = Word32.fromInt 0
  val ones32 = Word32.notb zero32
  val loopback_mask = Word32.<< (0wx7f, 0w24)

(*
		3.	internal functions address_*

	These functions test and mask the ip_address as per RFC 796 on
	address mapping.
*)


  datatype Ip_Address_Class = Class_A | Class_B | Class_C

  val class_a_id = (* 0wx80000000 : Word32.word *)
        Word32.* (Word32.fromInt 0x800000, Word32.fromInt 0x100)
  val class_b_id = (* 0wxC0000000 : Word32.word *)
        Word32.* (Word32.fromInt 0xC00000, Word32.fromInt 0x100)
  val class_c_id = (* 0wxE0000000 : Word32.word *)
        Word32.* (Word32.fromInt 0xE00000, Word32.fromInt 0x100)

  val class_a_mask = (* 0wxFF000000 : Word32.word *)
        Word32.* (Word32.fromInt 0xFF0000, Word32.fromInt 0x100)
  val class_b_mask = (* 0wxFFFF0000 : Word32.word *)
        Word32.* (Word32.fromInt 0xFFFF00, Word32.fromInt 0x100)
  val class_c_mask = (* 0wxFFFFFF00 : Word32.word *)
        Word32.* (Word32.fromInt 0xFFFFFF, Word32.fromInt 0x100)

  fun address_class ip_number =
       if Word32.andb (ip_number, class_a_id) = zero32 then Class_A
       else if Word32.andb (ip_number, class_b_id) = class_a_id then
	Class_B
       else if Word32.andb (ip_number, class_c_id) = class_b_id then
	Class_C
       else
	Trace.print_raise (Unknown_Class ip_number, SOME "address_class")

  fun address_network_mask ip_number =
       case address_class ip_number of
	  Class_A => class_a_mask
	| Class_B => class_b_mask
	| Class_C => class_c_mask

(*
	4.	internal function get_interface
*)

  fun get_interface (interfaces, interface: interface) =
       case B.Store.look (interfaces, interface) of
	  NONE => NONE
	| SOME (_, value: interface_info) => SOME value

(*
	5.	function address_for_interface
*)

  fun address_for_interface ((interfaces, _, _), interface) =
       case get_interface (interfaces, interface) of
	  NONE => NONE
	| SOME {local_ip, ...} => SOME local_ip

(*
	6.	internal function entry_for_address
*)

  fun entry_for_address ((interfaces, _, _), address) =
       let fun find_interface (_, SOME x) = SOME x
	     | find_interface ((interface,
				(info as {mask, network_number, ...})), NONE) =
	        if Word32.andb (address, mask) = network_number then
		 SOME (interface, info: interface_info)
		else NONE
       in B.Store.fold find_interface interfaces NONE
       end

(*
	7.	function interface_for_address
*)

  fun interface_for_address (state, address) =
       case entry_for_address (state, address) of
	  NONE => NONE
	| SOME (interface, _) => SOME interface

(*
	8.	internal function build_entry
*)

  fun build_entry (address, mask, mask_set, old_gateways) =
       let val mask_value = case mask of
	                       SOME m => m
			     | NONE => address_network_mask address
	   val network = Word32.andb (mask_value, address)
	   val new_gateways =
	        Default_Gateway.check_network (old_gateways,
					       {network = network,
						mask = mask_value})
	in {local_ip = address, network_number = network, mask = mask_value,
	    mask_set = mask_set, gateways = new_gateways}
	end

(*
	9.	function new
*)

  local
   fun same_interface (a: interface, b) = a = b
   fun hash_interface (a: interface) =
        B.V.List.fold (op+ ) (B.V.List.map
			      (Word.fromInt o B.V.Char.ord) (explode a)) 0w0

   fun same_peer (p1: ip_number, p2) = p1 = p2
   fun hash_peer peer = Word.fromLargeWord peer

  in
   fun new [] = (B.Store.new (hash_interface, same_interface),
		 B.Store.new (hash_peer, same_peer), NONE)
     | new ({interface, address, gateways, mask} :: rest) =
        let val (interfaces, special_gateways, current) = new rest
	    val entry = build_entry (address, mask, false,
				     Default_Gateway.init gateways)
	    val info = B.Store.add (interfaces, interface, entry)
        in (info, special_gateways, current)
        end
  end

(*
	10.	function set_interface_address
*)

  fun set_interface_address ((interfaces, special_gateways, default),
			     interface, address) =
       ((case B.Store.look (interfaces, interface) of
	    SOME (new_interfaces,
		  {local_ip, network_number, mask, mask_set, gateways}) =>
	     B.Store.add (new_interfaces, interface,
			  build_entry (address, NONE, false, gateways))
	  | NONE =>
	     B.Store.add (interfaces, interface,
			  build_entry (address, NONE, false,
				       Default_Gateway.init []))),
	  special_gateways, default)

(*
	11.	function disable_interface
*)

  fun disable_interface ((interfaces, special_gateways, default), interface) =
       (B.Store.remove (interfaces, interface), special_gateways, default)

(*
	12.	function set_interface_mask

	See RFC 1122, pp. 46-47: Mask should not be all ones, and
        should be either zero, or the eight highest bits should be
        one.
*)

  local
   val all_ones_mask = Word32.- (0w0, 0w1)
   val all_zero_mask = Word32.fromInt 0
   val eight_high_bits = Word32.<< (0wxff, 0w24)

   fun valid_mask new_mask =
        new_mask <> all_ones_mask andalso
	(new_mask = all_zero_mask orelse
	 Word32.andb (new_mask, eight_high_bits) = eight_high_bits)

  in
   fun set_interface_mask ((interfaces, special_gateways, default),
			   interface, new_mask) =
        ((if valid_mask new_mask then
           case B.Store.look (interfaces, interface) of
	      NONE => interfaces
	    | SOME (new_interfaces, {local_ip, network_number, mask,
				     mask_set, gateways}) =>
	       if mask_set then new_interfaces (* RFC 1122, page 45 *)
	       else
	        B.Store.add (new_interfaces, interface,
			     build_entry (local_ip, SOME new_mask, true,
					  gateways))
	  else interfaces), special_gateways, default)
  end

(*
	13.	function unset_interface_mask
*)

  fun unset_interface_mask ((interfaces, special_gateways, default),
			    interface) =
       ((case B.Store.look (interfaces, interface) of
	    NONE => interfaces
	  | SOME (new_interfaces, {local_ip, network_number, mask,
				   mask_set, gateways}) =>
	     B.Store.add (new_interfaces, interface,
			  build_entry (local_ip, NONE, false, gateways))),
	  special_gateways, default)

(*
	14.	function add_default_gateway
*)

  fun add_default_gateway ((state as (interfaces, special_gateways, default)),
			   gateway) =
       (interfaces, special_gateways,
	case entry_for_address (state, gateway) of
	   NONE => default
	 | SOME (interface, _) =>
	    SOME (interface, gateway))

(*
	15.	function add_preference_gateway
*)

  fun add_preference_gateway ((s as (interfaces, special_gateways, default)),
			       gateway, preference, ttl) =
        ((case entry_for_address (s, gateway) of
	     NONE => interfaces
	   | SOME (interface, 
		   {local_ip, network_number, mask,
		    mask_set, gateways}) =>
	      B.Store.add (interfaces, interface,
			   build_entry
			   (local_ip, SOME mask, mask_set,
			    Default_Gateway.add
			    (gateways, {gateway = gateway,
					preference = preference,
					ttl = ttl})))),
	 special_gateways, default)

(*
	16.	function add_specific_gateway
*)

  fun add_specific_gateway ((state as (interfaces, special_gateways, default)),
			    {destination, gateway}) =
        (interfaces,
	 case entry_for_address (state, gateway) of
	    NONE => special_gateways
	  | SOME (interface, _) =>
	     B.Store.add (special_gateways, destination, (interface, gateway)),
	 default)

(*
	17.	function remove_default_gateway
*)

  fun remove_default_gateway (state as (interfaces, gateways, NONE), gateway) =
       state
    | remove_default_gateway (state as (interfaces, gateways,
					SOME (interface, default)), gateway) =
       if gateway = default then (interfaces, gateways, NONE) else state

(*
	18.	function remove_specific_gateway
*)

  fun remove_specific_gateway ((interfaces, gateways, default),
			       {destination}) =
       (interfaces, B.Store.remove (gateways, destination), default)

(*
	19.	function has_default_gateway
*)

  fun has_default_gateway ((interfaces, _, _), interface) =
       case B.Store.look (interfaces, interface) of
	  NONE => false
	| SOME (_, {gateways, ...}: interface_info) =>
	   Default_Gateway.has_default gateways

(*
	20.	function resolve

	Broadcast addresses are described in RFC 1122, p. 30.
	The algorithm below is documented in RFC 1122, pp. 47 and 48.
*)

  local
   fun resolve_broadcast ip_number (_, SOME result) = SOME result
     | resolve_broadcast ip_number ((interface,
				     {local_ip, network_number,
				      mask, mask_set, gateways}), NONE) =
        if ip_number = ones32 orelse ip_number = zero32 then
	 SOME {interface = interface, interface_ip = local_ip,
	       next_hop = Broadcast}
        else if Word32.andb (loopback_mask, ip_number) = loopback_mask then
	 SOME {interface = interface, interface_ip = local_ip,
	       next_hop = Loopback}
        else if Word32.andb (mask, ip_number) = network_number then
	 let val host_mask = Word32.notb mask
	     val host_number = Word32.andb (host_mask, ip_number)
	 in if host_number = host_mask orelse host_number = zero32 then
	     SOME {interface = interface, interface_ip = local_ip,
		   next_hop = Broadcast}
	    else NONE
	 end
        else NONE

   fun find_gateway (_, SOME result) = SOME result
     | find_gateway ((interface, {local_ip, network_number,
				  mask, mask_set, gateways}), NONE) =
        case Default_Gateway.default gateways of
	   NONE => NONE
	 | SOME gateway =>
	    SOME {interface = interface, interface_ip = local_ip,
		  next_hop = Unicast {next_hop = gateway}}

   fun get_ip (state, interface, next) =
        case address_for_interface (state, interface) of
	   NONE =>
	    (Trace.local_print ("no address for interface " ^ interface);
	     NONE)
	 | SOME ip =>
	    SOME {interface = interface, interface_ip = ip, next_hop = next}

  in
   fun resolve (state as (interfaces, special_gateways, default), ip_number) =
        case entry_for_address (state, ip_number) of
	   SOME (interface, {local_ip, ...} ) =>
	    SOME {interface = interface, interface_ip = local_ip,
		  next_hop = Unicast {next_hop = ip_number}}
	 | NONE =>
	    case B.Store.look (special_gateways, ip_number) of
	       SOME (gateways, (interface, gateway)) =>
		get_ip (state, interface, Unicast {next_hop = ip_number})
	     | NONE =>
	        case B.Store.fold (resolve_broadcast ip_number)
		     interfaces NONE of
		   SOME x => SOME x
		 | NONE =>
		    case default of
		       SOME (interface, gateway) =>
			get_ip (state, interface, Unicast {next_hop = gateway})
		     | NONE =>
			B.Store.fold find_gateway interfaces NONE

(*
	21.	function valid_incoming
*)

   fun valid_incoming ((interfaces, gateways, default),
		       interface, destination_ip) =
        case B.Store.look (interfaces, interface) of
	   NONE => false
	 | SOME (_, interface_desc as {local_ip, ...}) =>
	    local_ip = destination_ip orelse
	    (case resolve_broadcast destination_ip
	                            ((interface, interface_desc), NONE) of
	        NONE => false
	      | SOME _ => true)

(*
	22.	function is_unicast_address

	Anything to or from such an address should not get a reply.
*)

   fun is_unicast_address ((interfaces, gateways, default), ip) =
        case B.Store.fold (resolve_broadcast ip) interfaces NONE of
	   SOME {interface, interface_ip, next_hop} =>
	    (case next_hop of
	        Broadcast => false
	      | _ => true)
	 | NONE => true
  end

(*
	23.	function gc
*)

  fun gc (interfaces, gateways, default) =
       let fun gc_interface (_, {local_ip, network_number, mask, mask_set,
				 gateways}) =
	        {local_ip = local_ip, network_number = network_number,
		 mask = mask, mask_set = mask_set,
		 gateways = Default_Gateway.gc gateways}
       in (B.Store.map gc_interface interfaces, gateways, default)
       end

	
 end (* struct *)
