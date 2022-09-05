(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	ipmux.fun: A multiplexer for IP.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Ip_Mux1
	2.	IP number type and operations
	3.	internal state
	4.	structure Setup
	5.	structure Address
	6.	structure Mux_Connection_Key
	7.	structure Pattern
	8.	other structures
	9.	other exported types
	10.	function init
	11.	function finalize
	12.	function interfaces
	13.	function set_interface
	14.	function session
	15.	functor Ip_Mux2
	16.	IP number type and operations
	17.	structure Setup
	18.	structure Address
	19.	structure Pattern
	20.	interface names
	21.	structure Mux_Connection_Key
	22.	other structures
	23.	other exported types
	24.	internal state
	25.	session


	iii.	RCS Log

$Log: ipmux.fun,v $
Revision 1.21  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.20  97/01/24  14:49:27  cline
eliminated (illegal) signature declaration within functor body.

Revision 1.19  1996/07/22  20:08:49  cline
*** empty log message ***

Revision 1.18  1996/06/07  20:18:34  cline
fixed usage of before

Revision 1.17  1996/05/14  01:19:28  esb
ipmux2 no longer needs interface name as functor parameter.

Revision 1.16  1996/04/30  20:24:55  esb
minor change.

Revision 1.15  1996/04/18  21:20:28  cline
converted hash from into to word

Revision 1.14  1996/03/12  22:24:15  esb
minor cleanup.

Revision 1.13  1996/02/23  21:15:38  esb
the interface name is now part of the connection key, not the packet.

Revision 1.12  1996/01/19  23:02:29  esb
adapted to the new wordarray signature.

Revision 1.11  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.10  1995/11/12  16:31:57  esb
added back Ip_Mux2; also made lower initialization come from setup.

Revision 1.9  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.8  1995/10/02  21:22:00  esb
made connection_key into abstract type.

Revision 1.7  1995/09/14  21:09:31  cline
work around for representation bug

Revision 1.6  1995/09/13  15:30:14  esb
added handling Already_Open from Arp.

Revision 1.5  1995/08/29  14:14:54  esb
version that includes IP and ICMP.

Revision 1.4  1995/06/29  18:19:56  esb
adapted to new wordarray.

Revision 1.3  1995/06/26  17:30:33  esb
worked around a compiler bug.

Revision 1.2  1995/06/23  19:55:30  esb
first version of IP that compiles with the new PROTOCOL signature.

Revision 1.1  1995/06/20  17:05:46  esb
Initial revision

*)

(*
	1.	functor Ip_Mux1

	Interface is the hardware name of the interface, e.g. "SE0".
	Case is not significant.  This multiplexer only "multiplexes"
	one interface.
*)

functor Ip_Mux1 (structure Arp: ADDRESS_RESOLUTION_PROTOCOL
		   where type Host.T = Word_Array.T
		     and type Setup.T = string
		 val ip_protocol_number: Arp.Protocol.T
		 structure B: FOX_BASIS): IP_MULTIPLEXER =
 struct

(*
	2.	IP number type and operations
*)

  type ip_number = Word32.word

  val ip_hash_mask = Word32.fromInt (*0w*)0xffffff : Word32.word
  val ip_byte_mask = Word32.fromInt (*0w*)0xff : Word32.word
  fun makestring_ip ip =
	(Int.toString (Word32.toInt (Word32.>> (ip, 0w24))) ^ "." ^
	 Int.toString (Word32.toInt
		       (Word32.andb (Word32.>> (ip, 0w16),
				     ip_byte_mask))) ^ "." ^
	 Int.toString (Word32.toInt
		       (Word32.andb (Word32.>> (ip, 0w8),
				     ip_byte_mask))) ^ "." ^
	 Int.toString (Word32.toInt (Word32.andb (ip, ip_byte_mask))))

  fun hash_ip ip =
        (Word31.fromLargeWord (Word32.>> (ip, 0w24) +
			       Word32.andb (ip, ip_hash_mask)))

  fun equal_ip (a: ip_number, b) = a = b

(*
	3.	internal state
*)

  val if_state = ref (("", NONE, []):
		      string * ip_number option * unit ref list)

  exception Interface_With_Multiple_Addresses
  exception No_Such_Interface

  fun remove_list (el, []) = []
    | remove_list (el, head :: tail) =
       if el = head then tail else head :: remove_list (el, tail)

(*
	4.	structure Setup
*)

  structure Setup: NETWORK_SETUP =
   struct
    type host_id =ip_number
    datatype setup = Setup of {local_id: host_id, interface: string,
			       gateways: host_id list,
			       mask: (host_id * {serve: bool}) option,
			       mtu: int option} list
    type T = setup
    fun makestring_gateways [] = ""
      | makestring_gateways [last] = "g" ^ makestring_ip last
      | makestring_gateways (first :: rest) =
	 "g" ^ makestring_ip first ^ ", " ^ makestring_gateways rest
    fun makestring (Setup []) = ""
      | makestring (Setup
		    ({local_id, interface, gateways, mask, mtu} :: rest)) =
         interface ^ " (" ^ makestring_ip local_id ^ ")" ^
	 makestring_gateways gateways ^
	 (case mask of
	     NONE => ""
	   | SOME (m, {serve}) =>
	      (", mask " ^ makestring_ip m ^ " (" ^
	       (if serve then "" else "do not ") ^ "serve)")) ^
	 (case mtu of
	     NONE => ""
	   | SOME m =>
	      ", mtu " ^ Integer.toString m) ^ makestring (Setup rest)
    fun equal (a: setup, b) = a = b
    fun hash (Setup []) = 0w0
      | hash (Setup ({local_id, interface, gateways, mask, mtu} :: rest)) =
	 hash_ip local_id + hash (Setup rest)
   end
  structure Mux_Setup = Setup

(*
	5.	structure Address
*)

  structure Address: IPMUX_ADDRESS =
   struct
    type ip_number = ip_number
    datatype address = Unicast of {interface: string, peer: ip_number}
                     | Broadcast of string
    type T = address
    fun equal (a: T, b) = a = b
    fun hash (Unicast {interface, peer}) = hash_ip peer
      | hash (Broadcast _) = 0w1
    fun makestring (Unicast {interface, peer}) =
         makestring_ip peer ^ " (" ^ interface ^ ")"
      | makestring (Broadcast interface) =
         "broadcast (" ^ interface ^ ")"
   end
  structure Mux_Address = Address

(*
	6.	structure Mux_Connection_Key
*)

  structure Mux_Connection_Key =
   struct
    open Arp.Connection_Key
    fun interface _ = 
         let val (name, _, _) = ! if_state in name end
   end
  structure Connection_Key = Mux_Connection_Key

(*
	7.	structure Pattern
*)

  structure Pattern: IPMUX_PATTERN =
   struct
    type ip_number = ip_number
    datatype pattern = Unicast of string
                     | Broadcast of string
                     | All
    type T = pattern
    fun equal (a: T, b) = a = b
    fun hash (Unicast interface) =
         Word.fromInt (B.V.Char.ord (B.V.String.ordof (interface, 0)))
      | hash (Broadcast interface) = hash (Unicast interface)
      | hash All = 0w0
    fun makestring (Unicast interface) = "unicast " ^ interface
      | makestring (Broadcast interface) = "broadcast " ^ interface
      | makestring All = "all interfaces "
   end
  structure Mux_Pattern = Pattern

(*
	8.	other structures
*)

  structure Incoming = Arp.Incoming
  structure Outgoing = Arp.Outgoing
  structure Count = Arp.Count
  structure Status = Arp.Status
  structure X = Arp.X
  exception Already_Open of Connection_Key.T

(*
	9.	other exported types
*)

  datatype interface = Enabled of string * ip_number
                     | Disabled of string

  datatype mux_session_extension =
      Mux_Session_Extension of
        {interfaces: unit -> interface list,
	 set_interface: interface -> unit,
	 maximum_packet_size: string -> Word.word,
	 minimum_packet_size: string -> Word.word}
  type session_extension = mux_session_extension
  type listen_extension = Arp.listen_extension 
  type connection_extension = Arp.connection_extension

  local
   structure Export:
    sig
     datatype connection = C of {send: Outgoing.T -> unit,
				 abort: unit -> unit,
				 extension: connection_extension}
     datatype listen = L of {stop: unit -> unit, extension: listen_extension}
    end
   = Arp
  in
   open Export
  end

  datatype handler = H of Connection_Key.T
                   -> {connection_handler: connection -> unit,
		       data_handler: connection * Incoming.T -> unit,
		       status_handler: connection * Status.T -> unit}

  datatype session = S of {connect: Address.T * handler -> unit,
			   listen: Pattern.T * handler * Count.T -> listen,
			   extension: session_extension}

(*
	10.	function init
*)

   fun init (_, []) = raise No_Such_Interface
     | init (session, {local_id, interface, gateways, mask, mtu} :: []) =
        let val (name, ip, list) = ! if_state
	in if name = "" orelse B.V.String.caseless_equal (interface, name) then
	    (case ip of
	        NONE => ()
	      | SOME ip =>
		 if ip <> local_id then raise Interface_With_Multiple_Addresses
		 else ();
	     if_state := (interface, SOME local_id, session :: list);
	     (local_id, interface, mtu))
	   else
	    raise No_Such_Interface
	end
     | init _ = raise No_Such_Interface

(*
	11.	function finalize
*)

   fun finalize session =
        case ! if_state of
	   (name, NONE, list) =>
	    if_state := (name, NONE, remove_list (session, list))
	 | (name, SOME ip, list) =>
	    case remove_list (session, list) of
	       [] => if_state := (name, NONE, [])
	     | new_list => if_state := (name, SOME ip, new_list)

(*
	12.	function interfaces
*)

   fun interfaces () =
        case ! if_state of
	   (name, NONE, _) => [Disabled name]
	 | (name, SOME ip, _) => [Enabled (name, ip)]

(*
	13.	function set_interface
*)

   fun set_interface session (Enabled (interface, new_ip)) =
        let val (name, ip, list) = ! if_state
	in if name = "" orelse B.V.String.caseless_equal (interface, name) then
	    (case ip of
	        NONE => ()
	      | SOME ip =>
		 if ip <> new_ip then raise Interface_With_Multiple_Addresses
		 else ();
	     if_state := (interface, SOME new_ip, session :: list))
	   else
	    raise No_Such_Interface
	end
     | set_interface session (Disabled interface) =
        let val (name, ip, list) = ! if_state
	in if name = "" orelse B.V.String.caseless_equal (interface, name) then
	    if_state := (interface, NONE, list)
	   else ()
	end

(*
	14.	function session
*)

  fun from_lower_address address =
       Word_Array.W32.U_Big.F.head (Word_Array.to32 address)

  fun to_lower_address ip =
       Word_Array.from32 (Word_Array.W32.U_Big.F.create (ip, 0w1))

  fun session (Setup.Setup setup, session_function) =
       let val id = ref ()
	   val (higher_self, interface, mtu) = init (id, setup)
	   val self = to_lower_address higher_self
	   fun call_session (Arp.S {connect, listen,
				    extension = {maximum_packet_size,
						 minimum_packet_size}}) =
	        let fun new_c (Address.Unicast {interface, peer}, H handler) =
		         ((connect (Arp.Arp_Address.Specific
				    {self = self, peer = to_lower_address peer,
				     protocol = ip_protocol_number},
				    Arp.H handler))
			  handle Arp.Already_Open x => raise Already_Open x)
		      | new_c (Address.Broadcast interface, H handler) =
		         ((connect (Arp.Arp_Address.Broadcast
				    ip_protocol_number, Arp.H handler))
			  handle Arp.Already_Open x => raise Already_Open x)
		    fun new_l (Pattern.Unicast interface, H handler, count) =
		         listen (Arp.Arp_Pattern.Specific
				  {self = self, protocol = ip_protocol_number},
				 Arp.H handler, count)
		      | new_l (Pattern.Broadcast interface, H handler, count) =
		         listen (Arp.Arp_Pattern.Broadcast ip_protocol_number,
				 Arp.H handler, count)
		      | new_l (Pattern.All, H handler, count) =
		         listen (Arp.Arp_Pattern.Specific
				  {self = self, protocol = ip_protocol_number},
				 Arp.H handler, count)
		    val real_max = case mtu of
		                      NONE => maximum_packet_size
				    | SOME m => Word.fromInt m
		    fun min interface = minimum_packet_size
		    fun max interface = real_max
		    val new_e = Mux_Session_Extension
				{interfaces = interfaces,
				 set_interface = set_interface id,
				 maximum_packet_size = max,
				 minimum_packet_size = min}
		in session_function (S {connect = new_c, listen = new_l,
					extension = new_e})
		end
       in (Arp.session (interface, call_session)
	   handle x => (finalize id; raise x))
	  before finalize id
       end

 end (* struct *)

(*
	15.	functor Ip_Mux2

	Interface1/2 is the hardware name of the interface, e.g. "SE0".
	Case is not significant.  This multiplexer only multiplexes
	two interfaces.
*)

functor Ip_Mux2 (structure Arp1: ADDRESS_RESOLUTION_PROTOCOL
		   where type Host.T = Word_Array.T
		     and type Setup.T = string
		 structure Arp2: ADDRESS_RESOLUTION_PROTOCOL
		   where type Host.T = Word_Array.T
		     and type Setup.T = string
		   sharing type Arp1.Incoming.T = Arp2.Incoming.T
		       and type Arp1.Outgoing.T = Arp2.Outgoing.T
		       and type Arp1.Count.T = Arp2.Count.T
		 val is_interface1: string -> bool
		 val is_interface2: string -> bool
		 val ip_protocol_number1: Arp1.Arp_Address.protocol
		 val ip_protocol_number2: Arp2.Arp_Address.protocol
		 val debug_level: int ref option
		 structure B: FOX_BASIS): IP_MULTIPLEXER =
 struct

(*
	16.	IP number type and operations
*)

  type ip_number = Word32.word

  val ip_hash_mask = Word32.fromInt (*0w*)0xffffff : Word32.word
  val ip_byte_mask = Word32.fromInt (*0w*)0xff : Word32.word
  fun makestring_ip ip =
       (Int.toString (Word32.toInt (Word32.>> (ip, 0w24))) ^ "." ^
	Int.toString (Word32.toInt
		      (Word32.andb (Word32.>> (ip, 0w16),
				    ip_byte_mask))) ^ "." ^
	Int.toString (Word32.toInt
		      (Word32.andb (Word32.>> (ip, 0w8),
				    ip_byte_mask))) ^ "." ^
	Int.toString (Word32.toInt (Word32.andb (ip, ip_byte_mask))))

  fun hash_ip ip =
       (Word.fromLargeWord (Word32.>> (ip, 0w24) +
			    Word32.andb (ip, ip_hash_mask)))

  fun equal_ip (a: ip_number, b) = a = b

(*
	17.	structure Setup
*)

  structure Setup: NETWORK_SETUP =
   struct
    type host_id = ip_number
    datatype setup = Setup of {local_id: host_id, interface: string,
			       gateways: host_id list,
			       mask: (host_id * {serve: bool}) option,
			       mtu: int option} list
    type T = setup
    fun makestring_gateways [] = ""
      | makestring_gateways [last] = "g" ^ makestring_ip last
      | makestring_gateways (first :: rest) =
	 "g" ^ makestring_ip first ^ ", " ^ makestring_gateways rest
    fun makestring (Setup []) = ""
      | makestring (Setup
		    ({local_id, interface, gateways, mask, mtu} :: rest)) =
         interface ^ " (" ^ makestring_ip local_id ^ ")" ^
	 makestring_gateways gateways ^
	 (case mask of
	     NONE => ""
	   | SOME (m, {serve}) =>
	      (", mask " ^ makestring_ip m ^ " (" ^
	       (if serve then "" else "do not ") ^ "serve)")) ^
	 (case mtu of
	     NONE => ""
	   | SOME m =>
	      ", mtu " ^ Integer.toString m) ^ makestring (Setup rest)
    fun equal (a: setup, b) = a = b
    fun hash (Setup []) = 0w0
      | hash (Setup ({local_id, interface, gateways, mask, mtu} :: rest)) =
	 hash_ip local_id + hash (Setup rest)
   end
  structure Mux_Setup = Setup

(*
	18.	structure Address
*)

  structure Address: IPMUX_ADDRESS =
   struct
    type ip_number = ip_number
    datatype address = Unicast of {interface: string, peer: ip_number}
                     | Broadcast of string
    type T = address
    fun equal (a: T, b) = a = b
    fun hash (Unicast {interface, peer}) = hash_ip peer
      | hash (Broadcast _) = 0w1
    fun makestring (Unicast {interface, peer}) =
         makestring_ip peer ^ " (" ^ interface ^ ")"
      | makestring (Broadcast interface) =
         "broadcast (" ^ interface ^ ")"
   end
  structure Mux_Address = Address

(*
	19.	structure Pattern
*)

  structure Pattern: IPMUX_PATTERN =
   struct
    type ip_number = ip_number
    datatype pattern = Unicast of string
                     | Broadcast of string
                     | All
    type T = pattern
    fun equal (a: T, b) = a = b
    fun hash (Unicast interface) =
         Word.fromInt (B.V.Char.ord (B.V.String.ordof (interface, 0)))
      | hash (Broadcast interface) = hash (Unicast interface)
      | hash All = 0w0
    fun makestring (Unicast interface) = "unicast " ^ interface
      | makestring (Broadcast interface) = "broadcast " ^ interface
      | makestring All = "all interfaces "
   end
  structure Mux_Pattern = Pattern

(*
	20.	interface names
*)

  val interface1 = ref "interface1"
  val interface2 = ref "interface2"

(*
	21.	structure Mux_Connection_Key
*)

  structure Mux_Connection_Key =
   struct
    datatype T = Conn1 of Arp1.Connection_Key.T
               | Conn2 of Arp2.Connection_Key.T
    fun equal (Conn1 a, Conn1 b) = Arp1.Connection_Key.equal (a, b)
      | equal (Conn2 a, Conn2 b) = Arp2.Connection_Key.equal (a, b)
      | equal _ = false
    fun hash (Conn1 v) = Arp1.Connection_Key.hash v
      | hash (Conn2 v) = Arp2.Connection_Key.hash v
    fun makestring (Conn1 v) =
         (! interface1) ^ ": " ^ Arp1.Connection_Key.makestring v
      | makestring (Conn2 v) =
         (! interface2) ^ ": " ^ Arp2.Connection_Key.makestring v
    fun interface (Conn1 _) = ! interface1
      | interface (Conn2 _) = ! interface2
   end
  structure Connection_Key = Mux_Connection_Key

(*
	22.	other structures
*)

  structure Incoming = Arp1.Incoming
  structure Outgoing = Arp1.Outgoing
  structure Count = Arp1.Count
  structure Status = struct type T = unit fun makestring _ = "" end
  structure X = Arp1.X
  exception Already_Open of Connection_Key.T

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ipmux.fun/2"
			   val makestring = Arp1.X.makestring)

(*
	23.	other exported types
*)

  datatype interface = Enabled of string * ip_number
                     | Disabled of string

  datatype mux_session_extension =
      Mux_Session_Extension of
        {interfaces: unit -> interface list,
	 set_interface: interface -> unit,
	 maximum_packet_size: string -> Word.word,
	 minimum_packet_size: string -> Word.word}
  type session_extension = mux_session_extension
  type listen_extension = unit 
  type connection_extension = unit 

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
	24.	internal state
*)

  local
   type interface_state = {state: interface, sessions: unit ref list}

   val if_state = ref ([]: interface_state list)

   exception Interface_With_Multiple_Addresses
   exception No_Such_Interface

   fun member (el, []) = false
     | member (el, head :: tail) = el = head orelse member (el, tail)

   fun add_list (el, list) = if member (el, list) then list else el :: list

   fun remove_list (el, []) = []
     | remove_list (el, head :: tail) =
        if el = head then tail else head :: remove_list (el, tail)

   fun add ([], session, interface, local_id) =
        if is_interface1 interface orelse is_interface2 interface then
	 [{state = Enabled (interface, local_id), sessions = [session]}]
	else Trace.print_raise (No_Such_Interface, SOME ("add/" ^ interface))
     | add ((head as {state = Disabled name, sessions}) :: rest,
	    session, interface, local_id) =
        if B.V.String.caseless_equal (name, interface) then
	 {state = Enabled (name, local_id),
	  sessions = add_list (session, sessions)} :: rest
	else head :: add (rest, session, interface, local_id)
     | add ((head as {state = Enabled (name, ip), sessions}) :: rest,
	       session, interface, local_id) =
       if B.V.String.caseless_equal (name, interface) then
	if local_id = ip then
	 {state = Enabled (interface, local_id),
	  sessions = add_list (session, sessions)} :: rest
	else Trace.print_raise (Interface_With_Multiple_Addresses, SOME "add")
       else head :: add (rest, session, interface, local_id)

   fun remove ([], _) = []
     | remove ((head as {state, sessions}) :: rest, session) =
	if member (session, sessions) then
	 case remove_list (session, sessions) of
	    [] => remove (rest, session)
	  | new_list =>
	     {state = state, sessions = new_list} :: remove (rest, session)
	else head :: remove (rest, session)

   fun enable ([], session, interface, local_id) =
        Trace.print_raise (No_Such_Interface, SOME "enable")
     | enable ((head as {state = Disabled name, sessions}) :: rest,
	       session, interface, local_id) =
        if B.V.String.caseless_equal (name, interface) andalso
           member (session, sessions) then
	 {state = Enabled (interface, local_id), sessions = sessions} :: rest
	else head :: enable (rest, session, interface, local_id)
     | enable ((head as {state = Enabled (name, ip), sessions}) :: rest,
	       session, interface, local_id) =
        if B.V.String.caseless_equal (name, interface) andalso
	   ip = local_id then
	 if member (session, sessions) then
	  head :: rest
	 else
	  {state = Enabled (name, ip),
	   sessions = add_list (session, sessions)} :: rest
	else head :: enable (rest, session, interface, local_id)

   fun disable ([], session, interface) =
        Trace.print_raise (No_Such_Interface, SOME "disable")
     | disable ((head as {state = Disabled name, sessions}) :: rest,
	       session, interface) =
        if B.V.String.caseless_equal (name, interface) andalso
	   member (session, sessions) then
	 head :: rest
	else head :: disable (rest, session, interface)
     | disable ((head as {state = Enabled (name, ip), sessions}) :: rest,
	       session, interface) =
        if B.V.String.caseless_equal (name, interface) andalso
           member (session, sessions) then
	 {state = Disabled name, sessions = sessions} :: rest
	else head :: disable (rest, session, interface)

  in
   fun from_lower_address address =
        Word_Array.W32.U_Big.F.head (Word_Array.to32 address)

   fun to_lower_address ip =
        Word_Array.from32 (Word_Array.W32.U_Big.F.create (ip, 0w1))

   val void_ip = to_lower_address (Word32.fromInt (*0w*)0)

   fun init (_, []) = ((void_ip, NONE), (void_ip, NONE))
     | init (session, {local_id, interface, gateways, mask, mtu} :: rest) =
        if is_interface1 interface orelse is_interface2 interface then
         (if_state := add (! if_state, session, interface, local_id);
	  if is_interface1 interface then interface1 := interface
	  else interface2 := interface;
	  let val (first, second) = init (session, rest)
	  in if is_interface1 interface then
	      ((to_lower_address local_id, mtu), second)
	     else (first, (to_lower_address local_id, mtu))
	  end)
	else
	 Trace.print_raise (No_Such_Interface, SOME ("init/" ^ interface))

   fun finalize session = if_state := remove (! if_state, session)

   fun interfaces session () =
	let fun fold_session ({state, sessions}, rest) =
	         if member (session, sessions) then state :: rest else rest
	in B.V.List.fold fold_session (! if_state) []
	end

   fun set_interface session (Enabled (name, ip)) =
        if_state := enable (! if_state, session, name, ip)
     | set_interface session (Disabled name) =
        if_state := disable (! if_state, session, name)

   fun interface_ip interface =
        let fun loop [] =
	         Trace.print_raise (No_Such_Interface, SOME "interface_ip")
	      | loop ({state = Disabled name, sessions} :: rest) = loop rest
	      | loop ({state = Enabled (name, ip), sessions} :: rest) =
		 if B.V.String.caseless_equal (name, interface) then ip
		 else loop rest
	in loop (! if_state)
	end
  end

(*
	25.	session
*)

  fun session (Setup.Setup setup, session_function) =
       let val id = ref ()
	   val ((self1, mtu1), (self2, mtu2)) = init (id, setup)
	   fun transform_chandler1 upper_connection_handler
	                           (Arp1.C {send, abort, extension}) =
		upper_connection_handler (C {send = send, abort = abort,
					     extension = ()})
	   fun transform_chandler2 upper_connection_handler
	                           (Arp2.C {send, abort, extension}) =
		upper_connection_handler (C {send = send, abort = abort,
					     extension = ()})
	   fun transform_dhandler1 upper_data_handler
	                           (Arp1.C {send, abort, extension}, packet) =
		upper_data_handler (C {send = send, abort = abort,
				       extension = ()}, packet)
	   fun transform_dhandler2 upper_data_handler
	                           (Arp2.C {send, abort, extension}, packet) =
		upper_data_handler (C {send = send, abort = abort,
				       extension = ()}, packet)
	   fun status_handler1 (Arp1.C {send, abort, extension}, status) =
	        Trace.local_print ("received status message " ^
				   Arp1.Status.makestring status ^
				   " from interface " ^ (! interface1))
	   fun status_handler2 (Arp2.C {send, abort, extension}, status) =
	        Trace.local_print ("received status message " ^
				   Arp2.Status.makestring status ^
				   " from interface " ^ (! interface1))
	   fun new_handler1 (upper_handler, interface) lower_key =
	        let val upper_key = Connection_Key.Conn1 lower_key
		    val {connection_handler, data_handler, status_handler} =
		         upper_handler upper_key
		in {connection_handler =
		        transform_chandler1 connection_handler,
		    data_handler = transform_dhandler1 data_handler,
		    status_handler = status_handler1}
		end
	   fun new_handler2 (upper_handler, interface) lower_key =
	        let val upper_key = Connection_Key.Conn2 lower_key
		    val {connection_handler, data_handler, status_handler} =
		         upper_handler upper_key
		in {connection_handler =
		        transform_chandler2 connection_handler,
		    data_handler = transform_dhandler2 data_handler,
		    status_handler = status_handler2}
		end
	   fun call_session2 (c1, l1, max1, min1)
	                     (Arp2.S
			      {connect = c2, listen = l2,
			       extension = {maximum_packet_size = max2,
					    minimum_packet_size = min2}}) =
	        let fun new_c (Address.Unicast {interface, peer}, H handler) =
		         if is_interface1 interface then
			  ((c1 (Arp1.Arp_Address.Specific
				{self = self1, peer = to_lower_address peer,
				 protocol = ip_protocol_number1},
				Arp1.H (new_handler1 (handler, interface))))
			   handle Arp1.Already_Open x =>
			           raise Already_Open (Connection_Key.Conn1 x))
			 else
			  ((c2 (Arp2.Arp_Address.Specific
				{self = self2, peer = to_lower_address peer,
				 protocol = ip_protocol_number2},
				Arp2.H (new_handler2 (handler, interface))))
			   handle Arp2.Already_Open x =>
			          raise Already_Open (Connection_Key.Conn2 x))
		      | new_c (Address.Broadcast interface, H handler) =
		         if is_interface1 interface then
			  ((c1 (Arp1.Arp_Address.Broadcast ip_protocol_number1,
				Arp1.H (new_handler1 (handler, interface))))
			   handle Arp1.Already_Open x =>
			           raise Already_Open (Connection_Key.Conn1 x))
			 else
			  ((c2 (Arp2.Arp_Address.Broadcast ip_protocol_number2,
				Arp2.H (new_handler2 (handler, interface))))
			   handle Arp2.Already_Open x =>
			           raise Already_Open (Connection_Key.Conn2 x))
		    fun new_l (Pattern.Unicast interface, H handler, count) =
		         if is_interface1 interface then
			  let val proto = ip_protocol_number1
			      val specific = {self = self1, protocol = proto}
			      val lower = Arp1.Arp_Pattern.Specific specific
			      val handler = Arp1.H (new_handler1
						    (handler, interface))
			      val Arp1.L {stop, extension} =
			            l1 (lower, handler, count)
			  in L {stop = stop, extension = ()}
			  end
			 else
			  let val proto = ip_protocol_number2
			      val specific = {self = self2, protocol = proto}
			      val lower = Arp2.Arp_Pattern.Specific specific
			      val handler = Arp2.H (new_handler2
						    (handler, interface))
			      val Arp2.L {stop, extension} =
			            l2 (lower, handler, count)
			  in L {stop = stop, extension = ()}
			  end
		      | new_l (Pattern.Broadcast interface, H handler, count) =
		         if is_interface1 interface then
			  let val proto = ip_protocol_number1
			      val lower = Arp1.Arp_Pattern.Broadcast proto
			      val handler = Arp1.H (new_handler1
						    (handler, interface))
			      val Arp1.L {stop, extension} =
			            l1 (lower, handler, count)
			  in L {stop = stop, extension = ()}
			  end
			 else
			  let val proto = ip_protocol_number2
			      val lower = Arp2.Arp_Pattern.Broadcast proto
			      val handler = Arp2.H (new_handler2
						    (handler, interface))
			      val Arp2.L {stop, extension} =
			            l2 (lower, handler, count)
			  in L {stop = stop, extension = ()}
			  end
		      | new_l (Pattern.All, H handler, count) =
			 if is_interface1 (! interface1) andalso
			    is_interface2 (! interface2) then
			  let val L {stop = s1, extension} = 
			             new_l (Pattern.Unicast (! interface1),
					    H handler, count)
			      val L {stop = s2, extension} = 
			             new_l (Pattern.Unicast (! interface2),
					    H handler, count)
			  in L {stop = fn () => (s1 (); s2 ()), extension = ()}
			  end
			 else if is_interface1 (! interface1) then
			  new_l (Pattern.Unicast (! interface1),
				 H handler, count)
			 else if is_interface2 (! interface2) then
			  new_l (Pattern.Unicast (! interface2),
				 H handler, count)
			 else L {stop = fn () => (), extension = ()}
		    val real_max1 = case mtu1 of
		                       NONE => max1
				     | SOME m => Word.fromInt m
		    val real_max2 = case mtu2 of
		                       NONE => max2
				     | SOME m => Word.fromInt m
		    fun min interface = 
		         if is_interface1 interface then min1
			 else min2
		    fun max interface =
		         if is_interface1 interface then real_max1
			 else real_max2
		    val new_e = Mux_Session_Extension
				{interfaces = interfaces id,
				 set_interface = set_interface id,
				 maximum_packet_size = max,
				 minimum_packet_size = min}
		in session_function (S {connect = new_c, listen = new_l,
					extension = new_e})
		end
	   fun call_session1 (Arp1.S {connect, listen,
				      extension = {maximum_packet_size,
						   minimum_packet_size}}) =
	        Arp2.session (! interface2,
			      call_session2 (connect, listen,
					     maximum_packet_size,
					     minimum_packet_size))
       in ((Arp1.session (! interface1, call_session1)
	    before finalize id)
	   handle x =>
	           (finalize id;
		    Trace.print_raise_again (x, SOME "session")))
       end

 end (* struct *)
