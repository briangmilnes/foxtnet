(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	An IP that receives ICMP messages on its status channel.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	iv.	Overview
	1.	functor Ip_With_Icmp 
	2.	using parts of Ip
	3.	State and initialize and finalize
	4.	funtion serve_local_problem
	5.	translation functions
	6.	function connect
	7.	function start_passive
	8.	functions close and abort
	9.	function makestring_status


		iii.	RCS Log
	
$Log: ipicmp.fun,v $
Revision 1.10  1995/03/24  01:47:43  esb
major revision.

Revision 1.9  1995/03/12  17:55:39  esb
adapted to new trace.sig.

Revision 1.8  1995/03/07  23:43:54  esb
updated tracing.

Revision 1.7  1995/01/17  21:07:20  esb
adapted to new icmp.sig

Revision 1.6  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.5  1994/11/11  18:11:52  esb
changed the functor parameters for Debug_Trace_Printer.

Revision 1.4  1994/11/10  16:12:20  milnes
Updated for tcpipeth/addressing and debug_trace structure.

Revision 1.3  1994/10/20  19:55:34  esb
made ip_subprotocol local, renamed the functor.

Revision 1.2  1994/09/14  15:32:07  milnes
Fixed a status piping bug.

Revision 1.1  1994/08/25  23:43:13  milnes
Initial revision

		iv.	Overview

	The general structure of ICMP is such that it almost has to be
	integrated in the same layer as IP; this however complicates
	the implementation of IP and is esthetically unattractive.  In
	the foxnet, ICMP is structured as follows:

	- we define a generic interface (ip.sig) which assumes the
	  implementation of ICMP functions.

	- we implement a simplified IP (ip.fun) which purely implements
	  IP, with the ICMP functions left unimplemented.

	- we implement ICMP (icmp.fun) which implements ICMP on top of IP.

	- we use this module to implement the full IP and ICMP by bringing
	  together the two submodules, ICMP and IP-without-ICMP.

		1.	functor Ip_With_Icmp 

*)

functor Ip_With_Icmp (structure B: FOX_BASIS
		      structure Ip: EXTENDED_IP
		      structure Icmp: ICMP_PROTOCOL
		         sharing type Ip.outgoing = Ip.incoming_data =
			              Icmp.ip_data = B.Dyn_Array.T
		             and type Icmp.ip_number = Ip.ip_number
		      val log_icmp_echos: bool
		      val debug_level: int ref option): IP_PROTOCOL =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ipicmp.fun")
  val local_print = Trace.local_print

(*
		2.	using parts of Ip
*)

  local 
   signature IP_SUBPROTOCOL =
    sig
     type ip_number
     type ip_protocol
     type incoming_data

     datatype ip_address =
         Address of {ip: ip_number, proto: ip_protocol}
       | Partial of {proto: ip_protocol}

     datatype ip_incoming =
         Ip_Packet of {source: ip_number,
		       checksum: FoxWord16.word,
		       data: incoming_data}

     structure Option: IP_OPTION
         sharing type Option.ip_number = ip_number

     sharing type incoming_data = B.Dyn_Array.T

     include PROTOCOL
      sharing type address = ip_address
          and type incoming = ip_incoming
          and type allocation = int
	  and type ip_number = Icmp.ip_number
	  and type incoming_data = Icmp.ip_data

     val max_packet_size: connection -> int
     val local_address: ip_number -> (string * ip_number) option
     val remote_address: connection -> address
     val packets_sent: unit -> int
     val packets_received: unit -> int
     val failed_sends: unit -> int
     val packets_rejected: unit -> int
     val makestring_ip: ip_number -> string

     val options: connection -> Option.T list
     val set_options: connection * Option.T list -> unit
     exception No_Waiting_Passive
     val quench_source: connection -> unit
     val time_to_live: connection -> int
     val set_time_to_live: connection * int -> unit
     val interfaces: unit -> (string * ip_number option) list
     val set_interface_address: string * ip_number -> unit
     val disable_interface: string -> unit
    end (* sig *)

   structure From_Ip: IP_SUBPROTOCOL = Ip
  in

   open From_Ip
  end 

  structure Icmp = Icmp

  (* redefine ip_status to use Icmp.In.icmp_message instead of
     Ip.Icmp.In.icmp_message.  Also redefine specific_status to
     show we are not exporting any implementation-specific stuff. *)
  datatype specific_status = None

  datatype ip_status =
      Option_Packet of (Option.T list * ip_incoming)
    | Icmp_Specific of Icmp.In.icmp_message
    | Ip_Specific of specific_status
  type status = ip_status

  datatype handler = 
      Handler of connection -> ((incoming -> unit) * (status -> unit))

(*
		3.	State and initialize and finalize
*)

  exception Ip_Over_Icmp_Implementation_Error of string

  val connections = ref ([]: (Ip.connection * Icmp.connection) list)

  local
   val initialization_count = ref 0

  in
   fun initialize () = 
        (Ip.initialize ();
         Icmp.initialize ();
         if ! initialization_count = 0 then connections := []
	 else ();
         initialization_count := ! initialization_count + 1;
         ! initialization_count)

   fun finalize () =
         (if ! initialization_count = 1 then
	    connections := []
	  else ();
	  Icmp.finalize ();
	  Ip.finalize ();
	  initialization_count := ! initialization_count - 1;
	  ! initialization_count)
  end

(*
		4.	funtion serve_local_problem
*)

  fun send_port_unreachable (conn, ip, data) =
       ((let val out = Icmp.Out.Unreachable (Icmp.Port_Unreachable, data)
	     val (_, send) = Icmp.allocate_send (conn, out)
	 in send ();
	    Icmp.close conn
	 end)
	  handle x => local_print ("exception " ^ System.exn_name x ^
				   " in ICMP response"))

  fun send_reassembly_time_exceeded (conn, packet) =
       local_print "seen reassembly_time_exceeded"

  fun serve_local_problem (_, Ip.Local_Parameter_Problem (packet, position)) =
       (local_print "parameter problem, service not implemented";
	raise (Ip_Over_Icmp_Implementation_Error "not implemented"))
    | serve_local_problem (conn, Ip.Local_Reassembly_Time_Exceeded packet) =
       send_reassembly_time_exceeded (conn, packet)
    | serve_local_problem (_,
			   Ip.Local_Missing_Required_Option (packet, option)) =
       (local_print "missing required option, service not implemented";
	raise (Ip_Over_Icmp_Implementation_Error "not implemented"))
    | serve_local_problem (conn, Ip.Local_Unreachable (ip, packet)) =
       send_port_unreachable (conn, ip, packet)

(*
		5.	translation functions
*)

  fun status_dispatch (_, ip_status_handler) (Ip.Option_Packet stuff) =
       ip_status_handler (Option_Packet stuff)
    | status_dispatch (connection, _) (Ip.Ip_Specific status) =
       serve_local_problem (connection, status)
    | status_dispatch (_, ip_status_handler) (Ip.Icmp_Specific message) =
       (local_print ("status_dispatch got Icmp_Specific status " ^
		     Ip.Icmp.makestring_incoming (message, NONE) ^
		     " from Ip_No_Icmp");
	raise (Ip_Over_Icmp_Implementation_Error "Icmp_Specific status"))

  local
   fun ignore_icmp_status s =
        local_print ("ignoring ICMP status " ^ Icmp.makestring_status s)

   fun to_ip_status ip_status_handler icmp_data =
        ip_status_handler (Icmp_Specific icmp_data)

  in
   fun icmp_data_to_ip_status ip_status_handler icmp_connection =
        (to_ip_status ip_status_handler, ignore_icmp_status)
  end 

  fun data_translation (ip_number, ip_handler) ip_connection =
       let val (ip_data_handler, ip_status_handler) = ip_handler ip_connection
	   val icmp_handler =
	        Icmp.Handler (icmp_data_to_ip_status ip_status_handler)
	   val icmp_addr = Icmp.Icmp_Address ip_number
	   val icmp_connection = Icmp.connect (icmp_addr, icmp_handler)
       in connections := (ip_connection, icmp_connection) :: (! connections);
	  (ip_data_handler,
	   status_dispatch (icmp_connection, ip_status_handler))
       end

(*
		6.	function connect
*)

  fun connect (address as (Ip.Partial _), _) =
       (local_print ("passive address " ^ makestring_address address ^
		     " used in connect");
	raise Illegal_Address "passive address in connect")
    | connect (address as (Ip.Address {ip, ...}), Handler ip_handler) =
        let val new_handler = data_translation (ip, ip_handler)
	in Ip.connect (address, Ip.Handler new_handler)
	end
  
(* 
		7.	function start_passive

	When an ip_connection is opened, find its remote address,
	build the icmp address to correspond to this, open on that
	address and pipe all this together.
*)

  local
   fun new_handler ip_handler ip_connection =
        case Ip.remote_address ip_connection of
	   Ip.Address {ip, ...} =>
	    data_translation (ip, ip_handler) ip_connection
	 | _ => 
	    raise (Ip_Over_Icmp_Implementation_Error
		   "illegal address in start_passive handler")

  in (* local *)

   fun start_passive (ip_address, Handler ip_handler, count) =
        Ip.start_passive (ip_address,
			  Ip.Handler (new_handler ip_handler), count)
  end (* local *)

(*
		8.	functions close and abort
*)

  local
   fun apply_remove (close_ip, close_icmp, ip_conn, []) = 
        raise (Ip_Over_Icmp_Implementation_Error "apply_remove")
     | apply_remove (close_ip, close_icmp, ip_conn, (ip, icmp) :: rest) =
        if Ip.equal_connection (ip, ip_conn) then
	 (close_icmp icmp;
	  close_ip ip;
	  rest)
        else (ip, icmp) :: apply_remove (close_ip, close_icmp, ip_conn, rest)

   fun close_abort (ip_op, icmp_op) ip_conn =
	connections := apply_remove (ip_op, icmp_op, ip_conn, ! connections)

  in (* local *)
   val close = close_abort (Ip.close, Icmp.close)
   val abort = close_abort (Ip.abort, Icmp.abort)
  end (* local *)

(*
		9.	function makestring_status
*)

  fun makestring_status (Option_Packet (options, data)) =
       Ip.Option.makestring_options options ^ ": " ^
       Ip.makestring_incoming (data, NONE)
    | makestring_status (Icmp_Specific m) =
       "ICMP specific status, " ^ Icmp.makestring_incoming (m, NONE)
    | makestring_status (Ip_Specific _) = "implementation-specific status"

 end
