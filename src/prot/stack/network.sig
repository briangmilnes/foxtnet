(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

		i.	Abstract
	network.sig: signature for network protocols in the TCP/IP stack

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature NETWORK_HOST_ID
	2.	signature NETWORK_SETUP
	3.	signature NETWORK_ADDRESS
	4.	signature NETWORK_PATTERN
	5.	signature NETWORK_INCOMING
	6.	signature NETWORK_OUTGOING
	7.	signature NETWORK_STATUS
	8.	signature NETWORK_PROTOCOL
	9.	basic types
	10.	specialized sub-structures
	11.	extensions
	12.	sharing constraints

		iii.	RCS Log

$Log: network.sig,v $
Revision 1.16  1996/07/22  17:45:54  cline
added outgoing sharing constraint

Revision 1.15  1996/03/12  22:27:19  esb
added port_unreachable to connection_extension.

Revision 1.14  1996/03/04  21:33:25  esb
removed install_parse.

Revision 1.13  1996/02/23  21:37:40  esb
converted structure sharing to type sharing, added key_to_address.

Revision 1.12  1996/01/19  23:06:05  esb
adapted to the new wordarray signature.

Revision 1.11  1996/01/16  22:38:59  cline
*** empty log message ***

Revision 1.10  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.9  1995/09/26  15:47:17  esb
added a status that makes it possible to do MTU discovery.

Revision 1.8  1995/09/14  21:12:51  cline
work around for representation bug

Revision 1.7  1995/09/13  15:33:13  esb
added parsing of network host IDs.

Revision 1.6  1995/08/29  14:12:04  esb
changes in status, option handling, and setup

Revision 1.5  1995/08/24  00:52:50  esb
extended the status for better handling of ICMP statuses.

Revision 1.4  1995/08/08  18:26:31  esb
added sharing constraint for Network_Setup.host_id.

Revision 1.3  1995/06/29  19:54:52  esb
added pseudo-header calculation for outgoing messages.

Revision 1.2  1995/06/23  19:57:36  esb
minor fix.

Revision 1.1  1995/06/20  17:20:21  esb
Initial revision

	1.	signature NETWORK_HOST_ID
*)

signature NETWORK_HOST_ID =
 sig
  include KEY

  val parse: string -> T option
 end

(*
	2.	signature NETWORK_SETUP
*)

signature NETWORK_SETUP =
 sig
  include KEY
  type host_id
  datatype setup = Setup of {local_id: host_id, interface: string,
			     gateways: host_id list, mtu: int option,
			     mask: (host_id * {serve: bool}) option} list
  sharing type T = setup
 end (* sig *)

(*
	3.	signature NETWORK_ADDRESS
*)

signature NETWORK_ADDRESS =
 sig
  include KEY
  type host_id
  type protocol_id
  (* parameterize address to work around SML/NJ's representation bug *)
  datatype 'a internal_address = Address of {peer: 'a,
					     proto: protocol_id}
  type address = host_id internal_address
  sharing type T = address
 end (* sig *)

(*
	4.	signature NETWORK_PATTERN
*)

signature NETWORK_PATTERN =
 sig
  include KEY
  type host_id
  type protocol_id
  (* parameterize pattern to work around SML/NJ's representation bug *)
  datatype 'a internal_pattern = Complete of {peer: 'a,
					      proto: protocol_id}
	                       | Partial of {proto: protocol_id}
  type pattern = host_id internal_pattern
  sharing type T = pattern
 end (* sig *)

(*
	5.	signature NETWORK_INCOMING

	A NETWORK_INCOMING structure provides all the operations of a
	regular EXTERNAL, and in addition also a pseudo-header
	checksum and protocol-dependent options.
*)

signature NETWORK_INCOMING =
 sig
  include EXTERNAL
  type checksum = Word16.word
  val pseudo_header_checksum: T -> checksum
  type net_option
  val options: T -> net_option list
 end (* sig *)

(*
	6.	signature NETWORK_OUTGOING

	A NETWORK_OUTGOING structure provides all the operations of a
	regular EXTERNAL, and in addition also protocol-dependent options.
*)

signature NETWORK_OUTGOING =
 sig
  include EXTERNAL
  type net_option
  val new_options: Word_Array.T * net_option list -> T
  val put_options: T * net_option list -> T
 end (* sig *)

(*
	7.	signature NETWORK_STATUS

	The Unit field is added to get around the SML/NJ's bug which
	results in the following message:

Error: The constructor Quench of datatype status
has different representations in the signature and the structure.
Change the definition of the types carried by the constructors in the
functor formal parameter and the functor actual parameter so that they
are both abstract, or so that neither is abstract.

	This bug is utterly annoying, but the NJ crowd speaks as if
	it were the best thing since sliced bread, so there is not
	much hope of its going away.
*)

signature NETWORK_STATUS =
 sig
  include PRINTABLE
  type higher_header
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
   sharing type T = status
 end (* sig *)

(*
	8.	signature NETWORK_PROTOCOL

	NETWORK_PROTOCOL has all the objects that a transport protocol
	might need from a network protocol.
*)

signature NETWORK_PROTOCOL =
 sig

  include PROTOCOL

(*
	9.	basic types
*)

  structure Host_Id: NETWORK_HOST_ID
  structure Protocol_Id: KEY

(*
	10.	specialized sub-structures
*)

  structure Network_Setup: NETWORK_SETUP
  structure Network_Address: NETWORK_ADDRESS
  structure Network_Pattern: NETWORK_PATTERN
  structure Network_Incoming: NETWORK_INCOMING
  structure Network_Outgoing: NETWORK_OUTGOING
  structure Network_Status: NETWORK_STATUS
   sharing type Host_Id.T = Network_Setup.host_id
              = Network_Address.host_id = Network_Pattern.host_id
       and type Protocol_Id.T = Network_Address.protocol_id
	      = Network_Pattern.protocol_id
       and type Network_Incoming.net_option = Network_Outgoing.net_option
       and type Network_Status.higher_header = Network_Incoming.T

(*
	11.	extensions
*)

  type specific_connection_extension
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

  type specific_session_extension
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
     manual overrides, and should not be needed under normal operation. *)
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

(*
	12.	sharing constraints
*)

  sharing type Setup.T = Network_Setup.T
      and type Address.T = Network_Address.T
      and type Pattern.T = Network_Pattern.T
      and type Incoming.T = Network_Incoming.T
      and type Outgoing.T = Network_Outgoing.T
      and type Status.T = Network_Status.T
      and type network_session_extension = session_extension
      and type connection_extension = network_connection_extension

  val key_to_address: Connection_Key.T -> Network_Address.T

 end (* sig *)
