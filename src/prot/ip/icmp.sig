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

	The ICMP protocol's signature.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ICMP_ADDRESS
	2.	signature ICMP_REDIRECT
	3.	signature ICMP_UNREACHABLE
	4.	signature ICMP_INCOMING
	5.	signature ICMP_PROTOCOL
	6.	internal structure In
	7.	internal structure Out
	8.	include PROTOCOL
	9.	ICMP-specific functions

		iii.	RCS Log
	
$Log: icmp.sig,v $
Revision 1.5  1995/06/20  17:02:17  esb
adapted to new protocol signature.

Revision 1.4  1995/03/24  01:41:57  esb
major revision.

Revision 1.3  1995/01/17  21:05:57  esb
made constructor names conform to standard.

Revision 1.2  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.1  1994/08/15  19:55:19  milnes
Initial revision

	1.	signature ICMP_ADDRESS
*)

signature ICMP_ADDRESS =
 sig
  type ip_number
  include KEY
   sharing type T = ip_number
 end (* sig *)

(*
	2.	signature ICMP_REDIRECT
*)

signature ICMP_REDIRECT =
 sig
   datatype redirect =
       Network_Redirect
     | Host_Redirect
     | Tos_Network_Redirect
     | Tos_Host_Redirect

   include PRINTABLE
    sharing type T = redirect
 end

(*
	3.	signature ICMP_UNREACHABLE
*)

signature ICMP_UNREACHABLE =
 sig
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

   include PRINTABLE
    sharing type T = unreachable
 end

(*
	4.	signature ICMP_INCOMING
*)

signature ICMP_INCOMING =
 sig
  type ip_number
  type ip_data
  type ip_option
  type redirect
  type unreachable
    (* When a parameter problem message arrives, Icmp parses it out
       and represents it as a problem in the IP header, Ip options,
       or the data of the packet. *)
  datatype problem_specifier = 
      Header of FoxWord8.word
    | Option of ip_option
    | Data of FoxWord8.word * ip_data
  val makestring_problem: problem_specifier -> string

  datatype icmp_message  =
(* these messages are normally passed to the transport layer. *)
      Unreachable of unreachable
    | Transit_Time_Exceeded
    | Reassembly_Time_Exceeded
    | Parameter_Problem of problem_specifier
    | Source_Quench 
    | Redirect of redirect
(* these messages are normally handled automatically by ICMP. *)
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

  include EXTERNAL
   sharing type T = icmp_message
 end

(*
	4.	signature ICMP_OUTGOING
*)

signature ICMP_OUTGOING =
 sig
  type ip_number
  type ip_data
  type unreachable
  datatype icmp_message =
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
  include EXTERNAL
   sharing type T = icmp_message
 end

(*
	5.	signature ICMP_PROTOCOL
*)

signature ICMP_PROTOCOL =
 sig
  type ip_number
  type ip_option
  type ip_data

  structure Icmp_Address: ICMP_ADDRESS
  structure Icmp_Connection: ICMP_ADDRESS
    sharing type Icmp_Address.ip_number = Icmp_Connection.ip_number
               = ip_number

  structure Redirect: ICMP_REDIRECT
  structure Unreachable: ICMP_UNREACHABLE
  structure In: ICMP_INCOMING
  structure Out: ICMP_OUTGOING
   sharing type In.redirect = Redirect.T
       and type In.unreachable = Out.unreachable = Unreachable.T
       and type In.ip_number = Out.ip_number = ip_number
       and type In.ip_data = Out.ip_data = ip_data
       and type In.ip_option = ip_option

(*
	8.	extensions

	by default on a connection ICMP will autonomously respond to
	all ICMP request packets, i.e. Echo, Time_Stamp, Mask_Request
	(if the serve_mask is set for the interface), and Mask_Reply.
	All other packets are passed up to the handler.  This serice
	can be explicitly disabled, and also can be re-enabled at a
	later time if desired.

	The same is true for packets from hosts to which we have
	no connections.
*)

  datatype on_off = On | Off

  type icmp_connection_extension =
        {service: unit -> on_off, set_service: on_off -> unit}

  type icmp_session_extension =
        {service: unit -> on_off, set_service: on_off -> unit,
	 serve_mask: {interface: string, mask: ip_number} -> unit,
	 stop_mask: string -> unit}

(*
	8.	include PROTOCOL
*)

  include PROTOCOL
   sharing type Setup.T = unit
       and type Pattern.T = unit
       and type Status.T = unit
       and type connection_extension = icmp_connection_extension
       and type listen_extension = unit
       and type session_extension = icmp_session_extension
       and Address = Icmp_Address
       and Connection_Key = Icmp_Connection
       and Incoming = In
       and Outgoing = Out

 end
