(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	ipmux.sig: signature for the Internet Protocol Multiplexer layer.


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature IPMUX_ADDRESS
	2.	signature IPMUX_PATTERN
	3.	signature IPMUX_CONNECTION_KEY
	4.	signature IP_MULTIPLEXER


	iii.	RCS Log

$Log: ipmux.sig,v $
Revision 1.14  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.13  96/02/23  21:12:01  esb
added Mux_Connection_Key, removed Mux_Incoming.

Revision 1.12  1996/01/19  23:02:29  esb
adapted to the new wordarray signature.

Revision 1.11  1995/11/12  16:31:08  esb
changed constructor "Disabled" to carry the interface name.

Revision 1.10  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.9  1995/10/02  21:22:00  esb
made connection_key into abstract type.

Revision 1.8  1995/09/14  21:09:21  cline
work around for representation bug

Revision 1.7  1995/08/29  14:14:54  esb
version that includes IP and ICMP.

Revision 1.6  1995/06/23  19:55:30  esb
first version of IP that compiles with the new PROTOCOL signature.

Revision 1.5  1995/06/20  17:02:17  esb
adapted to new protocol signature.

Revision 1.4  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.3  1994/08/29  18:15:30  robby
added Local_Info

Revision 1.2  1994/08/12  06:23:17  esb
added type allocation.

Revision 1.1  1994/08/03  19:17:19  esb
Initial revision

*)

(*
	1.	signature IPMUX_ADDRESS
*)

signature IPMUX_ADDRESS =
 sig
  include KEY
  type ip_number
  datatype address = Unicast of {interface: string, peer: ip_number}
                   | Broadcast of string
  sharing type T = address
 end (* sig *)

(*
	2.	signature IPMUX_PATTERN
*)

signature IPMUX_PATTERN =
 sig
  include KEY
  type ip_number
  datatype pattern = Unicast of string
                   | Broadcast of string
                   | All
  sharing type T = pattern
 end (* sig *)

(*
	3.	signature IPMUX_CONNECTION_KEY
*)

signature IPMUX_CONNECTION_KEY =
 sig
  include KEY
  val interface: T -> string
 end (* sig *)

(*
	4.	signature IP_MULTIPLEXER

	Modules satisfying this signature provide the same functions
	as those satisfying ADDRESS_RESOLUTION, but using a
	different syntax that lets us select among multiple
	lower-level interfaces.  For now we use string as the
        type of an interface identifier.
*)

signature IP_MULTIPLEXER =
 sig
  include PROTOCOL

  type ip_number

  structure Mux_Setup: NETWORK_SETUP
  structure Mux_Address: IPMUX_ADDRESS
  structure Mux_Pattern: IPMUX_PATTERN
  structure Mux_Connection_Key: IPMUX_CONNECTION_KEY

  datatype interface = Enabled of string * ip_number
                     | Disabled of string

  datatype mux_session_extension =
      Mux_Session_Extension of
        {interfaces: unit -> interface list,
	 set_interface: interface -> unit,
	 maximum_packet_size: string -> Word.word,
	 minimum_packet_size: string -> Word.word}

  sharing Address = Mux_Address
      and Pattern = Mux_Pattern
      and Setup = Mux_Setup 
      and Connection_Key = Mux_Connection_Key 
      and type ip_number = Mux_Address.ip_number = Mux_Pattern.ip_number =
               Mux_Setup.host_id
      and type session_extension = mux_session_extension
 end
 where type listen_extension = unit 
