(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
        Nick Haines (nickh@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891


		i.	Abstract

	eth.sig: signature for ethernet protocols

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ETH_NUMBER
	2.	signature ETH_ADDRESS
	3.	signature ETH_PATTERN
	4.	signature ETHERNET_PROTOCOL

		iii.	RCS Log
	
$Log: eth.sig,v $
Revision 1.24  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.23  96/01/19  23:01:26  esb
adapted to the new wordarray signature.

Revision 1.22  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.21  1995/11/10  23:31:35  esb
added sharing constraint on setup type.

Revision 1.20  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.19  1995/10/02  20:59:19  esb
added a sharing constraint Setup.T = unit

Revision 1.18  1995/09/14  21:07:56  cline
work around for representation bug

Revision 1.17  1995/06/20  16:56:38  esb
adapted to new protocol signature.

Revision 1.16  1995/02/21  15:45:18  esb
made packet counters into 64-bit quantities.

Revision 1.15  1995/01/18  20:59:20  esb
renamed datatype address to be eth_address.

Revision 1.14  1995/01/06  01:33:17  esb
removed set_arp_filter and clear_arp_filter.

Revision 1.13  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.12  1994/08/12  06:20:16  esb
added type allocation and set/clear_arp_filter.

Revision 1.11  1994/08/02  20:26:48  esb
adapted to new protocol signature.

Revision 1.10  1994/06/23  14:21:59  danwang
Changed Byte#.ubytes to ubyte#.

Revision 1.9  1994/06/16  16:37:34  danwang
Updated to use functorized Fox_Basis

Revision 1.8  1993/12/04  21:01:35  esb
very minor changes.

Revision 1.7  1993/10/25  19:31:51  cline
removed .U from Byte[421].U

Revision 1.6  1993/10/06  12:11:28  milnes
Updated to centralize state and with changes from code review.

Revision 1.5  1993/09/02  15:55:33  esb
changed the query and result types to info.

Revision 1.4  1993/06/16  19:58:27  esb
cleaned up a little

Revision 1.3  1993/06/16  09:00:58  esb
changed the definition of address_pattern to only include protocol type

Revision 1.2  1993/06/14  18:50:33  esb
added protocol number to address patterns

Revision 1.1  1993/06/10  23:06:53  milnes
Initial revision

*)

(*
	1.	signature ETH_NUMBER
*)

signature ETH_NUMBER =
 sig
  include KEY
  val new: Word48.word -> T
  val convert: T -> Word48.word
 end (* sig *)

(*
	2.	signature ETH_ADDRESS
*)

signature ETH_ADDRESS =
 sig
  include KEY
  type eth_number
  type eth_protocol
  datatype address = Address of {eth: eth_number, proto: eth_protocol}
  sharing type T = address
 end (* sig *)

(*
	3.	signature ETH_PATTERN
*)

signature ETH_PATTERN =
 sig
  include KEY
  type eth_number
  type eth_protocol
  datatype pattern = Complete of {eth: eth_number, proto: eth_protocol}
                   | Partial of {proto: eth_protocol}
  sharing type T = pattern
 end (* sig *)

(*
	4.	signature ETHERNET_PROTOCOL
*)

signature ETHERNET_PROTOCOL =
 sig
  include PROTOCOL

  structure Eth_Number: ETH_NUMBER where type T = Word48.word
  structure Eth_Protocol: KEY where type T = Word16.word

  structure Eth_Address: ETH_ADDRESS where type eth_number = Eth_Number.T
				       and type eth_protocol = Eth_Protocol.T
  structure Eth_Pattern: ETH_PATTERN where type eth_number = Eth_Number.T
				       and type eth_protocol = Eth_Protocol.T

  datatype eth_connection_extension =
      Eth_Connection_Extension of {connection_address: Address.T}

  datatype eth_session_extension =
      Eth_Session_Extension of
        {local_address: Eth_Number.T,
	 packets_sent: unit -> Word64.word,
	 packets_received: unit -> Word64.word,
	 failed_sends: unit -> Word64.word,
	 packets_rejected: unit -> Word64.word,
	 minimum_packet_size: Word.word,
	 maximum_packet_size: Word.word}

  sharing Address = Eth_Address = Connection_Key
      and Pattern = Eth_Pattern
      and type session_extension = eth_session_extension
      and type connection_extension = eth_connection_extension

 end (* sig *)
 where type Setup.T = string
   and type Status.T =unit
   and type listen_extension = unit
