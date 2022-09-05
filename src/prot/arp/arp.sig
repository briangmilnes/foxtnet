(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	arp.sig: signature for the Address Resolution Protocol


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ARP_ADDRESS
	2.	signature ARP_PATTERN
	3.	signature ADDRESS_RESOLUTION_PROTOCOL


	iii.	RCS Log

$Log: arp.sig,v $
Revision 1.15  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.14  96/01/19  23:05:07  esb
adapted to the new wordarray signature.

Revision 1.13  1995/10/02  21:19:19  esb
changed type of Arp_Connection_Key to be abstract.

Revision 1.12  1995/06/20  16:57:52  esb
adapted to new protocol signature.

Revision 1.11  1995/01/06  01:37:45  esb
renamed the interface to filter.

Revision 1.10  1994/12/21  20:36:49  milnes
Updated for new shadowed addressing, but it produces terrible performance
problems when the duplicate filters are installed.

Revision 1.9  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.8  1994/08/12  06:24:27  esb
added type allocation.

Revision 1.7  1994/08/02  20:29:33  esb
adapted to new protocol signature.

Revision 1.6  1993/10/27  01:19:26  esb
made the types explicit by using datatypes.

Revision 1.5  1993/10/25  19:36:25  cline
removed .U from Byte[421].U

Revision 1.4  1993/10/09  17:49:36  esb
consolidated the protocol state; we now use the new store interface.

Revision 1.3  1993/09/22  19:31:56  milnes
Removed "-----------"s.

Revision 1.2  1993/09/13  22:07:43  cline
deleted '#'s from RCS log

Revision 1.1  1993/08/24  21:20:29  esb
Initial revision

	1.	signature ARP_ADDRESS
*)

signature ARP_ADDRESS =
 sig
  type host
  type protocol
  datatype address = Specific of {self: host, peer: host, protocol: protocol}
                   | Broadcast of protocol
  include KEY
   sharing type T = address
 end (* sig *)

(*
	2.	signature ARP_PATTERN

	An Arp_pattern always specifies that broadcast packets
	will be accepted.
*)

signature ARP_PATTERN =
 sig
  type host
  type protocol
  datatype pattern = Specific of {self: host, protocol: protocol}
                   | Broadcast of protocol
  include KEY
   sharing type T = pattern
 end (* sig *)

(*
	3.	signature ADDRESS_RESOLUTION_PROTOCOL

	An address_resolution module lets a higher-level module use
	protocol addresses that may be different from lower-level
	hardware addresses.  The higher-level module never needs to
	see the lower-level addresses; we use byte arrays to store
	higher-level addresses.
*)

signature ADDRESS_RESOLUTION_PROTOCOL =
 sig

  structure Host: KEY
  structure Protocol: KEY
  structure Arp_Address: ARP_ADDRESS
  structure Arp_Pattern: ARP_PATTERN
   sharing type Host.T = Arp_Address.host = Arp_Pattern.host
       and type Protocol.T = Arp_Address.protocol = Arp_Pattern.protocol

  type arp_session_extension = {maximum_packet_size: Word.word,
				minimum_packet_size: Word.word}

  include PROTOCOL
   sharing Arp_Address = Address
       and Arp_Pattern = Pattern
       and type session_extension = arp_session_extension
 end
 where type listen_extension = unit
