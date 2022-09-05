(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

		i.	Abstract

	ip.sig: signature for protocols in the TCP/IP stack

---------------------------------------------------------------------
		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ICMP_PROTOCOL
	2.	signature IP_PROTOCOL
	3.	IP Options
	4.	ICMP
	5.	extensions

---------------------------------------------------------------------
		iii.	RCS Log

$Log: ip.sig,v $
Revision 1.42  1997/11/24  20:15:59  esb
adapted to 109.32 by Ken Cline

Revision 1.41  96/03/15  22:44:50  esb
added sharing constraings.

Revision 1.40  1996/02/23  21:15:18  esb
simplified some of the status constructors.

Revision 1.39  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.38  1995/09/26  15:47:40  esb
added traceroute ICMP message and some sharing constraints.

Revision 1.37  1995/08/29  14:14:54  esb
version that includes IP and ICMP.

Revision 1.36  1995/08/24  00:50:00  esb
added some ICMP support.

Revision 1.35  1995/08/08  18:21:36  esb
changed to support ICMP.

Revision 1.34  1995/06/23  19:55:30  esb
first version of IP that compiles with the new PROTOCOL signature.

Revision 1.33  1995/06/20  17:02:17  esb
adapted to new protocol signature.

Revision 1.32  1995/03/24  01:40:37  esb
added makestring_ip and changed incoming_packet to share with incoming_data.

Revision 1.31  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.30  1994/10/20  19:55:00  esb
got rid of ip_subprotocol.

Revision 1.29  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.28  1994/08/28  21:40:24  milnes
Modified for icmp.

Revision 1.27  1994/08/12  06:21:07  esb
added type allocation, renamed the signatures.

Revision 1.26  1994/08/02  20:32:38  esb
adapted to new protocol signature.

Revision 1.25  1994/06/16  16:39:20  danwang
Updated to use functorized Fox_Basis

Revision 1.24  1994/06/15  20:47:23  milnes
Installed subnet routing.

Revision 1.23  1994/06/09  18:36:25  esb
set type incoming_message to ip_receive, to separate out header and data.

Revision 1.22  1994/06/05  18:41:57  milnes
Added interaction with ICMP.

Revision 1.21  1994/05/23  14:00:40  milnes
Recommented passive open.

Revision 1.20  1994/05/10  07:47:09  esb
minor change.

Revision 1.19  94/05/05  14:33:58  esb
minor change.

Revision 1.18  94/05/03  20:55:11  milnes
Added ip option handling.

Revision 1.17  1994/01/21  21:10:29  esb
added time-to-live access to info and control.

Revision 1.16  1994/01/13  15:06:53  cline
Changed the type of max_packet_size.

Revision 1.15  1994/01/08  21:46:55  esb
added control operation to set the MTU.

Revision 1.14  1993/12/06  19:13:16  esb
minor fix.

Revision 1.13  1993/12/04  21:00:13  esb
improved the names and the comments.

Revision 1.12  1993/10/25  19:32:17  cline
removed .U from Byte[421].U

Revision 1.11  1993/10/08  05:23:57  esb
upgraded to have all the state in connections or in a single internal value.

Revision 1.10  1993/09/23  04:46:46  esb
removed sharing on the connection type; also added a table of contents.

Revision 1.9  1993/09/22  19:31:56  milnes
Removed "-----------"s.

Revision 1.8  1993/09/10  11:40:11  esb
added interfaces and local_address to the info datatype.

Revision 1.7  1993/09/02  22:20:22  esb
adapted to changes in the PROTOCOL signature.

Revision 1.6  1993/06/18  18:25:23  esb
changed the definition of query_result slightly; added keeping track
of actual packets sent and received.

Revision 1.5  1993/06/18  17:29:54  esb
fixed comments and formatting, and added a disable_interface control

Revision 1.4  1993/06/16  14:03:12  esb
fixed syntax error

Revision 1.3  1993/06/16  13:39:38  esb
added address option to address pattern

Revision 1.2  1993/06/15  23:48:01  esb
several changes, including new address pattern, new queries,
and new control operations

Revision 1.1  1993/06/10  23:08:40  milnes
Initial revision


	1.	signature ICMP_PROTOCOL
*)

signature ICMP_PROTOCOL =
 sig
  include PROTOCOL

  type ip_number
  sharing type Address.T = Connection_Key.T = ip_number

  type data_in
  type data_out
  type id
  type seq
  type timestamp (* needed for the NJ concrete/abstract bug: *) = Word32.word
  datatype icmp_in =
      Echo_Reply of {id: id, sequence: seq, data: data_in}
    | Timestamp_Reply of {id: id, sequence: seq, originate: timestamp,
			  receive: timestamp, transmit: timestamp,
			  returned: timestamp}
    | Traceroute of {forwarded: bool, id: Word16.word,
		     out_hops: Word16.word, return_hops: Word16.word,
		     out_speed: Word32.word, out_mtu: Word32.word}

  datatype icmp_out =
      Echo_Request of {id: id, sequence: seq, data: data_out}
    | Timestamp_Request of {id: id, sequence: seq}
    | Source_Quench of data_in
    | Parameter_Problem of data_in * int
    | Missing_Required_Option of data_in

  sharing type Incoming.T = icmp_in
      and type Outgoing.T = icmp_out
 end

(*
	2.	signature IP_PROTOCOL
*)

signature IP_PROTOCOL =
 sig

  include NETWORK_PROTOCOL

(*
	3.	IP Options
*)

  structure Option: IP_OPTION
    where type ip_option = Network_Outgoing.net_option

  sharing type Option.ip_number = Host_Id.T
     and  type Network_Outgoing.net_option = Network_Incoming.net_option

(*
	4.	ICMP
*)

  structure Icmp: ICMP_PROTOCOL
  sharing type Icmp.ip_number = Host_Id.T
      and type Icmp.data_in = Incoming.T
      and type Icmp.data_out = Outgoing.T = Network_Outgoing.T
      and type Icmp.Setup.T = Network_Setup.T
      and type Icmp.Address.T = Host_Id.T
      and type Icmp.connection_extension = network_connection_extension
      and type Icmp.Status.T = Network_Status.T

(*
	5.	extensions
*)

  type ip_session_extension = unit

  sharing type specific_session_extension = ip_session_extension 

 end (* sig *)
 where type Icmp.id = Word16.word
   and type Icmp.seq = Word16.word
   and type listen_extension = unit 
   and type specific_connection_extension = unit 
