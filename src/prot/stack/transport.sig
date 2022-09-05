(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Nick Haines (nickh@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

		i.	Abstract

	transport.sig: signature for transport protocols in the TCP/IP stack

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TRANSPORT_HOST_ID
	2.	signature TRANSPORT_SETUP
	3.	signature TRANSPORT_ADDRESS
	4.	signature TRANSPORT_PATTERN
	5.	signature TRANSPORT_CONNECTION_KEY
	6.	signature TRANSPORT_PROTOCOL
		signature TRANSPORT_PROTOCOL

		iii.	RCS Log

$Log: transport.sig,v $
Revision 1.10  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.9  96/02/23  21:38:01  esb
converted structure sharing to type sharing.

Revision 1.8  1996/02/15  19:03:14  esb
added Transport_Setup.

Revision 1.7  1996/01/16  22:38:59  cline
*** empty log message ***

Revision 1.6  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.5  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.4  1995/10/13  15:54:02  cstone
Added Host_Id structure

Revision 1.3  1995/09/15  16:40:39  cline
work around for representation analysis bug

Revision 1.2  1995/07/21  12:51:47  esb
renamed protocol_listen_extension to transport_listen_extension.

Revision 1.1  1995/06/20  17:20:21  esb
Initial revision

	1.	signature TRANSPORT_HOST_ID
*)

signature TRANSPORT_HOST_ID = NETWORK_HOST_ID

(*
	2.	signature TRANSPORT_SETUP
*)

signature TRANSPORT_SETUP = NETWORK_SETUP

(*
	3.	signature TRANSPORT_ADDRESS

	a program using a transport protocol can specify connections
	in different ways:
	- by completely specifying the connection
	- by leaving the local port unspecified (used with connect).
	  In this case, the implementation selects a free local port.
*)

signature TRANSPORT_ADDRESS =
 sig
  include KEY
  type host_id
  type port
  datatype address = 
	Complete of {peer: host_id, local_port: port, remote_port: port}
      | Remote_Specified of {peer: host_id, remote_port: port}
  sharing type T = address
 end (* sig *)

(*
	4.	signature TRANSPORT_PATTERN

	a program using a transport protocol can listen for:
	- a completely specified connection
	- by leaving the local port unspecified (used with connect).
	  In this case, the implementation selects a free local port.
	- by leaving the peer and remote port unspecified 
	  In this case, the missing components are taken from the
	  connections that are initiated by the peer.
	- by leaving everything unspecified 
	  In this case, the transport protocol must select a
	  local port.  There are no guarantees about this port
	  being "available".
*)

signature TRANSPORT_PATTERN =
 sig
  include KEY
  type host_id
  type port
  datatype pattern = 
	Complete of {peer: host_id, local_port: port, remote_port: port}
      | Remote_Specified of {peer: host_id, remote_port: port}
      | Local_Specified of {local_port: port}
      | Unspecified
  sharing type T = pattern
 end (* sig *)

(*
	5.	signature TRANSPORT_CONNECTION_KEY
*)

signature TRANSPORT_CONNECTION_KEY =
 sig
  include KEY
  type host_id
  type port
  datatype key = Key of {peer: host_id, local_port: port, remote_port: port}
  sharing type T = key
 end (* sig *)

(*
	6.	signature TRANSPORT_PROTOCOL

	A TCP protocol uses lower-layer addresses together with local
	and remote port numbers as TCP addresses.  It is also possible
	to leave the local port unspecified if the remote port and
	peer are specified.


*)

signature TRANSPORT_PROTOCOL =
 sig

  include PROTOCOL

  type host_id
  type port = Word16.word
  
  structure Host_Id: TRANSPORT_HOST_ID where type T = host_id

  structure Transport_Setup: TRANSPORT_SETUP
			       where type host_id = host_id
  structure Transport_Address: TRANSPORT_ADDRESS
			         where type host_id = host_id
				   and type port = port
  structure Transport_Pattern: TRANSPORT_PATTERN
			         where type host_id = host_id
				   and type port = port
  structure Transport_Key: TRANSPORT_CONNECTION_KEY
			     where type host_id = host_id
				   and type port = port

  type additional_listen_extension
  datatype transport_listen_extension =
        Listen_Extension of {local_port: port,
			     additional: additional_listen_extension}

  sharing type Setup.T = Transport_Setup.T
      and type Address.T = Transport_Address.T
      and type Connection_Key.T = Transport_Key.T
      and type Pattern.T = Transport_Pattern.T
      and type listen_extension = transport_listen_extension

 end (* sig *)
