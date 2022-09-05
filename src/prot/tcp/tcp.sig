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
	tcp.sig: signature for protocols in the TCP stack


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_STATUS
	2.	signature TCP_PROTOCOL
	3.	connection management
	4.	session statistics
	5.	sharing


		iii.	RCS Log

$Log: tcp.sig,v $
Revision 1.25  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.24  96/10/28  21:39:20  esb
added per-session packet counts.

Revision 1.23  1996/07/22  20:15:02  cline
*** empty log message ***

Revision 1.22  1996/05/14  01:21:43  esb
adapted to new tcplog.sig.

Revision 1.21  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.20  1995/09/20  19:50:48  cline
Added datatypes to work around compiler bug

Revision 1.19  1995/08/08  18:29:29  cline
upgraded to new foxnet signatures

Revision 1.18  1995/06/20  17:07:29  esb
adapted to new protocol signature.

Revision 1.17  1994/08/24  15:08:36  esb
modified start_passive_port to take an optional address.

Revision 1.16  1994/08/23  15:45:26  esb
added start_passive_port

Revision 1.15  1994/08/18  20:26:36  esb
adjusted status declaration.

Revision 1.14  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.13  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.12  1994/03/07  16:30:11  esb
added the TCP window strategy control operation.

Revision 1.11  94/02/17  01:06:31  esb
cleaned up logging.

Revision 1.10  94/02/14  14:22:51  esb
added logging and statistics.

Revision 1.9  94/01/28  01:11:56  esb
minor change.

Revision 1.8  1993/12/04  20:58:45  esb
improved the comments.

Revision 1.7  1993/10/25  19:33:16  cline
removed .U from Byte[421].U

Revision 1.6  1993/10/13  19:44:49  esb
changed the definition of connection to be an opaque type.

Revision 1.5  1993/09/10  11:43:24  esb
adapted to the new protocol signature.

Revision 1.4  93/08/13  14:18:55  esb
major revision

Revision 1.3  1993/07/10  01:37:01  esb
added lower_layer_address to allow writing of functors which
do not know the format of the lower layer address

Revision 1.2  1993/06/29  15:44:23  esb
brought it up to date with proto.sig

Revision 1.1  1993/06/10  23:11:58  milnes
Initial revision

*)
(*
	1.	signature TCP_STATUS

	TCP returns a status to the status handler whenever: urgent
	data is received, the connection is closed by the peer, or the
	connection is reset by the peer.  Urgent data is only
	delivered to the status handler, which can pass it to the data
	handler if so desired.
*)

signature TCP_STATUS =
 sig
  include PRINTABLE
  type incoming
  datatype tcp_status =
      Urgent_Data of incoming
    | Connection_Closing
    | Connection_Reset
  sharing type T = tcp_status
 end (* sig *)

(*
	2.	signature TCP_PROTOCOL

	A TCP protocol uses lower-layer addresses together with local
	and remote port numbers as TCP addresses.  It is also possible
	to leave the local port unspecified if the remote port and
	peer are specified.

	The lower_layer_address will typically be an IP address (e.g.,
	a ubyte4), but should not be specified in this signature. This
	should make it easier to use TCP directly over ethernet or ATM.
*)

signature TCP_PROTOCOL =
 sig
  include TRANSPORT_PROTOCOL

  structure Tcp_Status: TCP_STATUS

(*
	3.	connection management

	Tcp windows:

	TCP does flow control to avoid swamping the receiver. In order
	to do flow control properly, the TCP must know when the user
	has finished processing the data it received. TCP can either
	assume that this is when the data handler returns, or it can
	let some handler explicitly signal how much data has been
	consumed.  Note that in the explicit case, if the handler
	never signals that data has been consumed, TCP will assume the
	receiver is swamped and refuse to accept additional data.
	Since this is undesirable, the default is to assume the data
	has been consumed whenever the data handler (or status
	handler, for urgent data) returns.  If the handler will
	consume the data long before it returns, or if the handler
	may not return, the handler should explicitly signal when
	the data has been consumed.

	In the default case, "add_to_window" has no effect.

	Send_Immediately:

	Some implementations of TCP try to accumulate small packets
	before sending them, so they can be sent more efficiently.
	RFC 1122, p. 98 specifies that a TCP MUST allow this algorithm
	to be disabled on an individual connection.  Send_Immediately
	is off by default and can only be turned on.

	Logging and statistics:

	This is useful debugging/tuning/optimizing information, but
	is made optional since keeping track of this information may
	slow down the protocol implementation.
*)

  structure Log: TCP_LOG

  datatype tcp_connection_extension =
      Tcp_Connection_Extension of
        {urgent_send: Outgoing.T -> unit,

	 automatic_window_update: unit -> bool,
	 update_automatically: unit -> unit,
	 update_explicitly: unit -> unit,
	 add_to_window: int -> unit,

	 send_immediately: unit -> unit,

	 start_logging: unit -> unit,
	 get_log: unit -> (Log.time * Log.tcp_event) list,
	 start_stats: unit -> unit,
	 get_stats: unit -> Log.tcp_stats option}

(*
	4.	session statistics
*)

  datatype tcp_session_extension =
      Tcp_Session_Extension of
        {bytes_sent: unit -> Word64.word,
	 bytes_received: unit -> Word64.word,
         packets_sent: unit -> Word64.word,
	 packets_received: unit -> Word64.word,
	 failed_sends: unit -> Word64.word,
	 packets_rejected: unit -> Word64.word}

(*
	5.	sharing
*)

  sharing Status = Tcp_Status
      and type Tcp_Status.incoming = Incoming.T
      and type session_extension = tcp_session_extension
      and type connection_extension = tcp_connection_extension

 end (* sig *)
 where type additional_listen_extension = unit
