(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpsend.sig: this is the signature for the TCP send operation.


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_SEND


	iii.	RCS Log

$Log: tcpsend.sig,v $
Revision 1.11  1997/04/22  11:25:39  esb
adapted to new tcptcb.sig by removing the optional packet send function.

Revision 1.10  96/10/18  20:48:52  esb
changed send to send_queued.

Revision 1.9  1996/07/22  20:46:03  cline
*** empty log message ***

Revision 1.8  1996/05/16  23:53:02  esb
made a comment clearer.

Revision 1.7  1995/09/14  21:11:50  cline
work around for representation bug

Revision 1.6  1994/08/18  20:29:37  esb
added urgent support.

Revision 1.5  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.4  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.3  1994/03/11  04:58:55  esb
send now reports whether it succeeded or failed.

Revision 1.2  1994/01/09  03:21:21  esb
first functional version.

Revision 1.1  1993/11/11  05:31:56  esb
Initial revision


	1.	signature TCP_SEND
*)

signature TCP_SEND =
 sig

  type tcp_state
  type tcp_out
  type send_packet
  type segment

  datatype send_state = Valid_Send | Opening_Send | Closing_Send

  val send_state: tcp_state -> send_state

  (* Send_queued takes the latest TCP state and returns an updated state
     after having sent as many queued packets as possible, or a
     window probe if it is not possible to send any packets. *)
  val send_queued: tcp_state -> tcp_state

  exception Send_Packet of string

  (* send_packet take the state, urgent flag, and packet.
     It then calls send to do the actual sending, which is
     accomplished by changing the tcp_state rather than
     actually "sending" anything. *)
  val send_packet:
        tcp_state * send_packet * bool -> tcp_state

 end (* sig *)

