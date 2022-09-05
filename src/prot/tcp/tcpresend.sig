(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	Care and feeding of tcp's retransmit queue.


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_RETRANSMIT

	iii.	RCS Log
	
$Log: tcpresend.sig,v $
Revision 1.7  1996/07/22  20:18:51  cline
*** empty log message ***

Revision 1.6  1996/05/14  01:25:05  esb
changed to support Timestamp options by providing a send time.

Revision 1.5  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.4  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.3  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.2  1994/01/30  20:57:51  esb
changed the ack return value.

Revision 1.1  1994/01/20  19:29:29  esb
Initial revision


	1.	signature TCP_RETRANSMIT
*)

signature TCP_RETRANSMIT =
 sig
  structure Tcb: TCP_TCB

  (* we define as an invariant that there is an outstanding
     retransmit timer (either in the to_do list, or in the timer
     module) exactly when the resend queue is non-empty.
     This invariant must be preserved (a) whenever the retransmit
     timer expires, (b) whenever a segment is added to the empty
     resend queue, and (c) whenever the last segment is removed
     from the resend queue. *)

  val retransmit: Tcb.tcp_tcb * Tcb.tcp_out * Tcb.time_ms
                -> Tcb.tcp_tcb

  val acknowledge: Tcb.tcp_state * Tcb.tcp_tcb * Word32.word
                 * Tcb.time_ms option (* send time, if known *)
                 -> Tcb.tcp_tcb

 end (* sig *)




