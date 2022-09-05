(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpstate.sig: this is the signature for the TCP finite state machine.


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_STATE


	iii.	RCS Log

$Log: tcpstate.sig,v $
Revision 1.4  1996/07/22  20:46:45  cline
*** empty log message ***

Revision 1.3  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.2  1994/01/09  03:20:27  esb
first functional version.

Revision 1.1  1993/11/11  05:31:56  esb
Initial revision


	1.	signature TCP_STATE
*)

signature TCP_STATE =
 sig

  type tcp_state
  type tcp_timer

(* These calls return an updated state, often with a non-empty to_do list *)
  val active_open: tcp_state
                 * int (* initial window *)
                 * int (* maximum segment size *)
                 -> tcp_state
  val passive_open: tcp_state
                  * int (* initial window *)
                  * int (* maximum segment size *)
                  -> tcp_state
  val close: tcp_state -> tcp_state
  val abort: tcp_state -> tcp_state
  val timeout: tcp_state * tcp_timer -> tcp_state

 end (* sig *)

