(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpreceive.sig: this is the signature for the TCP receive operation.


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_RECEIVE


	iii.	RCS Log

$Log: tcpreceive.sig,v $
Revision 1.2  1996/07/22  20:17:52  cline
*** empty log message ***

Revision 1.1  1993/11/11  05:31:56  esb
Initial revision


	1.	signature TCP_RECEIVE
*)

signature TCP_RECEIVE =
 sig

  type tcp_state
  type tcp_in

(* update the to_do list of the tcb to record what we need to do
   given that we've received this packet. May also change the state
   that the connection is in. *)
  val receive: tcp_state * tcp_in -> tcp_state

 end (* sig *)

