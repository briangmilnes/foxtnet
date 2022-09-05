(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (ken.cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpheader.sig: executing the actions specified by
	the TCP state machine. The actions must be those
	defined in tcptcb.sig.


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_HEADER



	iii.	RCS Log

$Log: tcpheader.sig,v $
Revision 1.6  1996/07/22  20:16:03  cline
*** empty log message ***

Revision 1.5  1996/03/15  22:45:38  esb
removed allocate_segment, which was obsolete.

Revision 1.4  1996/01/19  23:03:30  esb
adapted to the new wordarray signature.

Revision 1.3  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.2  1995/09/14  21:11:12  cline
work around for representation bug

Revision 1.1  1995/08/08  18:38:53  cline
Initial revision


	1.	signature TCP_HEADER
*)

signature TCP_HEADER =
 sig

  type segment				(* data accepted from TCP *)
  type send_packet			(* data sent on lower layer *)
  type receive_packet			(* data received from lower layer *)
  type to_do_list
  datatype address = Address of {local_port: Word16.word,
				 remote_port: Word16.word}

  type action_state

  (* new creates an action_state given a packet send function
     and a packet receive function and an "act" function.

     When data comes in or a timer expires, the appropriate action
     will be queued on the to_do list. When a timer expires, the
   *)

  val new: {act: unit -> unit, to_do: to_do_list, address: address,
	    peer_checksum: send_packet -> Word16.word}
         -> action_state

  (* send_segment accepts an uninitialized segment to be sent
     and sends it, possibly adding actions to the to_do list. *)
  val send_segment: action_state * segment * (send_packet -> unit) -> unit

  (* process_packet accepts a raw packet from the network and computes
     the actions that it causes.  These actions are added to the
     to_do_list. *)
  val process_packet: action_state * receive_packet -> unit

  val identify: receive_packet -> {src:Word16.word, dest:Word16.word}

 end (* sig *)

