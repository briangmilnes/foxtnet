(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpaction.sig: executing the actions specified by
	the TCP state machine. The actions must be those
	defined in tcptcb.sig.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_ACTION

---------------------------------------------------------------------

	iii.	RCS Log

$Log: tcpaction.sig,v $
Revision 1.6  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.5  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.4  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.3  1994/05/10  08:04:51  esb
removed the receive function, which did not belong.

Revision 1.2  94/01/28  01:19:11  esb
minor change.

Revision 1.1  1994/01/09  03:24:15  esb
Initial revision


---------------------------------------------------------------------
	1.	signature TCP_ACTION
*)

signature TCP_ACTION =
 sig

  type segment				(* data accepted from TCP *)
  type send_packet			(* data sent on lower layer *)
  type receive_packet			(* data received from lower layer *)
  type to_do_list
  datatype address = Address of {local_port: FoxWord16.word,
				 remote_port: FoxWord16.word,
				 peer_checksum: FoxWord16.word}

  type action_state

  (* new creates an action_state given a packet send function
     and a packet receive function and an "act" function.

     When data comes in or a timer expires, the appropriate action
     will be queued on the to_do list. When a timer expires, the
   *)

  val new: {allocate: int -> (send_packet * (unit -> unit)),
	    act: unit -> unit, to_do: to_do_list, address: address}
         -> action_state

  (* allocate_segment accepts a data size and returns an initialized
     send_packet and a function that, when given a segment,
     sends the packet after adding information from the segment. *)
  val allocate_segment: action_state * int -> (send_packet * (segment -> unit))

  (* send_segment accepts an uninitialized segment to be sent
     and sends it, possibly adding actions to the to_do list. *)
  val send_segment: action_state * segment -> unit

  (* process_packet accepts a raw packet from the network, with the IP
     pseudo-checksum, and computes the actions that it causes.  These
     actions are added to the to_do_list. The minimal TCP header is
     also given. *)
  val process_packet: action_state * receive_packet
                    * FoxWord16.word * ByteArray.bytearray
                    -> unit

 end (* sig *)

