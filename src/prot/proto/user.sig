(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A user protocol signature that supports connection oriented
   and connectionless protocols.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	iv.	Overview
	1.	signature USER_PROTOCOL_TYPES
	2.	signature CONNECTION_ORIENTED
        3.	signature CONNECTIONLESS
        4.      signature USER2

		iii.	RCS Log
	
$Log: user.sig,v $
Revision 1.1  1995/06/20  17:08:23  esb
Initial revision

Revision 1.7  1994/08/12  06:19:48  esb
added type allocation.

Revision 1.6  1994/08/02  20:36:53  esb
adapted to new protocol signature.

Revision 1.5  1993/09/19  15:24:06  esb
improved the comments.

Revision 1.4  1993/09/02  15:49:35  esb
changes to go along with the changes in proto.sig version 1.5.

Revision 1.3  1993/07/02  21:24:23  esb
put control and query into the USER signature, where they belong

Revision 1.2  1993/06/21  02:59:03  esb
many changes due to actually trying to write a user-level protocol module

Revision 1.1  1993/06/10  23:00:46  milnes
Initial revision


		iv.	Overview


The protocols of the ARPA Internet TCP/IP suite are either
connection-oriented or connectionless.  With this we mean that the
protocol either establishes a connection with its peer before
communication commences or it does not.  Reliability of the
communication is an orthogonal issue.  This file provides signatures
for user interfaces to protocols; e.g., signatures that user code will
use to access one or more protocols.

The BSD UNIX system provides connection-oriented and connectionless
interfaces to both UDP and TCP.  We provide a signature USER that
contains signatures for connectionless and connection oriented access
to a protocol.  The individual protocols must support their natural
interaction style, but may also mimic the other style of communcation.
For example, UDP is naturally connectionless but may implement the
connection oriented style of communication by simply ignoring calls to
open, close, and abort.

*)

(*
	1.	signature USER_PROTOCOL_TYPES

Construct a signature of the types and exceptions used in user
interfaces to protocols. This lets us share types and exceptions
between the connection-oriented and the connectionless protocol
modules.

*)

signature USER_PROTOCOL_TYPES =
 sig
  (* An address identifies the set of peers (though usually
     just a single peer) with which we want to communicate. *)

  type address

  (* incoming and outgoing are the types of data sent and received. *)

  type incoming
  type outgoing

  (* description for packet allocation. *)
  type allocation

  (* Raised wnen a call to initialize fails. *)
  exception Initialization_Failed of string

  (* Raised when an address turns out to be somehow illegal. *)
  exception Illegal_Address of string

  (* Raised when a function other than initialize is called and
     the corresponding protocol has been finalized. *)
  exception Protocol_Finalized

  (* Raised on a call to allocate_send. *)
  exception Send_Failed of string

 end (* sig *)

(*

	2.	signature CONNECTION_ORIENTED
*)

signature CONNECTION_ORIENTED =
 sig

  (* Include a copy of the user types. *)

  structure UPT: USER_PROTOCOL_TYPES

  (* A connection oriented protocol establishes a connection first, and
     then the user sends and receives data over this connection. *)
  type connection

  (* Sometimes opens do not work because the network is down or the
     peer is not responding, or for some other reason; the Open_Failed
     exception then returns the address and a diagnostic string. *)
  exception Open_Failed of string

  (* Actively open a connection to a specific peer at this
     address and return the connection.
     May raise Protocol_Finalized, Open_Failed, or Bad_Address. *)
  val connect: UPT.address -> connection

  (* Passively wait for an incoming connection whose address matches
     the pattern. When the connection request arrives, return a
     connection to it and the address for the connection.
     May raise Protocol_Finalized, Open_Failed, or Bad_Address. *)
  val passive_open: UPT.address -> connection

  (* Raised when a function such as allocate_send, close, or abort
     is called and the corresponding connection has been closed. *)
  exception Connection_Closed of connection * string

  (* The normal way to cease communication.
     May raise Protocol_Finalized or Connection_Closed. *)
  val close: connection -> unit

  (* Abort communcation, without neccesarily waiting for the
     remote peer to agree.
     May raise Protocol_Finalized or Connection_Closed. *)
  val abort: connection -> unit

  (* Send a message along a connection.
     May raise Protocol_Finalized, Connection_Closed, or Send_Failed. *)
  val allocate_send: connection * UPT.allocation
                   -> (UPT.outgoing * (unit -> unit))

  (* Receive a message from a connection: a blocking read.
     May raise Protocol_Finalized or Connection_Closed. *)
  val receive: connection -> UPT.incoming

end (* sig *)

(*
        3.	signature CONNECTIONLESS
*)

signature CONNECTIONLESS =
 sig

  (* Include a copy of the user types. *)
  structure UPT: USER_PROTOCOL_TYPES

  type peer

  (* Send a message to the remote peer.
     May raise Protocol_Finalized or Bad_Address or Send_Failed. *)
  val allocate_send: UPT.address * UPT.allocation
                   -> (UPT.outgoing * (unit -> unit))

  (* Receive a message from a remote address; a blocking read.
     May raise Protocol_Finalized or Bad_Address. *)
  val receive: UPT.address -> (peer * UPT.incoming)

 end (* sig *)

(*
        4.      signature USER
*)

signature USER_PROTOCOL =
 sig
  structure CO: CONNECTION_ORIENTED
  structure CL: CONNECTIONLESS
  (* The intent here is to share all of the basic types
     and all the exceptions between CO and CL. *)
  sharing CO.UPT = CL.UPT

  (* Since protocols may allocate systems resources which cannot be
     released by the garbage collector, protocols must be shut down
     after their last use; last use includes the use of any function
     that depends, directly or indirectly, on this protocol, including
     any functions that take as parameters connections created using
     this protocol. *)

  (* May raise Initialization_Failed. *)
  val initialize: unit -> int

  (* May raise Protocol_Finalized. *)
  val finalize: unit -> int


 end (* sig *)


