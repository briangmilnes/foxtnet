(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken.Cline@cs.cmu.edu)
        Nick Haines (nickh@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	proto.sig: base signature to be matched by all protocols
	in the TCP/IP stack.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	iv.	Overview
	1.	signature PROTOCOL
	2.	types
	3.	protocol functions
	4.	string conversion functions
	5.	exceptions

	iii.	RCS Log

$Log: proto.sig,v $
Revision 1.13  1995/03/12  17:58:17  esb
adapted to new trace.sig.

Revision 1.12  1994/09/27  19:26:18  milnes
Fixed a typo.

Revision 1.11  1994/08/12  06:18:51  esb
added hash*, equal*, and type allocation.

Revision 1.10  1994/08/02  20:36:53  esb
new, redesigned version, with start_passive and allocate_send.

Revision 1.9  1994/05/23  14:04:51  milnes
Added print functions.

Revision 1.8  1993/12/04  20:52:18  esb
added the connection parameter to the handler.

Revision 1.7  1993/10/13  17:32:33  esb
made incoming and outgoing message into eqtypes.

Revision 1.6  1993/09/19  15:24:06  esb
improved the comments.

Revision 1.5  1993/09/02  15:49:07  esb
many changes suggested by the code review and by more careful
review of the definition.

Revision 1.4  1993/06/21  14:05:03  milnes
Edo made address_pattern and connection eq types and made connection died's
address an option.

Revision 1.3  93/06/15  22:30:58  esb
modified passive_open so the handler is allowed to fail and
return NONE rather than a valid handler

Revision 1.2  1993/06/14  21:09:37  esb
added exception Packet_Size

Revision 1.1  1993/06/10  23:00:46  milnes
Initial revision


	iv.	Overview

proto.sig defines all the types and values (functions, exceptions)
that we expect every data exchange protocol in the foxnet to support;
protocols that are not used for the transmission of data will have
other interfaces.  proto.sig defines a connection-oriented interface
to a protocol, and we assume it is possible to impose the
connection-oriented model on every protocol.

There are protocol operations, connection operations, and data
operations.  Protocol operations (initialize and finalize) control
allocation of the protocol's resources.  Connection operations
(active_open, start_passive, stop_passive, close, abort) do the same
for connections.  Data operations (send) are used to send
data. There is no explicit receive function since data is received
when the handler specified in an "open" operation is called by the
protocol and given a packet of data.

Exceptions are raised when something unexpected (that is, off the main
execution path) is detected on a call to a function.  A function is
supposed to raise only those exceptions that are listed for it.
Exceptions all carry a string with a brief explanation (in English) of
the reason for raising the exception or with at least the name of the
source file which raised the exception.  When an unexpected event is
detected on a connection independently of function calls, the status
function for that connection is called.

	1.	signature PROTOCOL
*)

signature PROTOCOL =
 sig

(*
	2.	types
*)

  (* A value of type active identifies the set of peers (though usually
     just a single peer) to which the connection is to be set up.
     A passive address specifies the set of peers that the user is
     willing to accept connections from.  A connection is a handle
     used in calls to send, close, or abort.  An address identifies
     the actual peer(s) on a connection. *)

  type address
  type connection

  (* Incoming and outgoing specify the format of the data provided to
     the data handler and to send.  Status messages specify changes in
     the state of a connection.  An allocation describes a packet to
     be allocated; this is usually just an integer giving the desired size. *)

  type incoming
  type outgoing
  type status

  type allocation

  (* a handler is a pair of functions.  At connection creation time,
     the functions are given the connection.  The remaining parameter
     is supplied when data is received on the connection or the status
     of the connection changes. *)

  datatype handler = Handler of connection
                                -> ((incoming -> unit) * (status -> unit))


(*
	3.	protocol functions
*)

  (* Since protocols may allocate systems resources which cannot be
     released by the garbage collector, protocols must be finalized
     after their last use; last use includes the use of any function
     from this protocol.  Both functions return the number of
     outstanding initializations, i.e. the number of times initialize
     has been called that has not been matched by a corresponding
     finalize. *)

  (* May raise Initialization_Failed. *)
  val initialize: unit -> int

  (* Does not raise exceptions. *)
  val finalize: unit -> int


  (* Open a connection to a specific peer at this address.
     May raise Protocol_Finalized, Illegal_Address, or Open_Failed. *)
  val connect: address * handler -> connection

  (* Passively listen for incoming connection requests whose address
     matches the pattern.  No more connections will be opened after
     stop_passive is called.  To limit the number of instantiations,
     this number can be specified in the call to start_passive.  *)

  (* May raise Protocol_Finalized or Illegal_Address. *)
  val start_passive: address * handler * int option
                   -> ( (* stop *) (unit -> unit) *
		        (* opened connections *) (unit -> connection list))

  (* allocate a new packet and a function to send it.  The function
     can be called many times to send the same packet;  the packet
     may be modified at any time that the function is not executing. *)
  (* May raise Protocol_Finalized or Connection_Closed or Send_Failed. *)
  val allocate_send: connection * allocation -> (outgoing * (unit -> unit))


  (* Close is the normal way to free a connection.  Abort will free
     the connection without necessarily waiting for the remote peer
     to consent to this closing.  Connections which are not freed
     will be freed when the protocol is finalized.
     Both functions may raise Protocol_Finalized or Connection_Closed. *)
  val close: connection -> unit

  val abort: connection -> unit


(*
	4.	conversion functions
*)

  (* Each protocol provides makestring functions for objects of
     each of the types specific to the protocol, except abstract
     types and function types. *)
  val makestring_address: address -> string 
  (* makestring for the data objects takes a parameter indicating
     the maximum number of data bytes to be printed.  If the
     parameter is NONE, all data bytes are printed. *)
  val makestring_incoming: incoming * int option -> string
  val makestring_outgoing: outgoing * int option -> string
  val makestring_status: status -> string

  (* hash returns an integer that encodes as much information as possible
     about the value it is given, though different addresses/connections
     may share the same hash value. *)
  val hash_address: address -> int
  val hash_connection: connection -> int
  val equal_address: address * address -> bool
  val equal_connection: connection * connection -> bool

(*
	5.	exceptions
*)

  (* Raised when a call to initialize fails. *)
  exception Initialization_Failed of string
  (* Raised when a function other than initialize is called and
     the corresponding protocol has been finalized. *)
  exception Protocol_Finalized
  (* Raised when a function such as send, close, or abort
     is called and the corresponding connection has been closed. *)
  exception Connection_Closed of connection * string
  (* Raised when an address turns out to be somehow illegal. *)
  exception Illegal_Address of string
  (* Raised when an open fails and no other exception applies. *)
  exception Open_Failed of string
  (* Raised on a call to send when the call is somehow illegal. *)
  exception Send_Failed of string

 end (* sig *)
