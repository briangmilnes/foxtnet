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
	2.	sub-structures
	3.	extension types
	4.	connection type
	5.	listen type
	6.	handler type
	7.	session type

	iii.	RCS Log

$Log: proto.sig,v $
Revision 1.1  1995/06/20  17:09:33  esb
Initial revision


	iv.	Overview

proto.sig defines all the types and values (functions, exceptions)
that we expect every data exchange protocol in the foxnet to support;
protocols that are not used for the transmission of data will have
other interfaces.  proto.sig defines a connection-oriented interface
to a protocol, and we assume it is possible to impose the
connection-oriented model on every protocol.

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
	2.	sub-structures

	A Setup.T has information needed to create a session.
	An Address.T specifies one or more peers for connections.
	A Pattern.T specifies one or more peers from which connections
	will be accepted.
	A Connection_Key.T is specifies the peer(s) in a connection.
	An Incoming.T carries incoming data.
	An Outgoing.T carries outgoing data.
	A Status.T is a control message from the protocol.
*)

  structure Setup: KEY

  structure Address: KEY
  structure Pattern: KEY
  structure Connection_Key: KEY

  structure Incoming: EXTERNAL
  structure Outgoing: EXTERNAL
  structure Status: PRINTABLE
  structure Count: COUNT

  structure X: PROTOCOL_EXCEPTIONS

  exception Already_Open of Connection_Key.T

(*
	3.	extension types

	These types are provided so a specific protocol signature may
	instantiate them as appropriate.
*)

  type connection_extension
  type listen_extension
  type session_extension

(*
	4.	connection type

	A "connection" is the capability to send data and to close
	the underlying connection.  The abort operation does NOT
	terminate the connection handler, which however becomes
	unable to send.
*)

  datatype connection = C of {send: Outgoing.T -> unit,
			      abort: unit -> unit,
			      extension: connection_extension}

(*
	5.	listen type

	A "listen" type is the capability to stop listening.
*)

  datatype listen = L of {stop: unit -> unit, extension: listen_extension}

(*
	6.	handler type

	A handler function is applied to a value of type Connection_Key.T
	every time a connection is instantiated either by connect or by
	listen.  The connection will remain open until the first of:
	- the connection_handler function completes or raises an exception
	- the abort function is called.

	While the connection is open, any one of the connection_handler,
	data_handler, or status_handler functions may call send to transmit
	data.  The protocol calls the connection_handler once per connection,
	the data_handler when data is received, and the status_handler when
	control messages must be delivered.
*)

  datatype handler = H of Connection_Key.T
                  -> {connection_handler: connection -> unit,
	              data_handler: connection * Incoming.T -> unit,
	              status_handler: connection * Status.T -> unit}

(*
	7.	session type

	In analogy with a connection, a session is active only until
	the session function completes or raises an exception.
	Connect and listen should only be called while the session is
	active.
*)

  datatype session = S of {connect: Address.T * handler -> unit,
			   listen: Pattern.T * handler * Count.T -> listen,
			   extension: session_extension}

  val session: Setup.T * (session -> 'a) -> 'a

 end (* sig *)
