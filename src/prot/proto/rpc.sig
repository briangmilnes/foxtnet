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

	rpc.sig: base signature to be matched by all RPC protocols
	in the TCP/IP stack.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature RPC

	iii.	RCS Log

$Log: rpc.sig,v $
Revision 1.1  1995/06/20  17:09:33  esb
Initial revision


	1.	signature RPC_PROTOCOL
*)

signature RPC_PROTOCOL =
 sig
  structure Setup: KEY

  structure Address: KEY
  structure Pattern: KEY
  structure Connection_Key: KEY

  structure Incoming: EXTERNAL
  structure Outgoing: EXTERNAL
  structure Status: PRINTABLE
  structure Count: COUNT

  structure X: RPC_EXCEPTIONS

  type session

  datatype 'a input_status = Incomplete
                           | Complete of 'a
                           | Left_Over of 'a * Incoming.T list

  val rpc: session * Address.T -> (Incoming.T list -> 'a input_status)
         -> Outgoing.T list -> 'a

  val serve: session * Pattern.T
           * (Incoming.T list -> Outgoing.T list input_status) * Count.T
           -> {stop: unit -> unit}

  val session: Setup.T * (session -> 'a) -> 'a
 end
