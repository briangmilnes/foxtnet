(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken.Cline@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	protoexn.sig: protocol exceptions and exception handling procedures.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature PROTOCOL_EXCEPTIONS
	2.	exceptions
	3.	print functions

	iii.	RCS Log

$Log: protoexn.sig,v $
Revision 1.2  1995/08/29  14:10:51  esb
added exception Receive.

Revision 1.1  1995/06/20  17:09:33  esb
Initial revision


	1.	signature PROTOCOL_EXCEPTIONS
*)

signature PROTOCOL_EXCEPTIONS =
 sig

(*
	2.	exceptions
*)

  exception Session of string		(* failure in session *)
  exception Listen of string		(* failure in listen or stop *)
  exception Connection of string	(* failure in connect or abort *)
  exception Send of string		(* failure in send *)
  exception Receive of string		(* failure in packet delivery *)

(*
	3.	makestring

	Return the exception name and the string, if the exception
	is one defined in this module, otherwise NONE.
*)

  val makestring: exn -> string option

 end (* sig *)

(*
	1.	signature RPC_EXCEPTIONS
*)

signature RPC_EXCEPTIONS =
 sig

(*
	2.	exceptions
*)

  exception Session of string		(* failure in session *)
  exception Serve of string		(* failure in serve or stop *)
  exception Rpc of string		(* failure in rpc *)

(*
	3.	makestring

	Return the exception name and the string, if the exception
	is one defined in this module, otherwise NONE.
*)

  val makestring: exn -> string option

 end (* sig *)

