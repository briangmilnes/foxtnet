(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This is a pair of signatures for remote procedure calls.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature RPC_CALL
	2.	signature RPC_SERVER

		iii.	RCS Log
	
$Log: rpc.sig,v $
Revision 1.5  1994/07/13  19:14:54  robby
added an error datatype to facilitate exception handling

Revision 1.4  94/07/07  17:00:43  robby
updated to use with EXTERN

Revision 1.3  94/06/29  16:25:32  robby
*** empty log message ***

Revision 1.2  94/06/26  15:51:38  robby
can't use handle as a type.

Revision 1.1  94/06/25  20:41:55  robby
Initial revision

		1.	signature RPC_CALL
*)

signature RPC_CALL=
sig
  type address
  structure Arg : EXTERN
  structure Res : EXTERN

  val call : address -> Arg.T -> Res.T
  exception RPC_Not_Initialized of string
  exception Packet_Size of int

  type error
  exception Call_Failed of error * string

  exception Bad_Address of address * string

  type info
  type control
  val query : unit -> info
  val control : control -> unit

  val initialize : unit -> int
  exception Initialization_Failed of string

  val finalize : unit -> int
end

(*
		2.	signature RPC_SERVER
*)

signature RPC_SERVER =
sig
  type server_handle
  type address
  structure Arg : EXTERN
  structure Res : EXTERN
  type address_pattern 

  val install : address_pattern -> (address * Arg.T -> Res.T) ->
                server_handle
  val uninstall : server_handle -> unit

  exception Bad_Request of address
  exception Kill_Error

  exception RPC_Not_Intialized of string

  val initialize : unit -> int
  val finalize : unit -> int
end
