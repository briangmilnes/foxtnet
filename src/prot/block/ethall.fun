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

	ethall.fun: Assembling the standard Ethernet protocol.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Eth_All
	2.	structure Eth_Address
	3.	structure Eth_Type
	4.	structure Eth_Length

	iii.	RCS Log

$Log: ethall.fun,v $
Revision 1.3  1996/01/19  23:06:29  esb
adapted to the new wordarray signature.

Revision 1.2  1995/08/08  18:28:04  esb
adapted to new external functors.

Revision 1.1  1995/06/20  17:16:27  esb
Initial revision


	1.	functor Eth_All
*)

functor Eth_All (structure Lower: PROTOCOL
		 structure B: FOX_BASIS): PROTOCOL =
 struct

(*
	2.	structure Eth_Address
*)

  fun cursor _ = 0w0
  structure Eth_Address_Key =
      Protocol_Extern48_Big (structure In = Lower.Incoming
			     structure Out = Lower.Outgoing
			     structure B = B)
  structure Eth_Address = Address (structure Lower = Lower
				   structure Address = Eth_Address_Key
				   val source_precedes_dest = false
				   val cursor_in = cursor
				   val cursor_out = cursor
				   structure B = B)

(*
	3.	structure Eth_Type
*)

  structure Eth_Type_Key =
      Protocol_Extern16_Big (structure In = Lower.Incoming
			     structure Out = Lower.Outgoing
			     structure B = B)

  structure Eth_Type = Multiplex (structure Lower = Eth_Address
				  structure Selector = Eth_Type_Key
				  val cursor_in = cursor
				  val cursor_out = cursor
				  structure B = B
				  val name = "eth type")

(*
	4.	structure Eth_Length
*)

  structure Eth_Length = Min_Length (structure Lower = Eth_Type
				     val min_length = fn _ => 0w46
				     structure B = B)

  open Eth_Length
 end (* struct *)
