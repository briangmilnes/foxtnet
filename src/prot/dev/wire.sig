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

	This file provides a signature for emulating a network wire; e.g., a 
  passive multiplexing delivery mechanism.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature WIRE

		iii.	RCS Log
	
$Log: wire.sig,v $
Revision 1.2  1995/06/20  16:48:58  esb
converted to new protocol signature.

Revision 1.1  1993/06/10  23:05:01  milnes
Initial revision


		1.	signature WIRE
*)

signature WIRE = 
 sig
(* The handler is called for both the specified address and the all-ones
   broadcast address. false is returned if there already is a handler
   for this address. *)
  val register: Word_Array.T * (Word_Array.T -> unit) -> bool

  val unregister: Word_Array.T -> bool

  val send: Word_Array.T -> unit

 end (* sig *)


