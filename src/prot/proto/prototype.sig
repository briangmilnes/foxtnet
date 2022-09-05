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
	1.	signature COUNT

	iii.	RCS Log

$Log: prototype.sig,v $
Revision 1.2  1995/06/27  14:48:11  cline
updated table of contents

Revision 1.1  1995/06/20  17:09:33  esb
Initial revision


	1.	signature COUNT

	Unlimited means any number; Maximum pre-specifies the maximum
	number; and Incremental must return Continue until it is time
	to stop, when it must return Done.
*)

signature COUNT =
 sig
  datatype continue = Continue | Done
  datatype T = Unlimited | Maximum of int | Incremental of unit -> continue
  val makestring_continue: continue -> string
  val makestring: T -> string
 end (* sig *)

