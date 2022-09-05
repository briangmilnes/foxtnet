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

	A signature of a timing utility for performing interactive timings of code.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TIMEUTIL

		iii.	RCS Log
	
$Log: timeutil.sig,v $
Revision 1.2  1993/09/13  22:08:02  cline
deleted '#'s from RCS log

Revision 1.1  1993/07/10  04:03:18  esb
Initial revision

Revision 1.1  1993/06/10  22:41:20  milnes
Initial revision


*)

(*
		1.	signature TIMEUTIL
*)

signature TIMEUTIL =
 sig
   val timeapp : ('a -> 'b) -> 'a -> string
   val ntimes  : int -> ('a -> 'b) -> 'a -> 'b
 end


