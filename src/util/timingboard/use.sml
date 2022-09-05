(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

         A load list for timingboard.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS log 
	1.	val timingboard

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.3  1994/06/16  16:52:21  danwang
Updated for functorized Fox_Basis

Revision 1.2  1994/01/17  21:41:45  esb
made neater.

Revision 1.1  1994/01/14  12:27:46  milnes
Initial revision

		1.	val timingboard
*)

val timingboard = (map (fn x => "./util/timingboard/" ^ x)
		   ["timingboard.sig", "timingboard.fun"])

val test_timingboard = (map (fn x => "./util/timingboard/" ^ x) [])

