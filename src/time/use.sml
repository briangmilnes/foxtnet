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

	A link files for the time stuff.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.5  1994/06/16  16:49:32  danwang
Updated for functorized Fox_Basis

Revision 1.4  1993/12/07  02:44:44  esb
added timer.{sig,fun}

Revision 1.3  1993/07/10  04:01:51  esb
brought in the timing functions that used to be in util

Revision 1.2  1993/06/11  17:25:26  esb
made file names relative to root of source directory

Revision 1.1  1993/06/10  22:53:52  milnes
Initial revision


		1.	...
*)

val time = map (fn x => "./time/" ^ x)
                ["timeutil.sig", "timeutil.fun", 
		 "time.sig", "time.fun", 
		 "timer.sig", "timer.fun",
		 "timing.sig", "timing.fun", 
		 "stopwatch.sig", "stopwatch.fun"]


