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

	Debug contains files that help control how the system is
	instantiated and run.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.7  1995/03/07  20:27:38  esb
eliminated debugtrace and printer.

Revision 1.6  1994/11/22  13:59:39  milnes
Added some more modules.

Revision 1.5  1994/11/10  22:02:23  esb
added printer.sig and printer.fun

Revision 1.4  1994/11/10  16:15:44  milnes
Added the new debugtrace files.

Revision 1.3  1994/06/16  16:31:31  danwang
Updated to use functorized Fox_Basis

Revision 1.2  1993/06/11  17:25:26  esb
made file names relative to root of source directory

Revision 1.1  1993/06/10  22:55:45  milnes
Initial revision


		1.	...
*)

val debug = map (fn x => "./debug/" ^ x) 
              ["debug.sig", "debug.fun", "trace.sig", "trace.fun"];


