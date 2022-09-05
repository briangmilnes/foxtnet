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

	A link file for the trivial filter module.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	


		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.6  1995/01/14  02:24:20  esb
fixed the filter, not tested yet.

Revision 1.5  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.4  1994/06/16  16:33:06  danwang
Updated for functorized Fox_Basis

Revision 1.3  1993/09/20  23:11:07  esb
restructured into code, test, and real.

Revision 1.2  1993/06/11  17:20:52  esb
made file names relative to root of source directory

Revision 1.1  1993/06/10  22:29:09  milnes
Initial revision


		1.	filter
*)

val filter = map (fn x => "./filter/" ^ x) ["filter.sig", "filter.fun"];

val test_filter = map (fn x => "./filter/" ^ x) ["filter.tst"];
