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

	A link file for the bytearray utilities module.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	


		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.8  1994/08/02  20:18:04  esb
added dynarray.* and compare*

Revision 1.7  1994/07/04  18:16:26  esb
added create.sig create.fun

Revision 1.6  94/06/27  17:11:21  robby
added access.tst

Revision 1.5  94/06/16  16:50:23  danwang
Updated for functorized Fox_Basis

Revision 1.4  1993/11/02  22:36:12  esb
split out test_arrayutil.

Revision 1.3  1993/10/29  04:44:26  esb
added the copy functions.

Revision 1.2  1993/09/13  22:16:57  cline
removed '#'s

Revision 1.1  1993/09/02  15:36:41  esb
Initial revision


		1.	util
*)

val arrayutil = map (fn x => "./util/bytearrays/" ^ x)
                    ["format.sig", "format.fun", "access.sig",
		     "access.fun","copy.sig", "copy.fun",
		     "create.sig", "create.fun",
		     "dynarray.sig", "dynarray.fun",
		     "comparearray.sig", "comparedyn.fun", "comparebyte.fun"]

val test_arrayutil = map (fn x => "./util/bytearrays/" ^ x)
                         ["copy.tst", "copy.tim", "access.tst",
			  "dynarray.tst"]
