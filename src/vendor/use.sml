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

	A signature to interface to the SML vendor's added functionality.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	val vendor

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.4  1994/06/16  16:53:40  danwang
Updated for functorized Fox_Basis

Revision 1.3  1993/06/18  17:23:59  esb
got rid of the vendor functor

Revision 1.2  1993/06/11  17:25:26  esb
made file names relative to root of source directory

Revision 1.1  1993/06/10  22:33:19  milnes
Initial revision


		1.	val vendor
*)


val vendor = map (fn x => "./vendor/" ^ x) ["vendor.sig", "vendor.fun"]


