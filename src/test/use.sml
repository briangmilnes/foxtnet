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

	A link file for the trivial test module.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	


		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.4  1994/06/16  16:48:59  danwang
Updated for functorized Fox_Basis

Revision 1.3  1993/09/02  15:27:01  esb
replaced test_functor.sig by teststructure.sig.

Revision 1.2  1993/06/11  17:20:52  esb
made file names relative to root of source directory

Revision 1.1  1993/06/10  22:31:39  milnes
Initial revision


		1.	test
*)

val test = map (fn x => "./test/" ^ x) 
            ["test.sig", "test.fun", 
	     "teststructure.sig"]













