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

	Basis contains files that group all the constructors from
	the basis into a single place.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.4  1994/06/16  16:07:04  danwang
Updated to use functorized Fox_Basis

Revision 1.3  1993/09/17  16:52:12  milnes
*** empty log message ***

Revision 1.2  1993/09/13  22:08:06  cline
deleted '#'s from RCS log

Revision 1.1  1993/08/27  20:32:46  esb
Initial revision


		1.	...
*)

val basis = map (fn x => "./basis/" ^ x) ["basis.sig", "basis.fun","basis.str"];


