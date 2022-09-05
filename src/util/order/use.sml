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
Revision 1.2  1994/06/16  16:53:09  danwang
Updated for functorized Fox_Basis

Revision 1.1  1994/02/18  09:32:05  esb
Initial revision


		1.	util
*)

val orderutil = map (fn x => "./util/order/" ^ x)
	         ["order.sig", "order.fun"]

val test_orderutil = map (fn x => "./util/order/" ^ x)
	         ["order.tst", "order.tim"]
