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

	A link file for the trivial control module.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	


		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.6  1994/06/16  16:29:59  danwang
Updated to use functorized Fox_Basis

Revision 1.5  1994/02/08  16:41:24  esb
added coro.tim.

Revision 1.4  93/09/20  23:11:14  esb
restructured into code, test, and real.

Revision 1.3  1993/09/02  15:19:46  esb
added event and pipe, removed the misnamed queue.

Revision 1.2  1993/06/11  17:25:26  esb
made file names relative to root of source directory

Revision 1.1  1993/06/10  22:35:17  milnes
Initial revision


		1.	control
*)

val control = map (fn x => "./control/" ^ x)
		["coro.sig", "coro.fun",
                 "event.sig", "event.fun",
                 "pipe.sig", "pipe.fun",
                 "dispatcher.sig", "dispatcher.fun"];

val test_control = map (fn x => "./control/" ^ x)
		["coro.tst", "coro.tim", "event.tst", "pipe.tst"];




