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

	A link file for the trivial eth module.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	


		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.10  1994/06/05  21:28:13  robby
added buildeth.sig

Revision 1.9  94/04/27  00:00:27  esb
took eth.tim out of real_eth.

Revision 1.8  93/10/25  17:28:08  milnes
Added buildeth.fun.

Revision 1.7  1993/10/12  22:34:10  esb
added time_eth.

Revision 1.6  1993/10/09  18:27:46  esb
changed "time_" back to "real_", since some of the files included
under this name measure functionality but not timing or performance.

Revision 1.5  1993/10/06  12:12:13  milnes
Simplified variables.

Revision 1.4  1993/09/20  23:11:42  esb
restructured into code, test, and real.

Revision 1.3  1993/09/10  18:27:34  milnes
Added prot.tim.

Revision 1.2  1993/06/11  17:25:26  esb
made file names relative to root of source directory

Revision 1.1  1993/06/10  23:06:53  milnes
Initial revision


		1.	eth
*)

val eth = map (fn x => "./prot/eth/" ^ x) 
  ["eth.sig", "eth.fun", "buildeth.sig" ,"buildeth.fun"]

val test_eth = map (fn x => "./prot/eth/" ^ x) ["eth.tst"]

val real_eth = map (fn x => "./prot/eth/" ^ x) []

val time_eth = map (fn x => "./prot/eth/" ^ x) ["eth.tim"]
