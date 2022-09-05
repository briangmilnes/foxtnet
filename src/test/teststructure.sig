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
	
	This file provides a trivial test signature that all
	test structures must meet.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TEST_STRUCTURE

		iii.	RCS Log
	
$Log: teststructure.sig,v $
Revision 1.2  1993/09/13  21:42:15  cline
removed "#"s from log

Revision 1.1  1993/09/02  15:26:48  esb
Initial revision


		1.	signature TEST_STRUCTURE
*)

signature TEST_STRUCTURE = 
 sig
  val run : unit -> unit
 end


