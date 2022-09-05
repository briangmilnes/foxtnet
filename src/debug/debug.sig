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

             A TEST signature that controls the installation of test

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DEBUG

		iii.	RCS Log
	
$Log: debug.sig,v $
Revision 1.1  1993/06/10  22:55:45  milnes
Initial revision


		1.	signature DEBUG
*)

signature DEBUG =
 sig
  val include_tests : bool;
  val do_tests : bool ref;

  val include_prints : bool;
  val do_prints : bool ref;

  val include_timings : bool;
  val do_timings : bool ref;
 end
  



