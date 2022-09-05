(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This is a signature for the pairing functor, specializing
	LINEARIZE and providing first and second as destructors of
	the type T and pair as a constructor.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: pair.sig,v $
Revision 1.1  1994/07/14  20:28:46  robby
Initial revision

Revision 1.1  94/07/13  18:49:07  robby
Initial revision


		1.	signature Pair
*)
signature PAIR_LINEARIZE=
sig
  include LINEARIZE

  structure P1:LINEARIZE
  structure P2:LINEARIZE

  val first : T -> P1.T
  val second : T -> P2.T
  val pair : P1.T*P2.T -> T
end
