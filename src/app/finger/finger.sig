(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Robert Findler (Robert.Finder@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature FINGER

		iii.	RCS Log
	
$Log: finger.sig,v $
Revision 1.2  1996/09/17  15:54:14  cline
rewrite

Revision 1.1  1994/08/28  18:55:20  robby
Initial revision


		1.	signature FINGER
*)
signature FINGER=
sig
  val finger: string -> string option
end
