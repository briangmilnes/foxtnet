(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	A little tool for accessing the environment variables.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature UNIX_ENVIRONMENT

		iii.	RCS Log
	
$Log: environ.sig,v $
Revision 1.1  1994/10/14  03:10:38  milnes
Initial revision

		1.	signature UNIX_ENVIRONMENT
*)

signature UNIX_ENVIRONMENT =
 sig
  val find : string -> string option
 end



