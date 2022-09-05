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

	A locative signature to allow read write access to 
     pieces of packets after allocation.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature LOCATIVE

		iii.	RCS Log
	
$Log: locative.sig,v $
Revision 1.1  1994/08/12  14:26:45  milnes
Initial revision


		1.	signature LOCATIVE
*)

signature LOCATIVE =
  sig
   type object
   type location
   type loc

   val loc  : location -> loc
   val !!   : loc -> object
   val ::== : loc * object -> unit
  end

