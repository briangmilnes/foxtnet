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

	A use file for the locative structures.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	val locs

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.1  1994/08/12  14:26:45  milnes
Initial revision


		1.	val locatives
*)

use "./locative/dynarray/use.sml";

val locs = (["./locative/locative.sig","./locative/locative.fun"] @ dynlocs)

