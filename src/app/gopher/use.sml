(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.2  1995/02/04  20:39:01  robby
updated to 107

# Revision 1.1  1994/08/28  21:59:19  robby
# Initial revision
#

		1.	...
*)
val gopher = map (fn x=>"app/gopher/"^x) 
  ["util.fun","hotlist.sig","gopher_rpc.sig","gopher.sig",
   "hotlist.fun","gopher_rpc.fun","gopher.fun"]

