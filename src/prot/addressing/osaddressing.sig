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
	
	A signature for gathering hostname/ip number/gateway ip from
     the operating system.
     
      


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature OS_ADDRESSING 

		iii.	RCS Log
	
$Log: osaddressing.sig,v $
Revision 1.1  1994/10/19  23:18:06  milnes
Initial revision

		1.	signature OS_ADDRESSING 
*)

signature OS_ADDRESSING =
 sig
  include ADDRESSING_TYPES
  val get_addressing : unit -> host
 end


