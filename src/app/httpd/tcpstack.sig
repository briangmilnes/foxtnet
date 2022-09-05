(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Chris Stone (Chris.Stone@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_STACK

		iii.	RCS Log
	
$Log: tcpstack.sig,v $
# Revision 1.1  1995/03/08  20:31:03  cstone
# Initial revision
#

		1.	signature TCP_STACK
*)




signature TCP_STACK =
sig
    structure Tcp : TCP_PROTOCOL

    val initialize : unit -> int
    val finalize   : unit -> int
end