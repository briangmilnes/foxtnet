(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

---------------------------------------------------------------------
		i.	Abstract

	Timer definition. When a timer is started, the handler is
	called after the specified number of milliseconds have
	elapsed, unless the timer is cleared before the expiration.
	Clearing a timer after it has elapsed is harmless.

---------------------------------------------------------------------
		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

---------------------------------------------------------------------
		iii.	RCS Log
	
$Log: timer.sig,v $
Revision 1.3  1994/04/26  17:52:54  esb
name no longer required as a parameter to start.

Revision 1.2  94/02/08  14:55:14  esb
added a name parameter to "start", for debugging.

Revision 1.1  1993/12/07  02:46:00  esb
Initial revision


---------------------------------------------------------------------
		1.	signature TIMER
*)

signature TIMER =
 sig

  type T

  val start: (unit -> unit) * int -> T

  val clear: T -> unit

 end

