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

	Module implementing timers. When a timer is started, the
	handler is called after the specified number of milliseconds have
	elapsed, unless the timer is cleared before the expiration.

---------------------------------------------------------------------
		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Timer
	2.	type T
	3.	function start
	4.	function clear

---------------------------------------------------------------------
		iii.	RCS Log
	
$Log: timer.fun,v $
Revision 1.5  1995/01/18  21:05:38  esb
adapted to new COROUTINE signature.

Revision 1.4  1994/04/26  17:53:13  esb
name no longer required as a parameter to start.

Revision 1.3  94/02/08  14:56:38  esb
added some debugging code.

Revision 1.2  1994/01/12  21:44:25  esb
name change.

Revision 1.1  1993/12/07  02:46:00  esb
Initial revision


---------------------------------------------------------------------
		1.	functor Timer
*)

functor Timer (structure Scheduler: COROUTINE) =
 struct

(*
---------------------------------------------------------------------
		2.	type T

	The type T is true if the timer is still valid, and false
	if the timer has expired.
*)

  type T = bool ref

(*
---------------------------------------------------------------------
		3.	function start
*)

  local
   fun sleep (valid, handler, ms) () =
        (Scheduler.sleep ms;
	 if ! valid then handler () else ())

  in
   fun start (handler, ms) =
        let val state = ref true
	in Scheduler.fork (sleep (state, handler, ms));
	   state
	end
  end (* local *)

(*
---------------------------------------------------------------------
		4.	function clear
*)

  fun clear valid = valid := false

 end	(* struct *)

