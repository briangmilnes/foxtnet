(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Test code for the coroutine package.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Coroutine
	2.	structure Test_Coroutine

		iii.	RCS Log
	
$Log: coro.tst,v $
Revision 1.15  1996/04/30  20:20:36  esb
made work again.

Revision 1.14  1995/06/20  16:44:13  esb
minor changes.

Revision 1.13  1995/03/10  03:44:58  esb
adapted to new vendor.sig.

Revision 1.12  1995/01/18  20:58:53  esb
separated fork and fork_limited_time.

Revision 1.11  1994/08/02  19:30:10  esb
adapted to uncurried test.sig.

Revision 1.10  1994/06/16  16:29:59  danwang
Updated to use functorized Fox_Basis

Revision 1.9  1994/04/28  13:19:01  esb
fixed a minor inaccuracy.

Revision 1.8  94/04/26  17:47:32  esb
added suspend and resume, recoded, added timing calls.

Revision 1.7  94/01/17  17:57:39  esb
changed to fork_limited.

Revision 1.6  1994/01/13  16:21:47  milnes
Added a kill to coroutines, it required priority queue and deq deletes.

Revision 1.5  93/10/21  16:39:26  esb
added a sleep statement to let everyone terminate before recording result.

Revision 1.4  1993/09/02  15:16:09  esb
changed test_functor to test_structure.

Revision 1.3  1993/07/13  02:19:08  esb
got rid of an obsolete variable definition

Revision 1.2  1993/07/07  15:58:14  esb
cleaned up a little

Revision 1.1  1993/06/10  22:35:17  milnes
Initial revision


		1.	functor Test_Coroutine
*)

functor Test_Coroutine (structure B: FOX_BASIS
                        val debug_level: int ref option): TEST_STRUCTURE =
 struct

  val now = B.V.Time.now

  fun run_tests () = 
       let fun reset () = (B.Scheduler.reset (); true);
           fun check_time_on_sleep () =
	        (B.Scheduler.reset (); 
		 let val was = now ()
		 in B.Scheduler.sleep 1000;
		    not (B.V.Time.< (now (), B.V.Time.addms (was, 1000)))
		 end)
	   fun record_run run _ = run := true
	   fun nothing_runs_after_reset () =
	        (B.Scheduler.reset (); 
		 let val done = ref false
		 in B.Scheduler.fork (record_run done);
		    B.Scheduler.reset (); 
		    B.Scheduler.sleep 100;
		    not (! done)
		 end)
	   fun thread_run () =
	        (B.Scheduler.reset (); 
		 let val done = ref false
		 in B.Scheduler.fork (record_run done);
		    B.Scheduler.sleep 100;
		    ! done
		 end)
	   fun limited_thread_run () =
	        (B.Scheduler.reset (); 
		 let val done = ref false
		 in B.Scheduler.fork_limited_time (record_run done);
		    B.Scheduler.sleep 100;
		    ! done
		 end)
       in B.Test.test ("reset", reset);
	  B.Test.test ("check time on sleep", check_time_on_sleep);
	  B.Test.test ("nothing runs after reset", nothing_runs_after_reset);
	  B.Test.test ("a thread runs", thread_run);
	  B.Test.test ("a limited thread runs", limited_thread_run)
       end

  fun run () = B.Test.tests ("Coroutine", 5, run_tests)

  val _ = if ! B.Debug.do_tests then run () else ()

 end 

(*
		2.	structure Test_Coroutine
*)

structure Test_Coroutine = Test_Coroutine (structure B = Fox_Basis
                                           val debug_level = NONE)
 
