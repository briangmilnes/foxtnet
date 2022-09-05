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

	Test code for the event package.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	structure Test_Coroutine

		iii.	RCS Log
	
$Log: event.tst,v $
Revision 1.11  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.10  1995/06/20  16:44:48  esb
brought up to use the basis.

Revision 1.9  1995/03/10  03:44:58  esb
adapted to new vendor.sig.

Revision 1.8  1995/01/18  20:58:53  esb
separated fork and fork_limited_time.

Revision 1.7  1994/12/01  18:28:09  esb
added select.

Revision 1.6  1994/08/02  19:30:10  esb
adapted to uncurried test.sig.

Revision 1.5  1994/06/16  16:29:59  danwang
Updated to use functorized Fox_Basis

Revision 1.4  1994/04/26  17:49:23  esb
adapted to new event.sig and coro.sig interfaces.

Revision 1.3  93/09/13  22:06:45  cline
deleted '#'s from RCS log

Revision 1.2  1993/09/10  11:31:10  esb
made signal return the event if it found someone to signal.

Revision 1.1  1993/09/02  15:17:37  esb
Initial revision


		1. 		functor Test_Event

*)

functor Test_Event (structure B: FOX_BASIS
		    val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "event.tst"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print

  val expecting = ref ["a", "b", "c", "b", "c", "b", "a"]

  val test_count = ref 0

  fun check_result (event, count) =
       case ! expecting of
          [] =>
           (test_count := ! test_count + 1;
	    local_print ("expecting no more events, got event " ^ event);
	    false)
        | (h :: t) =>
           (expecting := t;
	    test_count := ! test_count + 1;
	    if ! test_count = count andalso h = event then
	     true
	    else
	     (local_print ("expecting event (" ^ h ^ "," ^
			   Integer.toString (! test_count) ^
			   "), got event (" ^
			   event ^ "," ^ Integer.toString count ^ ")");
	      false))

  fun test_signal (qab, qc) =
       let fun matcha "a" = true
	     | matcha _ = false
           fun matchb "b" = true
	     | matchb _ = false
           fun matchc _ = true
           fun signal (queue, match, n) () =
	        B.Event_Queue.signal {queue = queue, match = match,
				      value = n} <> NONE
       in B.Test.test ("signal 1", (signal (qab, matcha, 1)));
          B.Test.test ("signal 2", (signal (qab, matchb, 2)));
          B.Test.test ("signal 3", (signal (qc, matchc, 3)));
          B.Scheduler.sleep 100;
          B.Test.test ("signal 4", (signal (qab, matchb, 4)));
          B.Scheduler.sleep 100;
          B.Test.test ("signal 5", (signal (qc, matchc, 5)));
          B.Scheduler.sleep 100;
          B.Test.test ("signal 6", (signal (qab, matchb, 6)));
          B.Scheduler.sleep 100;
          B.Test.test ("signal 7", (signal (qab, matcha, 7)));
          B.Scheduler.sleep 100
       end

  fun test_wait (qab, qc) =
       let fun wait (queue, event) () =
             check_result (event,
		           B.Event_Queue.wait {queue = queue, event = event,
					       while_waiting = fn () => ()})
       in B.Scheduler.fork (fn () => B.Test.test ("wait 2", wait (qab, "b")));
          B.Scheduler.fork (fn () => B.Test.test ("wait 4", wait (qab, "b")));
          B.Scheduler.fork (fn () => B.Test.test ("wait 1", wait (qab, "a")));
          B.Scheduler.fork (fn () => B.Test.test ("wait 7", wait (qab, "a")));
          B.Scheduler.fork (fn () => B.Test.test ("wait 3", wait (qc, "c")));
          B.Scheduler.fork (fn () => B.Test.test ("wait 5", wait (qc, "c")));
          B.Scheduler.fork (fn () => B.Test.test ("wait 6", wait (qab, "b")));
          B.Scheduler.sleep 10000
       end

  fun run_tests () =
       let val qab = B.Event_Queue.new ()
           val qc = B.Event_Queue.new ()
       in (test_wait (qab, qc);
           test_signal (qab, qc))
       end  

  fun run () =
       (B.Scheduler.reset ();
	test_count := 0;
	expecting := ["a", "b", "c", "b", "c", "b", "a"];
        B.Test.tests ("event", 14, run_tests))

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
		1.	structure Test_Coroutine
*)

structure Test_Event = Test_Event (structure B = Fox_Basis
				   val debug_level = NONE)
