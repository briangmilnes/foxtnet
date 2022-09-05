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

	pipe.tst: This file contains a test functor and structure
	for the synchronized data pipe functor Data_Pipe.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Pipe
	2.	structure Test_Pipe

		iii.	RCS Log
	
$Log: pipe.tst,v $
Revision 1.16  1996/07/05  17:23:22  esb
added a test case that broke one implementation.

Revision 1.15  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.14  1995/06/20  16:45:12  esb
changed new to take a unit parameter instead of an int option.

Revision 1.13  1995/03/12  16:23:06  esb
adapted debug_print to new interface.

Revision 1.12  1995/03/10  03:44:58  esb
adapted to new vendor.sig.

Revision 1.11  1995/01/18  20:58:53  esb
separated fork and fork_limited_time.

Revision 1.10  1994/12/01  23:32:47  esb
tested select and made it work.

Revision 1.9  1994/12/01  18:28:09  esb
added select.

Revision 1.8  1994/08/02  19:30:10  esb
adapted to uncurried test.sig.

Revision 1.7  1994/06/16  16:29:59  danwang
Updated to use functorized Fox_Basis

Revision 1.6  1994/04/26  17:50:27  esb
adapted to new COROUTINE and EVENT signatures.

Revision 1.5  94/01/17  17:59:04  esb
interface change

Revision 1.4  1993/10/14  18:17:51  milnes
Used implicit sequencing in let bodies.

Revision 1.3  1993/10/12  22:48:30  esb
added requeue and made T an eqtype. Also made the parameter to new optional.

Revision 1.2  1993/09/13  22:06:48  cline
deleted '#'s from RCS log

Revision 1.1  1993/09/02  15:18:40  esb
Initial revision


		1.	functor Test_Pipe

*)

functor Test_Pipe (structure B: FOX_BASIS
		   val debug_level: int ref option): TEST_STRUCTURE = 
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "pipe.tst"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print

   (* the test proceeds as follows: There is a number of series (sets)
      of tests.  Each set of tests records in the corresponding
      variable (qN_data) the values received as a pair (series,
      value); data is sent along a queue in a strictly sequential
      order, that is, lower values before higher values.  So when a
      value is received, to check that it's correct it's sufficient to
      check that it's the highest value in its series in the list, and
      that the next lower value is also present in the list. *)

  fun is_largest (series, value, []) = true
    | is_largest (series: int, value: int, (hs, hv) :: rest) =
       (series <> hs orelse value > hv) andalso
       is_largest (series, value, rest)

  fun has_prev (series, value, []) = false
    | has_prev (series: int, value: int, (hs, hv) :: rest) =
       (series = hs andalso value = (hv + 1)) orelse
       has_prev (series, value, rest)

  val q12_data = ref [(1, 0), (2, 0)]
  val q3_data = ref [(3, 0)]
  val q4_data = ref [(4, 0)]
  val q5_data = ref [(5, 0), (6, 0), (7, 0)]
  val q8_data = ref [(8, 0), (9, 0)]

  (* the first test, with q{12,3,4,5,8}_data sizes of 2, 1, 1, 3, 2,
     should be 2. *)
  fun test_name () =
       Integer.toString ((length (! q12_data)) + (length (! q3_data)) +
			 (length (! q4_data)) + (length (! q5_data)) +
			 (length (! q8_data)) - 7)

  fun receive_msg (result, (series, value)) =
   let val current_res = ! result
       val current_name = test_name ()
       fun test_fun () =
	    if is_largest (series, value, current_res) andalso
	       has_prev (series, value, current_res) then true
	    else
	     (local_print ("test " ^ current_name ^
			   " received bad value " ^
			   Integer.toString series ^
			   "." ^ Integer.toString value ^ "\n");
	      false)
   in B.Test.test (current_name, test_fun);
      result := (series, value) :: (! result);
      if debug_level <> NONE then
       B.V.Print.print ("received value " ^
			Integer.toString series ^
			"." ^ Integer.toString value ^ "\n")
      else ()
   end

  fun send_msg (q, series, value) =
       B.Pipe.enqueue (q, (series, value))

  fun rq (_, _, 0) = ()
    | rq (q, data, n) =
       (receive_msg (data, B.Pipe.dequeue q);
        rq (q, data, n - 1))

  fun sq (_, _, 0) = ()
    | sq (queues, data, n) =
       (receive_msg (data, B.Pipe.select queues);
        sq (queues, data, n - 1))

  fun send_queue (_, series, value, 0) = ()
    | send_queue (q, series, value, n) =
       (send_msg (q, series, value);
        send_queue (q, series, value + 1, n - 1))

  fun send_queues ([]) = ()
    | send_queues ((q, series, value) :: rest) =
       (send_msg (q, series, value);
        send_queues rest)

(* this tests a bug in one version of the pipe software, where the
   enqueue operation would loop forever. *)
  fun timeout_queue () =
       let val p = B.Pipe.new ()
       in B.Pipe.dequeue_timeout (p, 0) = NONE andalso
	  B.Pipe.enqueue (p, ()) = ()
       end

  fun run_tests () = 
   let val q12 = B.Pipe.new ()
       val q3 = B.Pipe.new ()
       val q4 = B.Pipe.new ()
       val q5 = B.Pipe.new ()
       val q6 = B.Pipe.new ()
       val q7 = B.Pipe.new ()
       val q5r = [q5, q6, q7]
       val q5s = [(q5, 5, 1), (q6, 6, 1), (q5, 5, 2), (q7, 7, 1)]
       val q8 = B.Pipe.new ()
       val q9 = B.Pipe.new ()
       val q8r = [q8, q9]
       val q8s = [(q8, 8, 1), (q9, 9, 1), (q8, 8, 2), (q8, 8, 3)]
       fun queue_sizes () = (B.Pipe.size q12, B.Pipe.size q3, B.Pipe.size q4)
       fun initial_sizes () = queue_sizes () = (0, 0, 0)
       fun final_sizes () = queue_sizes () = (0, ~1, 2)
   in map B.Pipe.clear [q12, q3, q4, q5, q6, q7, q8, q9];
      B.Test.test ("1", initial_sizes);
      B.Scheduler.fork (fn () => rq (q12, q12_data, 3));
      B.Scheduler.fork (fn () => rq (q12, q12_data, 2));
      B.Scheduler.fork (fn () => rq (q3, q3_data, 4));
      B.Scheduler.fork (fn () => sq (q8r, q8_data, 4));
      B.Scheduler.fork (fn () => send_queue (q3, 3, 1, 3));

      B.Scheduler.fork (fn () => send_queue (q12, 2, 1, 2));
      B.Scheduler.fork (fn () => send_queue (q12, 1, 1, 3));

      B.Scheduler.fork (fn () => send_queue (q4, 4, 1, 4));
      B.Scheduler.fork (fn () => send_queues q5s);
      B.Scheduler.fork (fn () => rq (q4, q4_data, 2));
      B.Scheduler.fork (fn () => sq (q5r, q5_data, 4));
      B.Scheduler.fork (fn () => send_queues q8s);
      B.Scheduler.sleep 1000;
      B.Test.test (test_name (), final_sizes);
      B.Test.test ("timeout-queue", timeout_queue);
      map B.Pipe.clear [q12, q3, q4, q5, q6, q7, q8, q9];
      ()
   end

  fun run () =
       (B.Scheduler.reset ();
	B.Test.tests ("Pipe", 21, run_tests);
        B.Scheduler.reset ())

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
		2.	structure Test_Pipe
*)

structure Test_Pipe = Test_Pipe (structure B = Fox_Basis
				 val debug_level = NONE)

