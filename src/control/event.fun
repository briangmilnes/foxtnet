(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	event.fun: FoxNet event manager package

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Event_Queue
	2.	function new
	3.	function size
	4.	function empty
	5.	function clear
	6.	function wait
	7.	function select
	8.	function signal
	9.	function wait_timeout
	10.	functor Simple_Event_Queue

	iii.	RCS Log

$Log: event.fun,v $
Revision 1.22  1997/03/10  18:51:02  esb
removed a compiler warning message.

Revision 1.21  1997/02/13  00:47:25  esb
eliminated imperative types.

Revision 1.20  1996/04/18  21:00:50  cline
converted hash from int to word

Revision 1.19  1995/11/10  23:30:00  esb
added a simplified event interface.

Revision 1.18  1995/10/17  21:44:10  esb
added wait_timeout.

Revision 1.17  1995/06/20  16:44:29  esb
changed over to using module Trace.

Revision 1.16  1995/03/12  16:23:06  esb
adapted debug_print to new interface.

Revision 1.15  1995/03/10  03:44:58  esb
adapted to new vendor.sig.

Revision 1.14  1994/12/01  18:28:09  esb
added select.

Revision 1.13  1994/06/16  16:29:59  danwang
Updated to use functorized Fox_Basis

Revision 1.12  1994/05/24  16:08:02  milnes
Changed an error message to print "event.fun:".

Revision 1.11  1994/05/04  01:36:36  esb
added the "empty" function.

Revision 1.10  94/04/26  17:49:01  esb
reimplemented using suspend and resume.

Revision 1.9  94/03/25  16:02:50  esb
added printing of the name of handled exceptions.

Revision 1.8  94/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file move problems that confused rcs.

Revision 1.6  1994/01/17  17:58:21  esb
interface change.

Revision 1.5  1994/01/13  16:22:06  milnes
Added a kill to coroutines, it required priority queue and deq deletes.

Revision 1.4  93/12/04  20:36:50  esb
improved comments, removed the equality types, adapted to new coro.sig.

Revision 1.3  1993/09/13  22:06:44  cline
deleted '#'s from RCS log

Revision 1.2  1993/09/10  11:31:10  esb
made signal return the event if it found someone to signal.

Revision 1.1  1993/09/02  15:17:37  esb
Initial revision

*)
(*

	1.	functor Event_Queue
*)

functor Event_Queue (structure Scheduler: COROUTINE
		     structure Store: STORE
		     structure Debug: DEBUG
		     structure V: VENDOR
		     val debug_level: int ref option): EVENT_QUEUE =
 struct
  structure Trace = Trace (structure V = V
			   val debug_level = debug_level
			   val module_name = "event.fun"
			   val makestring = fn _ => NONE)

  datatype ('a, 'b, 'c) wait =
      Wait of 'a * 'b Scheduler.suspension
    | Select of 'a * 'b Scheduler.suspension * 'c list

  (* the identifier for the store is a word ref.  The word is
     used for hashing, and so different entries may have the same
     value; the ref is used for uniqueness, and a new one is
     allocated for each entry. *)
  datatype ('a, 'b) T =
      Q of (word ref, ('a, 'b, ('a, 'b) T) wait) Store.T ref

(*
	2.	function new
*)

  val hash_randomizer = ref 0w0
  val hash_range = 0w10000

  fun hash n = ! n
  fun equal (a: word ref, b) = a = b

  fun new () = Q (ref (Store.new (hash, equal)))

(*
	3.	function size
*)

  fun size (Q queue) = Store.size (! queue)

(*
	4.	function empty
*)

  fun empty (Q queue) = Store.empty (! queue)

(*
	5.	function clear
*)

  fun clear (Q queue) = queue := Store.new (hash, equal)

(*
	6.	function wait
*)

  fun wait {queue = Q queue, event, while_waiting} =
       let val id = ref (! hash_randomizer)
	   fun queue_suspension s =
                (queue := Store.add (! queue, id, Wait (event, s));
	         ((while_waiting ())
	          handle x =>
		          Trace.print_handled (x, SOME "wait/while_waiting")))
       in hash_randomizer := (! hash_randomizer + 0w1) mod hash_range;
	  Scheduler.suspend queue_suspension
       end

(*
	7.	function select
*)

  fun select {queues, while_waiting} =
       let val id = ref (! hash_randomizer)
	   val queue_list = map (fn (a, b) => a) queues
	   fun queue_suspension s (Q queue, event) =
                queue := Store.add (! queue, id, Select (event, s, queue_list))
	   fun suspend_all s =
	        (map (queue_suspension s) queues;
	         ((while_waiting ())
	          handle x =>
		          Trace.print_handled (x,
					       SOME "select/while_waiting")))
       in hash_randomizer := (! hash_randomizer + 0w1) mod hash_range;
          Scheduler.suspend suspend_all
       end

(*
	8.	function signal
*)

  local
   fun safe_match (match, v) =
        (match v)
	handle x =>
	       (Trace.print_handled (x, SOME "match");
		false)

   fun remove_id id (Q queue) = queue := Store.remove (! queue, id)

   fun find_match _ (_, SOME x) = SOME x
     | find_match (match, queue) ((id, Wait (event, s)), NONE) =
        if safe_match (match, event) then
	 (queue := Store.remove (! queue, id);
	  SOME (event, s))
	else NONE
     | find_match (match, _) ((id, Select (event, s, queue_list)), NONE) =
        if safe_match (match, event) then
	 (map (remove_id id) queue_list;
	  SOME (event, s))
	else NONE

  in
   fun signal {queue = Q queue, match, value} =
        case Store.fold (find_match (match, queue)) (! queue) NONE of
           NONE =>
	    (Trace.debug_print (fn _ => "no threads waiting for signal");
	     NONE)
	 | SOME (event, suspension) =>
	    (Scheduler.resume (suspension, value);
	     SOME event)
  end (* local *)

(*
	9.	function wait_timeout

	This implementation uses two queues.  The first queue is used
	to accept the result of signaling, if any.  The second queue
	is used to transmit a result option, namely NONE if called by
	the timeout or SOME result otherwise.  The timer cleans up the
	first queue if it expires, otherwise everything happens as
	usual and the timer is garbage collected when it expires.
*)

(*    wait_timeout: {queue: ('a, 'b) T, event: 'a,
		     while_waiting: unit -> unit, timeout: int} -> 'b option *)
  fun wait_timeout {queue = (Q queue): ('a, 'b) T,
		    event, while_waiting, timeout} =
       let val new_queue = new (): (unit, 'b option) T
           val id = ref (! hash_randomizer)
	   fun queue_suspension s =
                (queue := Store.add (! queue, id, Wait (event, s));
	         ((while_waiting ())
	          handle x =>
		          Trace.print_handled
			    (x, SOME "wait_timeout/while_waiting")))
	   fun wait_for_data () =
                (hash_randomizer := (! hash_randomizer + 0w1) mod hash_range;
		 signal {queue = new_queue, match = fn _ => true,
			 value = SOME (Scheduler.suspend queue_suspension)};
		 ())
	   fun timer () =
	        (Scheduler.sleep timeout;
		 queue := Store.remove (! queue, id);
		 signal {queue = new_queue, match = fn _ => true,
			 value = NONE};
		 ())
	   fun start_thread () =
	        (Scheduler.fork timer;
		 wait_for_data ())
       in wait {queue = new_queue, event = (), while_waiting = start_thread}
       end

 end (*struct*)

(*
	10.	functor Simple_Event_Queue
*)

functor Simple_Event_Queue (structure Event_Queue: EVENT_QUEUE):
                           SIMPLE_EVENT_QUEUE =
 struct
  type T = (unit, unit) Event_Queue.T

  val new = Event_Queue.new
  val clear = Event_Queue.clear
  val size = Event_Queue.size
  val empty = Event_Queue.empty

  fun wait (queue, while_waiting) =
       Event_Queue.wait {queue = queue, event = (),
			 while_waiting = while_waiting}

  fun map_select queue = (queue, ())

  fun select (queues, while_waiting) =
       Event_Queue.select {queues = map map_select queues,
			   while_waiting = while_waiting}

  fun wait_timeout (queue, while_waiting, timeout) =
       case Event_Queue.wait_timeout {queue = queue, event = (),
				      while_waiting = while_waiting,
				      timeout = timeout} of
	  NONE => false
	| SOME () => true

  fun signal queue =
       case Event_Queue.signal {queue = queue, match = fn _ => true,
				value = ()} of
	  NONE => false
	| SOME () => true

 end (*struct*)






