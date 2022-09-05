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

	pipe.fun: functor for synchronized data pipes

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Data_Pipe
	2.	function new
	3.	function size
	4.	function clear
	5.	function enqueue
	6.	internal function add_suspension
	7.	internal function suspend_timeout
	8.	function dequeue
	9.	function dequeue_timeout
	10.	function dequeue_immediately
	11.	function requeue
	12.	function select
	13.	function size
	14.	function clear

		iii.	RCS Log
	
$Log: pipe.fun,v $
Revision 1.15  1997/02/13  00:47:53  esb
eliminated imperative types.

Revision 1.14  1996/10/13  18:49:52  esb
adapted to pass value restriction.

Revision 1.13  1996/07/05  17:23:02  esb
re-implementation from scratch, using scheduler instead of Event_Queue.

Revision 1.12  1995/10/17  21:44:25  esb
added dequeue_immediately and dequeue_timeout.

Revision 1.11  1995/06/20  16:45:12  esb
changed new to take a unit parameter instead of an int option.

Revision 1.10  95/03/10  03:44:58  esb
adapted to new vendor.sig.

Revision 1.9  1994/12/01  23:32:47  esb
tested select and made it work.

Revision 1.8  1994/12/01  18:44:17  esb
minor change.

Revision 1.7  1994/12/01  18:28:09  esb
added select.

Revision 1.6  1994/04/26  17:50:27  esb
adapted to new COROUTINE and EVENT signatures.

Revision 1.5  94/01/17  17:58:45  esb
interface change.

Revision 1.4  1993/10/12  22:48:30  esb
added requeue and made T an eqtype. Also made the parameter to new optional.

Revision 1.3  1993/09/17  16:41:10  milnes
Added a default sizing parameter.

Revision 1.2  1993/09/13  22:06:46  cline
deleted '#'s from RCS log

Revision 1.1  1993/09/02  15:18:40  esb
Initial revision


		1.	functor Data_Pipe

	The current implementation does not use the size parameter
	provided to "new".
*)

functor Data_Pipe (structure Scheduler: COROUTINE
		   structure Queue: DEQ
		   structure V: VENDOR
		   structure Debug: DEBUG): DATA_PIPE =
 struct

  datatype 'value queue =
      Suspended of ('value Scheduler.suspension * unit ref) Queue.T
    | Data of 'value Queue.T

  type 'value T = 'value queue ref

(*
	2.	function new
*)

  fun new _ = ref (Data (Queue.new ()))

(*
	3.	function size
*)

  fun size (ref (Data queue)) = Queue.size queue
    | size (ref (Suspended queue)) = ~ (Queue.size queue)

(*
	4.	function clear
*)

  fun clear self = self := Data (Queue.new ())

(*
	5.	function enqueue
*)

  fun enqueue (self as (ref (Suspended queue)), value) = 
       (case Queue.first queue of
	   NONE =>
	    self := Data (Queue.add_to_back (Queue.new (), value))
	 | SOME (rest, (first, _)) =>
	    (if Queue.empty rest then self := Data (Queue.new ())
	     else self := Suspended rest;
	     Scheduler.resume (first, value)))
    | enqueue (self as (ref (Data queue)), value) =
       self := Data (Queue.add_to_back (queue, value))

(*
	6.	internal function add_suspension
*)

  local
   fun add_suspension (self, id) s =
        case ! self of
	   Suspended queue =>
	    self := Suspended (Queue.add_to_back (queue, (s, id)))
	 | Data queue =>
	    (case Queue.first queue of
	        NONE =>
		 self := Suspended (Queue.add_to_back (Queue.new (), (s, id)))
	      | SOME (rest, value) =>
		 (self := Data rest;
		  Scheduler.resume (s, value)))

(*
	7.	internal function suspend_timeout
*)

   fun suspend_timeout (self, timeout) option_suspension =
        let val id = ref ()
	    fun timeout_thread () =
	         (Scheduler.sleep timeout;
		  case ! self of
		     Suspended queue =>
		      (case Queue.delete (queue, fn (_, x) => x = id) of
	                  NONE => ()		(* done already *)
			| SOME new_queue =>
(* remove from the queue, and resume the option suspension with NONE. *)
			   (self := Suspended new_queue;
			    Scheduler.resume (option_suspension, NONE)))
		   | _ => ())
	in Scheduler.fork timeout_thread;
(* if "suspend" ever returns, dequeue worked, so resume the option
   suspension with the result.  Enqueue will have removed the
   suspension from the queue so the timeout thread will do nothing. *)
           Scheduler.resume (option_suspension,
			     SOME (Scheduler.suspend
				   (add_suspension (self, id))))
	end

(*
	8.	function dequeue
*)

  in
   fun dequeue (self as (ref (Suspended queue))) =
        Scheduler.suspend (add_suspension (self, ref ()))
     | dequeue (self as (ref (Data queue))) = 
	(case Queue.first queue of
	    NONE => Scheduler.suspend (add_suspension (self, ref ()))
	  | SOME (rest, first) =>
	     (self := Data rest;
	      first))

(*
	9.	function dequeue_timeout
*)

   fun dequeue_timeout (self as (ref (Suspended queue)), time) =
        Scheduler.suspend (suspend_timeout (self, time))
     | dequeue_timeout (self as (ref (Data queue)), time) = 
        (case Queue.first queue of
	    NONE => Scheduler.suspend (suspend_timeout (self, time))
	  | SOME (rest, head) =>
	     (self := Data rest;
	      SOME head))

  end (* local *)

(*
	10.	function dequeue_immediately
*)

  fun dequeue_immediately (ref (Suspended _)) = NONE
    | dequeue_immediately (self as (ref (Data queue))) = 
       (case Queue.first queue of
	   NONE => NONE
	 | SOME (rest, head) =>
	    (self := Data rest;
	     SOME head))

(*
	11.	function requeue
*)

  fun requeue (self as (ref (Suspended queue)), value) = 
       enqueue (self, value)
    | requeue (self as (ref (Data queue)), value) = 
       self := Data (Queue.add_to_front (queue, value))

(*
	12.	function select
*)

  exception Illegal_Argument_To_Pipe_Select

  fun select [single_pipe] = dequeue single_pipe
    | select [] = raise Illegal_Argument_To_Pipe_Select
    | select pipes =
       let fun ready_pipe [] = NONE
	     | ready_pipe (first :: rest) =
	        case ! first of
		   Suspended _ => ready_pipe rest
		 | Data queue =>
		    (case Queue.first queue of
		        NONE => ready_pipe rest
		      | SOME (rest, head) =>
			 (first := Data rest;
			  SOME head))
	   val resumed = ref false
	   fun dequeue_one_pipe (pipe, suspension) () =
	        let val result = dequeue pipe
		in if ! resumed then ()
		   else
		    (resumed := true;
		     Scheduler.resume (suspension, result))
		end
	   fun dequeue_all_pipes [] _ = ()
	     | dequeue_all_pipes (first :: rest) suspension =
	        Scheduler.fork (dequeue_one_pipe (first, suspension))
       in case ready_pipe pipes of
	     SOME value => value
	   | NONE =>
	      Scheduler.suspend (dequeue_all_pipes pipes)
       end

 end (* struct *)

