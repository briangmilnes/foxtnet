(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	sem.fun: implementation of a semaphore module.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor SEMAPHORE

		iii.	RCS Log
	
$Log: sem.fun,v $
Revision 1.3  1996/06/07  20:17:49  cline
handle exceptions when using before

Revision 1.2  1995/11/10  23:30:12  esb
added try_lock and try_acquire.

Revision 1.1  1995/06/20  16:46:14  esb
Initial revision


		1.	functor SEMAPHORE
*)

functor Semaphore (structure Scheduler: COROUTINE
		   structure Queue: FIFO): SEMAPHORE =
 struct
  datatype semaphore = Free | Busy of unit Scheduler.suspension Queue.T

  type T = semaphore ref

  fun new () = ref Free

  fun free (ref Free) = true
    | free _ = false

  fun queue_size (ref Free) = ~1
    | queue_size (ref (Busy queue)) = Queue.size queue

  fun clear semaphore = semaphore := Free

  (* note that enqueue may be called in a different thread than acquire,
     so it needs to check semaphore all over again. *)
  fun enqueue semaphore self =
       case ! semaphore of
	  Free =>
	   (semaphore := Busy (Queue.new ());
	    Scheduler.resume (self, ()))
	| Busy queue =>
	   semaphore := Busy (Queue.add (queue, self))

  fun acquire semaphore =
       case ! semaphore of
	  Free => semaphore := Busy (Queue.new ())
	| Busy queue =>
	   Scheduler.suspend (enqueue semaphore)
               (* when resumed, we have lock *)

  fun try_acquire semaphore =
       case ! semaphore of
	  Free =>
	   (semaphore := Busy (Queue.new ());
	    true)
	| Busy queue => false

  fun release semaphore =
       case ! semaphore of
	  Free => ()
	| Busy queue =>
	   (case Queue.next queue of
	       NONE => semaphore := Free
	     | SOME (new_queue, next_in_line) =>
	        (semaphore := Busy new_queue;
		 Scheduler.resume (next_in_line, ())))

  fun before_handle (f, arg, after, after_arg) =
    let val result = (f arg) handle x => (after after_arg; raise x)
    in after after_arg;
       result
    end

  fun with_lock (semaphore, f, arg) =
       (acquire semaphore;
	before_handle (f, arg, release, semaphore))

  fun try_lock (semaphore, f, arg) =
       (if try_acquire semaphore then
	 SOME (before_handle (f, arg, release, semaphore))
	else NONE)
 end
