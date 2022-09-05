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

	event.sig: signature for an event synchronization module.
	An event manager has two major operations: wait and signal. A
	thread doing a wait is suspended until another thread calls a
	matching signal.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature EVENT_QUEUE

		iii.	RCS Log
	
$Log: event.sig,v $
Revision 1.13  1997/04/22  10:40:02  esb
improved comments.

Revision 1.12  97/02/13  00:47:05  esb
eliminated imperative types

Revision 1.11  1995/11/10  23:29:44  esb
added a simplified event interface.

Revision 1.10  1995/10/17  21:44:10  esb
added wait_timeout.

Revision 1.9  1994/12/01  18:28:09  esb
added select.

Revision 1.8  1994/08/08  18:18:56  milnes
Updated signal's comment.

Revision 1.7  1994/05/04  01:36:22  esb
added the "empty" function.\

Revision 1.6  94/04/26  17:48:45  esb
no more string parameter in wait.

Revision 1.5  93/12/04  20:36:26  esb
improved comments, removed the equality types.

Revision 1.4  1993/10/12  22:47:54  esb
made T and its parameters eqtypes.

Revision 1.3  1993/09/13  22:06:43  cline
deleted '#'s from RCS log

Revision 1.2  1993/09/10  11:31:10  esb
made signal return the event if it found someone to signal.

Revision 1.1  1993/09/02  15:17:37  esb
Initial revision


		1.	signature SIMPLE_EVENT_QUEUE

	A simple event queue supports control synchronization.  A queue
	can be waited on in one thread.

*)

signature SIMPLE_EVENT_QUEUE =
 sig

  type T

  val new: unit -> T

  val clear: T -> unit

  val size: T -> int
  val empty: T -> bool

(* "wait" queues itself to wait for an event, then (in a separate
   thread) executes the "post-processing" function.  The
   post-processing function may call signal and thus cause the wait to
   complete. *)

  val wait: T * (unit -> unit) -> unit

(* signal the event queue, returning whether any thread was woken up. *)
  val signal: T -> bool


  val select: T list * (unit -> unit) -> unit

(* returns true if it returns as a result of a signal, false otherwise *)
  val wait_timeout: T * (unit -> unit) * int -> bool

 end

(*
		1.	signature EVENT_QUEUE
*)

signature EVENT_QUEUE =
 sig

(* an event queue can be used to suspend a thread (wait) until the
   awaited event is signaled (signal). The 'a represents the type
   of the event, the 'b the type of the value waited for. *)
  eqtype ('a, 'b) T

  val new: unit -> ('a, 'b) T

  val clear: ('a, 'b) T -> unit

  val size: ('a, 'b) T -> int
  val empty: ('a, 'b) T -> bool

(* the parameters to "wait" are the queue, the event ID, and a
   "post-processing" closure that will be called only after the
   wait has been queued.
 *)
  val wait: {queue: ('a, 'b) T, event: 'a, while_waiting: unit -> unit} -> 'b

  val select: {queues: (('a, 'b) T * 'a) list,
	       while_waiting: unit -> unit}
            -> 'b

  val wait_timeout: {queue: ('a, 'b) T, event: 'a,
		     while_waiting: unit -> unit, timeout: int} -> 'b option

(* the parameters to "signal" are the queue, a function to determine
   whether an event matches this signal, and a value to be returned
   by the corresponding "wait".

   If there is no matching "wait", this function has no effect, other
   than perhaps calling the match function multiple times. The
   value returned is NONE.

   If there are multiple matching "wait"s, only the oldest is woken up
   and given the value. Signal awakens the "event" coroutine and
   immediately returns (SOME event). Callers should not depend on the
   event coroutine being called immediately as implementations may or
   may not choose to do this.
 *)
  val signal: {queue: ('a, 'b) T, match: 'a -> bool, value: 'b}
            -> 'a option

 end
