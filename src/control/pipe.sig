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

	pipe.sig: signature for synchronized data pipes

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DATA_PIPE

		iii.	RCS Log
	
$Log: pipe.sig,v $
Revision 1.8  1997/02/13  00:47:53  esb
eliminated imperative types.

Revision 1.7  1995/10/17  21:44:25  esb
added dequeue_immediately and dequeue_timeout.

Revision 1.6  1995/06/20  16:45:12  esb
changed new to take a unit parameter instead of an int option.

Revision 1.5  94/12/01  18:28:09  esb
added select.

Revision 1.4  1994/08/02  19:29:45  esb
got rid of eqtypes where possible.

Revision 1.3  1993/10/12  22:48:30  esb
added requeue and made T an eqtype. Also made the parameter to new optional.

Revision 1.2  1993/09/13  22:06:47  cline
deleted '#'s from RCS log

Revision 1.1  1993/09/02  15:18:40  esb
Initial revision


		1.	signature DATA_PIPE

 A data pipe stores enqueued values until dequeued in first-in, first-out
 order. A dequeue operation on a pipe with no data will block, and is
 only restarted after another thread queues some data. If more than one
 dequeue operation is blocked, they are satisfied in FIFO order.

 *)

signature DATA_PIPE =
 sig
  type 'value T

(* To discard excess data, use the size function.  To dequeue from
   any one of a set of pipes, use "dequeue o select". (select will
   block forever if given an empty list). *)

  val new: unit -> 'value T

  val enqueue: 'value T * 'value -> unit
  val dequeue: 'value T -> 'value

  val select: 'value T list -> 'value
  val dequeue_immediately: 'value T -> 'value option
  val dequeue_timeout: 'value T * int -> 'value option

(* a data item re-queue will be placed in the front of the queue,
   not the back. This violates the normal FIFO ordering of the queue,
   but can be very useful in selected cases. The caller may be suspended
   if the queue is overfull. *)
  val requeue: 'value T * 'value -> unit

(* the size of a pipe with "dequeue" operations pending is negative. *)
  val size: 'value T -> int
  val clear: 'value T -> unit

 end
