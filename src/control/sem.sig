(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	sem.sig: signature for a semaphore module.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature SEMAPHORE

		iii.	RCS Log
	
$Log: sem.sig,v $
Revision 1.2  1995/11/10  23:30:12  esb
added try_lock and try_acquire.

Revision 1.1  1995/06/20  16:46:14  esb
Initial revision


		1.	signature SEMAPHORE

	A semaphore can be used around critical regions, allowing a
	single thread to enter the critical region at any one time.
*)

signature SEMAPHORE =
 sig
  type T

  val new: unit -> T

  (* with_lock acquires the lock, applies the function to the given
     argument, releases the lock, and returns the result of the call. *)
  val with_lock: T * ('a -> 'b) * 'a -> 'b
  val try_lock: T * ('a -> 'b) * 'a -> 'b option

  val acquire: T -> unit
  val release: T -> unit
  val try_acquire: T -> bool

  val free: T -> bool
  val queue_size: T -> int		(* returns ~1 if lock is free *)
  val clear: T -> unit			(* kills any pending processes *)
 end
