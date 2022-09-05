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

	coro.sig: signature COROUTINE, for a simple coroutine package


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature COROUTINE

		iii.	RCS Log
	
$Log: coro.sig,v $
Revision 1.13  1997/04/22  10:38:52  esb
added the function "yield".

Revision 1.12  97/03/27  13:44:35  esb
removed fork_limited_time, added handlers.

Revision 1.11  97/02/13  00:46:41  esb
eliminated imperative types.

Revision 1.10  1995/01/18  20:58:53  esb
separated fork and fork_limited_time.

Revision 1.9  1994/04/28  13:17:57  esb
made leaner and meaner.

Revision 1.8  94/04/26  17:47:32  esb
added suspend and resume, recoded, added timing calls.

Revision 1.7  94/01/17  17:57:12  esb
changed to fork_limited.

Revision 1.6  1994/01/13  16:21:42  milnes
Added a kill to coroutines, it required priority queue and deq deletes.

Revision 1.5  93/12/04  20:35:14  esb
removed polling function and added low-priority threads.

Revision 1.4  1993/09/17  16:38:01  milnes
 Changed sleep_fun to set_polling_fun.

Revision 1.3  1993/09/02  15:14:23  esb
added get_info and set_info and the Info datatype.

Revision 1.2  1993/06/18  14:36:41  esb
added string parameter to sleep_fun

Revision 1.1  1993/06/10  22:35:17  milnes
Initial revision


		1.	signature COROUTINE

*)

signature COROUTINE =
 sig

(* This exception is raised by exit when no other process can be woken up. *)
  exception No_Ready_Thread

(* Eliminate all but the calling thread. *)
  val reset: unit -> unit

(* fork creates a coroutine, queues it, and returns to the parent process.
 *)
  val fork: (unit -> unit) -> unit

(* Sleep suspends this thread for at least the specified
   number of milliseconds. If there are any coroutines on the
   ready queue, execution of the first ready coroutine begins
   immediately. *)
  val sleep: int -> unit

  exception No_Such_Suspension

(* suspend suspends this thread until a corresponding resume, returning
   the value provided by resume.
   The suspended thread's suspension is given to the argument function.
   When resuming a function, the resumed coroutine is placed on
   the ready queue and the calling coroutine continues execution.
   A suspension can only be resumed once; attempts to resume it
   more than once raise No_Such_Suspension.
   fun yield () = suspend (fn s => resume (s, ()))
 *)
  type 'a suspension
  val suspend: ('a suspension -> 'b) -> 'a
  val resume: 'a suspension * 'a -> unit
  val yield: unit -> unit

(* Exit terminates the current thread and restarts a ready thread, if any;
   raises No_Ready_Thread if there are no more ready or sleeping threads. *)
  val exit: unit -> 'a

(* install_handler takes a function which is called whenever data
   is available on a device.  The function can read the device expecting
   that the read will not block.  The type of a device depends on the
   specific implementation.  For the user-level foxnet,
      type device = Posix.IO.file_desc  *)

  type device = Posix.FileSys.file_desc
  exception Device_Handler_Already_Installed
  val install_handler: device * (device -> unit) -> unit
  val uninstall_handler: device -> unit

 end

