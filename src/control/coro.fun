(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	coro.fun: coroutine and communication package for the FoxNet

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	Functor Coroutine
	2.	internal function gettime
	3.	local implementation of Fifo
	4.	types and state
	5.	internal function null_thread
	6.	function reset
	7.	function fork
	8.	internal function coroutine_exn
	9.	error functions internal to exit
	10.	internal function call_handler
	11.	internal function call_ready
	12.	internal function wakeup
	13.	function exit
	14.	function sleep
	15.	function suspend
	16.	function resume
	17.	function yield
	18.	function install_handler
	19.	function uninstall_handler

	iii.	RCS Log

$Log: coro.fun,v $
Revision 1.44  1997/12/11  19:45:52  esb
fixed a bug that polled non-zero time even when processes were waiting.

Revision 1.43  97/12/09  21:17:35  esb
replaced the Foxnet cfun "select" with the more standard "poll".

Revision 1.42  97/11/14  11:39:27  cline
updated to work with SMLNJ 109.32 basis

Revision 1.41  97/04/22  10:38:52  esb
added the function "yield".

Revision 1.40  97/03/27  13:44:35  esb
removed fork_limited_time, added handlers.

Revision 1.39  1997/02/13  00:46:41  esb
eliminated imperative types.

Revision 1.38  1996/11/18  19:20:25  esb
changed "wakeup" to wake up all sleeping processes that are ready.

Revision 1.37  1996/06/11  03:25:04  esb
adapted to new priority queue functor.

Revision 1.36  1996/05/17  19:07:35  cline
fixed storage leak in exit

Revision 1.35  1996/04/18  18:58:49  cline
changed Time.gettimeofday to Time.now

Revision 1.34  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.33  1995/06/20  16:43:55  esb
converted over to Trace and V.Time.

Revision 1.32  1995/03/10  03:44:58  esb
adapted to new vendor.sig.

Revision 1.31  1995/01/18  20:58:53  esb
separated fork and fork_limited_time.

Revision 1.30  1994/11/02  19:29:32  esb
fixed a bug that made sleep terminate prematurely sometimes.

Revision 1.29  1994/08/17  16:21:48  esb
to increase accuracy made null_thread return and set max_calls to one.

Revision 1.28  1994/04/28  13:18:33  esb
many changes.

Revision 1.27  94/04/26  17:47:32  esb
added suspend and resume, recoded, added timing calls.

Revision 1.26  94/03/25  16:18:48  esb
added a better print function, and now prints exception names.

Revision 1.25  94/02/28  11:08:46  esb
cleaned up the exit code and added an idle profiling counter.

Revision 1.24  94/02/25  16:02:02  esb
changed so limited threads are call with the longest possible time.

Revision 1.23  94/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file move problems that confused rcs.

Revision 1.21  1994/01/31  01:14:36  esb
optimized a little.

Revision 1.20  1994/01/17  17:57:26  esb
changed to fork_limited.

Revision 1.19  1994/01/13  16:21:13  milnes
Added a kill to coroutines, it required priority queue and deq deletes.

Revision 1.18  93/12/04  20:35:41  esb
removed polling function and added low-priority threads; cleaned up.

Revision 1.17  1993/10/25  19:31:31  cline
removed .U from Byte[421].U

Revision 1.16  1993/10/22  02:47:23  esb
exit no longer raises No_Ready_Threads, since the polling function may
create new threads. Also improved the error messages.

Revision 1.15  1993/10/18  20:40:33  esb
improved error messages.

Revision 1.14  1993/10/14  18:16:15  milnes
Used implicit sequencing in let bodies.

Revision 1.13  1993/10/12  22:42:26  esb
adapted to new definition of fifo.

Revision 1.12  1993/10/09  00:09:27  esb
removed a tracing message that made it very hard to trace large programs.

Revision 1.11  1993/09/17  16:37:18  milnes
Name change of sleep_fun to set_polling_fun.

Revision 1.10  1993/09/02  15:15:08  esb
added get_info and set_info and the Info datatype.

Revision 1.9  1993/07/20  15:40:27  esb
Fixed a bug in the "less" function.

Revision 1.8  1993/07/20  15:09:39  esb
Started using Priority_Queue for the sleeping_queue.

Revision 1.7  1993/07/13  02:19:22  esb
got rid of a bug caused by reliance, within advance_timer, of
values from before call_sleep_fun still being valid. This
caused problems when the sleep_fun would suspend and the
ready and sleep queues would change before the sleep_fun returned.

Revision 1.6  1993/07/08  03:00:11  esb
removed the unused type packet (pointed out by nickh).

Revision 1.5  1993/07/07  15:37:08  esb
fixed a bug that would read the value of a ref, then call advance_timer,
which changed the value of the ref, then would read sleeping_queue and
assume that the old value of the ref would still hold.
Also cleaned up quite a bit.

Revision 1.4  1993/07/07  13:18:51  esb
removed a performance bug

Revision 1.3  1993/06/18  15:50:49  esb
removed print statements (bug was elsewhere) and added do_if_debug

Revision 1.2  1993/06/18  14:45:26  esb
added string argument and temporary debugging messages for sleep_fun

Revision 1.1  1993/06/10  22:35:17  milnes
Initial revision

	1.	Functor Coroutine
*)

functor Coroutine (structure Debug: DEBUG
		   structure V: VENDOR): COROUTINE =
 struct
  exception No_Ready_Thread
  exception No_Such_Suspension
  exception Device_Handler_Already_Installed
  exception Unable_To_Convert_IOD_to_Poll_Desc

  fun makestring_exn No_Ready_Thread =
       SOME "no ready thread"
    | makestring_exn No_Such_Suspension =
       SOME "no such suspension (suspension already resumed)"
    | makestring_exn Device_Handler_Already_Installed =
       SOME "device handler already installed for this device"
    | makestring_exn Unable_To_Convert_IOD_to_Poll_Desc =
       SOME "error converting IO descriptor to poll descriptor"
    | makestring_exn _ = NONE

  structure Trace = Trace (structure V = V
			   val debug_level = NONE
			   val module_name = "coro.fun"
			   val makestring = makestring_exn)

  datatype spec = Normal of unit -> unit
                | Limited_Time of int -> unit

  datatype 'a suspension = Suspension of 'a V.Control.cont * int ref

  type device = Posix.IO.file_desc

(*
	2.	internal function gettime

	Call V.Time.now at most once every millisecond by setting
	a ref in a function called (supposedly) every millisecond,
	and then calling V.Time.now if the ref is set, and using
	the cached time otherwise.
*)

  local			(* local to all the functions in coroutine. *)

(*
   local
    val pthread_start: (int -> unit) =
           System.Unsafe.CInterface.c_function "FoxNet" "pthread_start";

    val pthread_executed: (unit -> bool) =
           System.Unsafe.CInterface.c_function "FoxNet" "pthread_executed";

    val time = ref (V.Time.now ())
    fun noop () = ()
    val start_fun = ref noop
	  
    fun start () =
         (start_fun := noop;
	  pthread_start 4000000)	(* 4 ms = 4 * 10^6 ns *)

    val _ = start_fun := start

   in (* inner local *)

    fun gettime () =
	 (! start_fun ();
	  if pthread_executed () then time := V.Time.now () else ();
	  ! time)

    val time_has_changed = pthread_executed

   end (* inner local *)
*)

(*
	This code makes gettime the same as V.Time.now.
*)
   val gettime = V.Time.now

   fun time_has_changed _ = true
(*
	This code calls V.Time.now at most once every N calls.  This lets us
	run faster, at the cost of possibly inaccurate timings.  For
	accurate timings, set max_calls to 1.
*)

(*
   local				(* local to gettime. *)
    val max_calls = 100 (* 100 *)

    val time = ref (V.Time.now ())
    val count = ref max_calls			(* update on first call. *)
   in
    fun gettime () =
         (if ! count >= max_calls then
	   (time := V.Time.now ();
	    count := 1)
	  else
	   count := ! count + 1;
	  ! time)
   end
*)

(*
	3.	local implementation of Fifo

	This implementation is included here so the compiler can
	in-line it, for speed.
*)

   type 'a fifo = 'a list * 'a list

   fun fifo_new () = ([], [])

   fun fifo_size (fwd, back) = length fwd + length back

   fun fifo_add ((fwd, back), elt) = (fwd, elt :: back)

   fun fifo_next (h :: r, back) = SOME ((r, back), h)
     | fifo_next ([], []) = NONE
     | fifo_next ([], [single]) = SOME (([], []), single)
     | fifo_next ([], back) =
        case rev back of h :: rest => SOME ((rest, []), h) | _ => NONE

(*
	4.	types and state
*)

   datatype coroutine_type = Regular_Coroutine
                           | Limited_Time_Coroutine

   datatype ready = Fun of (unit -> unit) | Cont of (unit V.Control.cont)

   type sleep = V.Time.time * unit V.Control.cont

   fun sleep_less ((t1, _), (t2, _)) = V.Time.< (t1, t2)

   structure Sleep = Priority_Queue (type key = sleep
				     val less = sleep_less)

   type handler = device -> unit
   fun eq_desc (d1, d2: device) = d1 = d2
   fun hash_desc d = Word.fromLargeWord (Posix.FileSys.fdToWord d)
   structure Handlers = Single_Store (structure V = V
				      type key = device
				      type value = handler * device
				      val eq = eq_desc
				      val hash = hash_desc)

   (* incarnation numbers increase monotonically from 1. An incarnation
      of zero is reserved as being invalid. *)
   val incarnation: int ref = ref 1
   val ready_queue: ready fifo ref = ref (fifo_new ())
   val sleep_queue: Sleep.T = Sleep.new ()
   val current: coroutine_type ref = ref Regular_Coroutine
   val poll_arguments = ref ([]: OS.IO.poll_desc list)

   val poll_iods = ref ([]: OS.IO.poll_desc list)

   fun makestring_state () =
        "generation " ^ Integer.toString (! incarnation) ^
	", ready " ^ Integer.toString (fifo_size (! ready_queue)) ^
	", handlers " ^ Integer.toString (Handlers.size ()) ^
	" = " ^ Integer.toString (List.length (! poll_iods)) ^
	", sleeping " ^ Integer.toString (Sleep.size sleep_queue)

(*
	5.	internal function null_thread

	null_thread is called when the coroutine system has no ready
	threads and waits for sleeping threads to wake up.
	null_thread returns right away so we can check the clock
	and see if it's time to wake someone up.
*)

   fun null_thread n = ()

  in (* local *)

(*
	6.	function reset
*)

   fun reset () =
        (incarnation := ! incarnation + 1;
	 ready_queue := fifo_new ();
	 Handlers.remove_selected (fn _ => true);
	 poll_iods := [];
	 while Sleep.size sleep_queue > 0 do Sleep.pop sleep_queue;
	 current := Regular_Coroutine)

(*
	7.	function fork

	Enqueue the function f.
*)

   fun fork f =
(* in-line the call to fifo_add, for speed. The original is:
	ready_queue := fifo_add (! ready_queue, f)
 *)
        let val (front, back) = ! ready_queue
	in ready_queue := (front, (Fun f :: back))
	end

(*
	8.	internal function coroutine_exn
*)

   local

    fun coroutine_exn x =
         (Trace.print_handled (x, SOME "thread");
	  case x of			(* avoid infinite loops. *)
	     No_Ready_Thread => Trace.print_raise (x, NONE)
	   | No_Such_Suspension => Trace.print_raise (x, NONE)
	   | _ => ())

(*
	9.	error functions internal to exit
*)

    fun no_ready_thread () =
         Trace.print_raise (No_Ready_Thread,
			    SOME ("exit (" ^ makestring_state () ^ ")"))

(*
	10.	internal function call_handler
*)

(*
    fun call_handler device =
         let val fd = Posix.FileSys.wordToFD (Word32.fromInt device)
	 in case Handlers.look fd of
	       NONE => Trace.local_print "returned iodesc not in Handlers"
	     | SOME (h, d) => (Trace.local_print ("Calling handler " ^
		                                   Word.toString device);
		               h d)
	 end
*)
    fun call_handler poll_info =
         let val pd = OS.IO.infoToPollDesc poll_info
             val iod = OS.IO.pollToIODesc pd
	 in case Posix.FileSys.iodToFD iod of
	       NONE => Trace.local_print "returned iodesc not fd"
	     | SOME fd =>
		(case Handlers.look fd of
		  NONE => Trace.local_print "returned iodesc not in Handlers"
		| SOME (h, d) => ( (* print "."; *)
		                  h d))
	 end

(*
	11.	internal function call_ready
*)

    fun call_ready sleep_time =
	 case fifo_next (! ready_queue) of
	    SOME (new_ready, coroutine) =>
	     (case OS.IO.poll (! poll_iods, SOME Time.zeroTime) of
	         [] =>			(* nothing waiting *)
		  (ready_queue := new_ready;
		   current := Regular_Coroutine;
		   case coroutine of 
	              Fun f => f ()
		    | Cont c => V.Control.throw c ())
	       | (first :: _) =>	(* call handler instead of next *)
		  call_handler first)
	  | NONE =>
	     (case OS.IO.poll (! poll_iods, SOME sleep_time) of
	         [] => ()
	       | (first :: _) =>
		  call_handler first)

(*
 
	12.	internal function wakeup

	Wake up all the sleeping processes that need to be awake now.
	Return the interval from now until the next process will be
	ready to wake up, or zero if there are no sleeping processes.
*)

    local
     fun maketime milliseconds =
          Time.fromMilliseconds (Int32.fromInt (Int.min (milliseconds, 100)))

     fun wakeup_loop (current_time, wakeup_time, continuation) =
          if V.Time.< (current_time, wakeup_time) then
           maketime (V.Time.deltams (wakeup_time, current_time))
          else
           (ready_queue := fifo_add (! ready_queue,
                                     Cont continuation);
            Sleep.pop sleep_queue;
            case Sleep.first sleep_queue of
               NONE => Time.zeroTime
             | SOME (t, c) => wakeup_loop (current_time, t, c))
    in
     fun wakeup () =
          if time_has_changed () then
           case Sleep.first sleep_queue of
              NONE => Time.zeroTime
	    | SOME (t, c) => wakeup_loop (gettime (), t, c)
	  else Time.zeroTime
    end

(*
	13.	function exit

	Discard the current thread and start the next one if any,
	after waking up any sleeping threads.

	Note that both wakeup and call_ready may modify all the queues,
	so do not trust any cached values.
*)

   in (* local *)
    val exit_cont: unit V.Control.cont option ref  = ref NONE

    (* schedule_once_and_for_all sets exit_cont to a continuation which
       invokes some thread and handles any exceptions raised.  Throwing
       to this continuation eliminates the problem of increasing handle
       stacks that occurs with a recursive implementation of exit. *)
    fun schedule_once_and_for_all () =
         (V.Control.callcc (fn c => (exit_cont := SOME c));
	  (call_ready (wakeup ())
	   handle x => coroutine_exn x);
          exit ())

    and exit () =
         (case ! exit_cont of
	     NONE => schedule_once_and_for_all ()
           | SOME c => V.Control.throw c ())

   end (* local *)

(*
	14.	function sleep

	suspend this thread for at least the specified number of
	milliseconds; immediately begin executing any coroutines
	on the ready queues.
*)

   fun sleep ms =
        let fun add_sleep c =
		 (Sleep.add (sleep_queue, (V.Time.addms (gettime (), ms), c));
		  exit ())
        in V.Control.callcc add_sleep
        end

(*
	15.	function suspend

	Note that the incarnation is copied into the suspension,
	so we can invalidate the suspension when we resume it.
*)

   fun suspend f =
        V.Control.callcc (fn cc =>
			  (f (Suspension (cc, ref (! incarnation)));
			   exit ()))

(*
	16.	function resume
*)

   fun resume (Suspension (_, ref 0), _) =
        Trace.print_raise (No_Such_Suspension, SOME "resume")
     | resume (Suspension (cc, valid), value) =
        if ! valid = ! incarnation then
	 (valid := 0;
	  fork (fn _ => V.Control.throw cc value);
	  ())
	else
	 Trace.print_raise (No_Such_Suspension,
			    SOME "resuming resumed suspension")

(*
	17.	function yield
*)

   fun yield () =
        suspend (fn s => resume (s, ()))

(*
	18.	function install_handler
*)

  fun install_handler (device, handler) =
       (Trace.debug_print (fn _ => "installing handler for " ^
			   Word32.toString (Posix.FileSys.fdToWord device));
	case (Handlers.look device,
	      OS.IO.pollDesc (Posix.FileSys.fdToIOD device)) of
	   (NONE, SOME pd) =>			(* install the handler *)
	    (Handlers.add (device, (handler, device));
	     poll_iods := OS.IO.pollIn pd :: ! poll_iods)
	 | (SOME _, _) =>
	    Trace.print_raise (Device_Handler_Already_Installed,
			       SOME "install_handler")
	 | (NONE, NONE) =>
	    Trace.print_raise (Unable_To_Convert_IOD_to_Poll_Desc,
			       SOME "install_handler"))

(*
	19.	function uninstall_handler
*)

  fun uninstall_handler device =
       let val pd = case OS.IO.pollDesc (Posix.FileSys.fdToIOD device) of
	               NONE =>
			Trace.print_raise (Unable_To_Convert_IOD_to_Poll_Desc,
					   SOME "uninstall_handler")
		| SOME pd => pd
	   val dev = OS.IO.pollIn pd
	   fun remove [] = []
	     | remove (first :: rest) =
	        if first = dev then rest
		else first :: (remove rest)
(* order is important: remove from the poll arguments BEFORE removing
   from the handlers, otherwise call_handler might report an error. *)
       in Trace.debug_print (fn _ => "removing handler for " ^
			     Word32.toString (Posix.FileSys.fdToWord device));
          poll_iods := remove (! poll_iods);
	  Handlers.remove device
       end

  end (* local *)

 end (*struct*)
