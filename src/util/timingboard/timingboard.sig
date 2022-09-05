(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract
	
	A signature for the timingboard, it is just a 4 byte array that
 is read with FoxWord32.sub.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TIMINGBOARD

		iii.	RCS Log
	
$Log: timingboard.sig,v $
Revision 1.6  1994/09/30  16:35:19  esb
changed Byte4 to FoxWord32

Revision 1.5  1994/04/26  17:52:06  esb
added a count of times each counter is started.

Revision 1.4  94/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file
move problems that confused rcs.

Revision 1.2  1994/01/30  21:17:37  milnes
Changed the initialization to allow for running heaps that
use the timingboards.

Revision 1.1  1994/01/14  12:29:35  milnes
Initial revision

		1.	signature TIMINGBOARD
*)

signature TIMINGBOARD =
 sig
  val initialize : unit -> unit 
  val installed : bool ref
  (* True if the machine has a timing board and the function has been
     applied to turn it on. *)

  (* When the timing board is installed, this is an array that when 
     read with a Byte4.Unsafe_U.sub (array, 0) reads the timing board 
     register. As the tag word before this array is not correct, do
     not pass this to any polymorphic operation. 

     If the board is not installed this is a normal byte array initialized
     to zero. *)
  val array: ByteArray.bytearray ref

  (* An abstract type that holds the name of a counter, The counter's
     sum, the current value, and the number of times the counter has been
     started may be read. *)
  type counter

  val name: counter -> string
  val sum: counter -> FoxWord32.word
  val current: counter -> FoxWord32.word
  val running: counter -> bool
  val count: counter -> FoxWord32.word

  (* Add a new counter named with the string, returns the counter. *)
  val add: string -> counter

  (* Get a counter's byte array by name. *) 
  exception Counter_Not_Found of string 
  val get: string -> counter

  (* Zero all of the listed counters, stopping all running counters. *)
  val zero_counters: unit -> unit    

  (* Zero all stopped counters, but if one is running restart it. *)
  val zero_or_restart_counters: unit -> unit    

  (* Empty the list of counters. *)
  val reset: unit -> unit

  (* Make a string for a counter. *)
  val makestring_counter: counter -> string

  (* Make a string of timers sorted by size of timer clicks. *)
  val makestring_counters: unit -> string

  (* Start a timer running. *)
  val start: counter -> unit

  (* Stop a timer running.  *)
  val stop: counter -> unit

  (* Time on a counter a thunk's (fn () => ...) application to (). *)
  val time: counter * (unit -> 'a) -> 'a 
 end


