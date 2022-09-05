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



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor StopWatch

		iii.	RCS Log
	
$Log: stopwatch.fun,v $
Revision 1.4  1996/03/05  21:11:38  derby
changed the stopwatch to take different kinds of time and measures.

Revision 1.3  1996/01/16  21:59:12  derby
Removed the dependance on the Timing structure and replaced it
with the vendor equiv.

Revision 1.2  1994/03/03  16:56:53  esb
added header.


		1.	functor StopWatch
*)

functor StopWatch (  
            structure Time : TIMEBASE
            val measure : unit -> Time.time
                   ) : STOPWATCH  =
   
   struct

      type time = Time.time

      datatype stopwatch_value =
         Stop of {total : Time.time, lap : Time.time}
       | Run of {started_at : Time.time, lap : Time.time}
         
      type stopwatch = stopwatch_value ref
         
      fun since t = Time.- (measure(), t)
         
      fun stopwatch () = ref (Stop {total = Time.zero,
                                    lap = Time.zero})

      exception Stopped
      exception Running

      fun start (s as ref (Run _)) = raise Running
        | start (s as ref (Stop{total, lap})) =
         s := Run {lap = lap,
                   started_at = since(total)}

      fun stop (s as ref (Stop _)) = raise Stopped
        | stop (s as ref (Run{lap, started_at})) = 
         s := Stop{lap = lap,
                   total = since(started_at)}

      fun lap (s as ref (Stop {lap, total})) =
         s := Stop {lap = total, total = total}
        | lap (s as ref (Run {lap, started_at})) =
         s := Run {lap = since(started_at),
                   started_at = started_at}
         
      fun reset (s as ref (Stop _)) = 
         s := Stop{lap = Time.zero, total = Time.zero}
        | reset (s as ref (Run _)) =
         s := Run{lap = Time.zero, started_at = measure()}
         
      fun lap_time (ref (Stop{lap,...})) = lap
        | lap_time (ref (Run {lap,...})) = lap
         
      fun time (ref (Stop{total,...})) = total
        | time (ref (Run{started_at,...})) = since(started_at)
         
      fun running (ref (Run _)) = true
        | running (ref (Stop _)) = false
         
   end (* StopWatch *)

