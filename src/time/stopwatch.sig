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
	1.	signature STOPWATCH

		iii.	RCS Log
	
$Log: stopwatch.sig,v $
Revision 1.4  1996/03/05  21:11:38  derby
changed the stopwatch to take different kinds of time and measures.

Revision 1.3  1996/01/16  21:58:02  derby
Removed the dependance on the Timing structure and replaced it
with the vendor equiv.

Revision 1.2  1994/03/03  16:56:53  esb
added header.


		1.	signature STOPWATCH
*)

signature STOPWATCH = 
   sig

      type time

      type stopwatch

      val stopwatch : unit -> stopwatch

      exception Stopped
      exception Running

      val start : stopwatch -> unit
      val lap : stopwatch -> unit
      val stop : stopwatch -> unit
      val reset : stopwatch -> unit

      val running : stopwatch -> bool

      val lap_time : stopwatch -> time
      val time : stopwatch -> time

    end

