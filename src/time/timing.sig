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
	1.	signature TIMING

		iii.	RCS Log
	
$Log: timing.sig,v $
Revision 1.2  1994/03/03  16:56:53  esb
added header.


		1.	signature TIMING
*)

signature TIMING = 
    sig

	structure Time : TIME

	type timing

	val real_time : timing -> Time.time
	val user_time : timing -> Time.time
	val sys_time : timing -> Time.time
	val gc_time : timing -> Time.time

	val - : timing * timing -> timing
	val + : timing * timing -> timing
	val * : timing * int -> timing
	val div : timing * int -> timing

	val now : unit -> timing

	val zero_timing : timing

	val makestrings : timing -> string list
	val makestrings_per : timing * int * string -> string list
    end

