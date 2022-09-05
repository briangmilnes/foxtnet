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
	1.	signature FOX_TIME

		iii.	RCS Log
	
$Log: time.sig,v $
Revision 1.7  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.6  1995/09/17  20:51:48  cstone
Added split_time

Revision 1.5  1995/06/20  17:39:42  esb
minor fix.

Revision 1.4  1995/01/21  14:38:40  esb
added make_date.

Revision 1.3  1994/03/03  16:56:53  esb
added header.


		1.	signature FOX_TIME
*)

signature FOX_TIME =
 sig
  datatype time = Time of {sec: int, usec: int}

  val - : time * time -> time
  val + : time * time -> time
  val * : time * int -> time

  val div: time * int -> time

  val < : time * time -> bool
  val > : time * time -> bool

  val time_to_real: time -> real
  val real_to_time: real -> time

  val makestring: time -> string
  val make_date: time -> string
 
  val split_time: int (* number of seconds *) ->
       {years: int,
	months: int,
	days: int,
	weekdays: int,
	hours : int,
	minutes: int,
	seconds: int}

  val time_of_day: unit -> time

  val zero_time: time
 end

