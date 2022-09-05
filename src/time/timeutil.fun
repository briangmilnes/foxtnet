(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Timing utilities.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor TimeUtil
              

		iii.	RCS Log
	
$Log: timeutil.fun,v $
Revision 1.5  1995/03/10  03:53:01  esb
adapted to new vendor.sig.

Revision 1.4  1994/09/30  16:37:20  esb
now use the vendor functions.

Revision 1.3  1993/09/13  22:08:01  cline
deleted '#'s from RCS log

Revision 1.2  1993/08/27  18:22:29  esb
cleaned up the comments.

Revision 1.1  1993/07/10  04:03:18  esb
Initial revision

Revision 1.1  1993/06/10  22:41:20  milnes
Initial revision


		1.	functor TimeUtil
*)

functor TimeUtil (structure V: VENDOR): TIMEUTIL =
 struct

  fun timeapp f x =
       let val start_t = V.Time.gettimeofday ()
	   val {usr = start_usr, sys = start_sys,
		gc = start_gc} = V.Time.gettime ()
           val result = f x
	   val stop_t = V.Time.gettimeofday ()
	   val {usr = stop_usr, sys = stop_sys,
		gc = stop_gc} = V.Time.gettime ()
	   val final_t = V.Time.deltams (stop_usr, start_usr)
	   val ts = V.Time.deltams (stop_sys, start_sys)
	   val tg = V.Time.deltams (stop_gc, start_gc)
	   val final_rt = V.Time.deltams (stop_t, start_t)
       in "Non-GC User Time: " ^ (makestring final_t) ^ " System Time: " ^
          (makestring ts) ^ " GC User Time: " ^ (makestring tg) ^
          " Real Time:" ^ (makestring final_rt)
       end

  fun ntimes 1 f x  = f x
    | ntimes n f x  = (f x; ntimes (n - 1) f x)

 end (* struct *)

