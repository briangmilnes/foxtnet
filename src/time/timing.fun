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
	1.	functor Timing

		iii.	RCS Log
	
$Log: timing.fun,v $
Revision 1.4  1995/03/10  03:53:01  esb
adapted to new vendor.sig.

Revision 1.3  1994/03/03  16:56:53  esb
added header.


		1.	functor Timing
*)

functor Timing (structure Time: TIME
		structure Vendor: VENDOR): TIMING = 
 struct

  structure Time = Time

  type timing = {real: Time.time,
		 user: Time.time,
		 sys: Time.time,
		 gc: Time.time}

  fun real_time ({real,...}: timing) = real
  fun user_time ({user,...}: timing) = user
  fun sys_time ({sys,...}: timing) = sys
  fun gc_time ({gc,...}: timing) = gc

  local

   fun time_to_timing1 f ({real, user, sys, gc}, a) =
    {real = f (real, a), user = f (user, a),
     sys = f (sys, a), gc = f (gc, a)}

   fun time_to_timing2 f ({real=r1, user=u1, sys=s1, gc=g1},
			  {real=r2 ,user=u2 ,sys=s2, gc=g2}) =
    {real = f (r1, r2), user = f (u1, u2),
     sys = f (s1, s2), gc = f (g1, g2)}
		
  in val (op div) = time_to_timing1 (Time.div)
     val (op * )  = time_to_timing1 (Time.* )
     val (op - )  = time_to_timing2 (Time.- )
     val (op + )  = time_to_timing2 (Time.+ )
  end

  fun now () =
   let val {usr as Vendor.Time.TIME {sec = us, usec = uus},
	    sys as Vendor.Time.TIME {sec = ss, usec = sus},
	    gc as Vendor.Time.TIME {sec = gs, usec = gus}} =
                Vendor.Time.gettime()
       val real = Time.time_of_day()
       val user = Time.Time {sec = us, usec = uus}
       val sys = Time.Time {sec = ss, usec = sus}
       val gc = Time.Time {sec = gs, usec = gus}
   in {user = Time.- (user, gc), sys = sys, real = real, gc = gc}
   end

  val zero_timing = {real = Time.zero_time, user = Time.zero_time,
		     sys = Time.zero_time, gc = Time.zero_time}

  fun makestrings {real, user, sys, gc} =
   ["Real time: " ^ (Time.makestring real) ^ "\n",
    "User time: " ^ (Time.makestring user) ^ "\n",
    "Sys  time: " ^ (Time.makestring sys) ^ "\n",
    "G.C. time: " ^ (Time.makestring gc) ^ "\n"]

  fun makestrings_per (t, n, s) =
   if n = 0 then ("Time for zero " ^ s ^ "s:\n") :: (makestrings t)
   else ("Time per " ^ s ^ ":\n") :: (makestrings (t div n))

    end (* struct *)

