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
	1.	signature TABULATE
	2.	functor Tabulate

		iii.	RCS Log
	
$Log: tabulate.sml,v $
Revision 1.4  1994/06/17  13:57:15  danwang
Removed Tabulate structure.

Revision 1.3  1994/05/04  01:37:50  esb
rewrote to be tail recursive so it needs less space.

Revision 1.2  94/03/02  21:39:51  esb
added RCS log, table of contents.


		1.	signature TABULATE
*)




signature TABULATE = 
 sig
  val tabulate : (int -> 'a) * int -> 'a list
 end (* sig *)

(*
		2.	functor Tabulate
*)

functor Tabulate () : TABULATE =
 struct

(* this function uses an accumulator so the compiler can
   do tail-recursion optimization, otherwise the creation of very
   large loops (> 2M elements) can easily run out of memory.
   The list is built in reverse order, end first. *)
  fun tabulate (f, n) = 
   let fun loop (i, accumulator) =
            if i = 0 then accumulator
	    else loop (i - 1, f (i - 1) :: accumulator)
   in loop (n, [])
   end

 end (* struct *)


