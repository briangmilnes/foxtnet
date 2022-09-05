(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Sidd Puri (sidd@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	writeonce.sig: implementation of write-once variables.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature WRITE_ONCE

		iii.	RCS Log

$Log: writeonce.fun,v $
Revision 1.3  1996/02/06  22:07:34  esb
added Memoize.

Revision 1.2  1995/08/16  21:31:55  esb
added comment.

Revision 1.1  1995/08/16  21:26:05  esb
Initial revision


		1.	functor Write_Once

	A write-once variable can be written to exactly once.
	Any access to the variable before it is written is
	suspended, and resumed once the variable is written.
*)

functor Write_Once (structure V: VENDOR
		    structure Scheduler: COROUTINE): WRITE_ONCE =
 struct
  datatype 'a write_once = Initialized of 'a
                         | Uninitialized of 'a Scheduler.suspension list

  type 'a T = 'a write_once ref

  fun new () = ref (Uninitialized [])

  fun get var =
       case ! var of
	  Initialized v => v
	| Uninitialized list =>
	   Scheduler.suspend (fn s => var := Uninitialized (s :: list))

  exception Already_Initialized

  local
   fun resume_value value suspension =
        Scheduler.resume (suspension, value)

  in
   fun set (var, value) =
        case ! var of
	 Initialized _ => raise Already_Initialized
       | Uninitialized list =>
	  (var := Initialized value;
	   V.List.app (resume_value value) list)
  end

 end

(*
		2.	functor Memoize

	A memo-ized variable is initialized at the time it
	is first needed, and never recomputed.
*)

functor Memoize (): MEMOIZE =
 struct
  datatype ('a, 'b) value = Lazy of ('a -> 'b) * 'a | Done of 'b

  type ('a, 'b) T = ('a, 'b) value ref

  fun new (f, a) = ref (Lazy (f, a))

  fun get (ref (Done result)) = result
    | get (x as (ref (Lazy (f, a)))) =
       let val result = (f a)
       in x := Done result;
	  result
       end

 end
