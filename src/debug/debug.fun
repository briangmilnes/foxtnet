(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

             A TEST signature that controls the installation of test

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Debug

		iii.	RCS Log
	
$Log: debug.fun,v $
Revision 1.1  1993/06/10  22:55:45  milnes
Initial revision


		1.	functor Debug
*)

functor Debug(val include_tests   : bool
              val do_tests        : bool
              val include_prints  : bool
              val do_prints       : bool
              val include_timings : bool 
              val do_timings      : bool
              ) : DEBUG =
 struct 
  (* Instantiate the testing functors with their test code. *)
  val include_tests = include_tests
  (* When the testing functors instantiate, actually run the test code. *)
  val do_tests = ref do_tests

  (* Instantiate the functors with their printing code. *)
  val include_prints = true
  (* When the code from the functors runs, have it print its
    messages. *)
  val do_prints = ref do_prints

  (* Instantiate the timing functors with their code. *)
  val include_timings = true
  (* When the timing functors are instantiated, actually run their timing code. *)
  val do_timings = ref do_timings
 end
  
