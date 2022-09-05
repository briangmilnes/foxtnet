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

	Test code for the coroutine package.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Write_Once
	2.	structure Test_Write_Once

		iii.	RCS Log
	
$Log: writeonce.tst,v $
Revision 1.1  1995/08/16  21:26:05  esb
Initial revision


		1.	functor Test_Write_Once
*)

functor Test_Write_Once (structure B: FOX_BASIS
			 val debug_level: int ref option): TEST_STRUCTURE =
 struct

  fun run_tests () = 
       let val synch = B.Pipe.new ()
	   fun read (name, expected, v) () =
	        (B.Test.test (name, fn _ => B.Write_Once.get v = expected);
		 B.Pipe.enqueue (synch, ()))
	   fun count 0 = ()
	     | count n =
	        (B.Pipe.dequeue synch;
		 count (n - 1))
	   val v1 = B.Write_Once.new ()
	   val v2 = B.Write_Once.new ()
	   val v3 = B.Write_Once.new ()
       in B.Scheduler.fork (read ("never called", (), v3));
          B.Scheduler.fork (read ("before assignment", 1, v1));
          B.Scheduler.fork (read ("before assignment, again", 1, v1));
	  B.Write_Once.set (v1, 1);
          B.Scheduler.fork (read ("after assignment", 1, v1));
          B.Scheduler.fork (read ("after assignment, again", 1, v1));
	  B.Scheduler.fork (read ("never called", (), v3));
	  ((B.Write_Once.set (v1, 2);
	    B.Test.test ("set twice", fn _ => false))
	   handle B.Write_Once.Already_Initialized =>
	           B.Test.test ("did not write twice", fn _ => true));
	  B.Write_Once.set (v2, "hello world");
          B.Scheduler.fork (read ("after init", "hello world", v2));
	  B.Scheduler.fork (read ("never called", (), v3));
	  count 5
       end

  fun run () = B.Test.tests ("Write-Once", 6, run_tests)

  val _ = if ! B.Debug.do_tests then run () else ()

 end 

(*
		2.	structure Test_Write_Once
*)

structure Test_Write_Once = Test_Write_Once (structure B = Fox_Basis
					     val debug_level = NONE)
 
