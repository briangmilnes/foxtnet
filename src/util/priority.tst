(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
        Nick Haines  (Nick.Haines@cs.cmu.edu)
        Brian Milnes (Brian.Milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

                i.      Abstract

        Test code to test the Priority_Queue functor.


                ii.     Table of Contents

        i.      Abstract
        ii.     Table of Contents
        iii.    RCS Log
        1.      functor Test_Priority
        2.      structure Test_Priority

                iii.    RCS Log

$Log: priority.tst,v $
Revision 1.11  1996/06/11  03:36:08  esb
adapted to new priority.sig.

Revision 1.10  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.9  1994/09/30  16:27:34  esb
changed to work for SML/NJ 1.05 as well as 0.93.

Revision 1.8  1994/08/02  20:11:12  esb
adapted to uncurried test.sig.

Revision 1.7  1994/06/16  17:00:15  danwang
Updated for functorized Fox_Basis

Revision 1.6  1994/01/17  17:54:12  esb
Standardized the interface.

Revision 1.5  1994/01/13  16:18:48  milnes
Updated on the path to updating coroutines with a delete.

Revision 1.4  93/09/13  22:08:00  cline
deleted '#'s from RCS log

Revision 1.3  1993/09/02  15:46:39  esb
minor changes.

Revision 1.2  93/07/20  15:05:27  esb
Changed the interface to "next" so returns option instead of exception.

Revision 1.1  1993/07/20  14:29:16  esb
Initial revision


                1.      functor Test_Priority
*)

functor Test_Priority (structure Debug: DEBUG
		       structure V: VENDOR
		       structure Test: TEST
		       val do_prints: bool): TEST_STRUCTURE =
 struct

  structure S = Priority_Queue (type key = int val less = Integer.<)

  fun run_test () =
       let fun heaptolist h = S.fold (fn (a,  b) => b @ [a]) h []
           fun listequal ([], []) = true
             | listequal (_, []) = false
	     | listequal ([], _) = false
             | listequal (h1 :: l1, h2 :: l2) =
	        (h1 = h2 andalso listequal (l1, l2))
	   fun t (NONE, []) = true
	     | t (NONE, a) = false
	     | t (SOME h, a) = listequal (heaptolist h, a)
	   val q1 = S.new ()
	   val q2 = S.new ()
	   val d55 = S.add (q1, 55);
	   val d33 = S.add (q1, 33);
	   val d66 = S.add (q1, 66);
	   val d22 = S.add (q1, 22);
	   val d19 = S.add (q1, 19);
	   val d3 = S.add (q2, 3);
	   val d2 = S.add (q2, 2);
	   val d1 = S.add (q2, 1);
	   val d0 = S.add (q2, 0);
       in Test.test ("1", (fn () => case S.next (S.new ()) of
		                       NONE => true
				     | SOME _ => false));
	  Test.test ("2", (fn () => listequal (heaptolist q1, 
					       [19, 22, 33, 55, 66])));
	  Test.test ("3", (fn () => S.makestring (q2, Integer.toString, "-") = 
			            "0-1-2-3"));
	  Test.test ("4", (fn () => 0 = S.size (S.new ())));
	  Test.test ("5", (fn () => 4 = S.size q2));
	  Test.test ("6", (fn () => S.empty (S.new ())));
	  Test.test ("7", (fn () => not (S.empty q1)));
	  Test.test ("8", (fn () => S.first q1 = SOME 19));
	  Test.test ("9", (fn () => S.first q2 = SOME 0));
	  d33 ();
	  d22 ();
	  Test.test ("10", (fn () => S.makestring (q1, Integer.toString,
						   "-") = "19-55-66"));
	  d19 ();
	  Test.test ("11", (fn () => S.first q1 = SOME 55));
	  S.add (q1, 88);
	  S.add (q1, 44);
	  Test.test ("12", (fn () => S.first q1 = SOME 44))
       end

  fun run () =
       if Debug.include_tests then Test.tests ("Priority", 12, run_test)
       else ()

  val _ = if ! Debug.do_tests then run () else ()

 end (* struct *)

(*
                1.      structure Test_Priority
*)

structure Test_Priority = Test_Priority (structure Debug = Fox_Basis.Debug
					 structure V = Fox_Basis.V
					 structure Test = Fox_Basis.Test
					 val do_prints = false)

