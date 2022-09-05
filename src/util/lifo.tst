(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Test code to test the Lifo functor.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Lifo
	2.      structure Test_Lifo

		iii.	RCS Log
	
$Log: lifo.tst,v $
Revision 1.4  1994/08/02  20:11:12  esb
adapted to uncurried test.sig.

Revision 1.3  1994/06/16  16:51:18  danwang
Updated for functorized Fox_Basis

Revision 1.2  1994/01/17  17:54:12  esb
Standardized the interface.

Revision 1.1  1993/06/10  22:41:20  milnes
Initial revision


		1.	functor Test_Lifo
*)

signature TEST_LIFO =
 sig
  val run : unit -> unit
 end
 
functor Test_Lifo (structure Debug : DEBUG
		   structure V : VENDOR
		   structure Test : TEST
		   val do_prints : bool): TEST_FIFO =
 struct

  structure Lifo = Lifo ()

  fun test_run () =
       let exception Empty
           val a0 = Lifo.new ()
           val b0 = Lifo.new ()
           val a1 = Lifo.push (a0, 3);
           val b1 = Lifo.push (b0, "xyz");
           val b2 = Lifo.push (b1, "xyzzy");
           val three = case Lifo.pop a1 of
	                  NONE => raise Empty
			| SOME (_, x) => x
           val a2 = case Lifo.pop a1 of NONE => raise Empty | SOME (s, _) => s
           val valid_empty = case Lifo.pop a2 of NONE => true | _ => false
           val xyzzy = case Lifo.pop b2 of
                          NONE => raise Empty
			| SOME (_, x) => x
           val b3 = case Lifo.pop b2 of NONE => raise Empty | SOME (s, _) => s
           val xyz = case Lifo.pop b3 of NONE => raise Empty | SOME (_, x) => x
           val b4 = case Lifo.pop b3 of NONE => raise Empty | SOME (s, _) => s
           val sizes = (map Lifo.size [a0, a1, a2],
		        map Lifo.size [b0, b1, b2, b3, b4])
           val expected_sizes =      ([ 0,  1,  0],
				      [ 0,  1,  2,  1,  0])
           fun prta (x: int) = makestring x
           fun prtb x = x
           val str0 = Lifo.makestring (a0, prta, ", ")
           val str1 = Lifo.makestring (a1, prta, ", ")
           val str2 = Lifo.makestring (b2, prtb, ", ")
       in Test.test ("pop", (fn _ => three = 3));
          Test.test ("pop copy", (fn _ => xyz = "xyz"));
	  Test.test ("pop copy again", (fn _ => xyzzy = "xyzzy"));
	  Test.test ("sizes", (fn _ => sizes = expected_sizes));
	  Test.test ("makestring empty", (fn _ => str0 = ""));
	  Test.test ("makestring non-empty", (fn _ => str1 = "3"));
	  Test.test ("makestring", (fn _ => str2 = "xyzzy, xyz"));
	  ()
       end

  fun run () = 
       if Debug.include_tests then Test.tests ("Lifo", 7, test_run)
       else ()

  val _ = if ! Debug.do_tests then run () else ()

 end (* struct *)

(*
		2. structure Test_Lifo
*)

structure Test_Lifo = Test_Lifo (structure Debug = Fox_Basis.Debug
				 structure V = TV
				 structure Test = Fox_Basis.Test
				 val do_prints = false)




