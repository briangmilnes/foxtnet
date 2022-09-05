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

	Test code to test the Fifo functor.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Fifo
	2.      structure Test_Fifo

		iii.	RCS Log
	
$Log: fifo.tst,v $
Revision 1.6  1995/06/20  17:37:26  esb
minor fix.

Revision 1.5  1994/08/02  20:11:12  esb
adapted to uncurried test.sig.

Revision 1.4  1994/06/16  16:51:18  danwang
Updated for functorized Fox_Basis

Revision 1.3  1994/01/17  17:54:12  esb
Standardized the interface.

Revision 1.2  1993/10/12  22:36:09  esb
changed order of arguments so the fifo is always the first argument.

Revision 1.1  1993/06/10  22:41:20  milnes
Initial revision


		1.	functor Test_Fifo
*)

signature TEST_FIFO =
 sig
  val run: unit -> unit
 end
 
functor Test_Fifo (structure Debug: DEBUG
		   structure V: VENDOR
		   structure Test: TEST
		   val do_prints: bool): TEST_FIFO =
 struct

  structure Fifo = Fifo (structure V = V)

  fun test_run () =
   let exception Empty
       val a0 = Fifo.new ()
       val b0 = Fifo.new ()
       val a1 = Fifo.add (a0, 3);
       val b1 = Fifo.add (b0, "xyz");
       val b2 = Fifo.add (b1, "xyzzy");
       val (a2, three) = case Fifo.next a1 of NONE => raise Empty | SOME x => x
       val valid_empty = case Fifo.next a2 of NONE => true | SOME _ => false
       val (b3, xyz) = case Fifo.next b2 of NONE => raise Empty | SOME x => x
       val (b4, xyzzy) = case Fifo.next b3 of NONE => raise Empty | SOME x => x
       val sizes = (map Fifo.size [a0, a1, a2],
		    map Fifo.size [b0, b1, b2, b3, b4])
       val expected_sizes =      ([ 0,  1,  0],
				  [ 0,  1,  2,  1,  0])
       fun prta (x: int) = makestring x
       fun prtb x = x
       val str0 = Fifo.makestring (a0, prta, ", ")
       val str1 = Fifo.makestring (a1, prta, ", ")
       val str2 = Fifo.makestring (b2, prtb, ", ")
   in Test.test ("next", (fn _ => three = 3));
      Test.test ("next copy", (fn _ => xyz = "xyz"));
      Test.test ("next copy again", (fn _ => xyzzy = "xyzzy"));
      Test.test ("sizes", (fn _ => sizes = expected_sizes));
      Test.test ("makestring empty", (fn _ => str0 = ""));
      Test.test ("makestring non-empty", (fn _ => str1 = "3"));
      Test.test ("makestring", (fn _ => str2 = "xyz, xyzzy"));
      ()
   end

  fun run () = 
   if Debug.include_tests then
    Test.tests ("Fifo", 7, test_run)
   else ()

  val _ = if ! Debug.do_tests then run () else ()

 end (* struct *)

(*
		2. structure Test_Fifo
*)

structure Test_Fifo = Test_Fifo (structure Debug = Fox_Basis.Debug
				 structure V = Fox_Basis.V
				 structure Test = Fox_Basis.Test
				 val do_prints = false)




