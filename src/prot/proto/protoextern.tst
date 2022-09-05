(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Ken Cline (ken.cline@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Test code for protocol externals.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Shrink_Array
	2.	function run_test
	3.	function run
	4.	structure Test_Protocol_External

		iii.	RCS Log
	
$Log: protoextern.tst,v $
Revision 1.3  1996/04/30  20:29:25  esb
adapted to new seq.sig, specifying words instead of ints.

Revision 1.2  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.1  1995/06/20  17:09:33  esb
Initial revision


---------------------------------------------------------------------
	1.	functor Test_Shrink_Array
*)

functor Test_Protocol_External
             (structure E: EXTERNAL
              structure B: FOX_BASIS
              val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "protoextern.tst"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print

(*
---------------------------------------------------------------------
	2.	function run_test
*)

  structure Word_Array8 = Word_Array.W8.Native.F
  val create = Word_Array.from8 o Word_Array8.create 

  fun run_test () =
       let fun sub (array, default) =
	        case Word_Array8.next (Word_Array.to8 array) of
		   NONE => Word8.fromInt default
		 | SOME (head, rest) => head
	   val a1 = E.new (create (0w1, 0w10000))
	   val a2 = E.new (create (0w2, 0w1))
	   val a3 = E.new (create (0w3, 0w99))
	   val a4 = E.new (create (0w4, 0w4))
	   val s1 = sub (E.sub (a1, {start = 0w0, length = 0w1}), 0)
	   val s2 = sub (E.sub (a2, {start = 0w0, length = 0w1}), 0)
	   val s3 = ((sub (E.sub (a3, {start = 0w0, length = 0w100}), 0))
		     handle _ => Word8.fromInt 0)
	   val s4 = sub (E.sub (a4, {start = 0w3, length = 0w1}), 0)
	   val u1 = E.update (a1, 0w9999, create (0w6, 0w1))
	   val u2 = E.update (a2, 0w0, create (0w7, 0w1))
	   val u3 = E.update (a3, 0w1, create (0w8, 0w1))
	   val u4 = E.update (a4, 0w2, create (0w9, 0w1))
	   val s5 = sub (E.sub (a1, {start = 0w9999, length = 0w1}), 0)
	   val s6 = sub (E.sub (a2, {start = 0w0, length = 0w1}), 0)
	   val s7 = ((sub (E.sub (a3, {start = 0w99, length = 0w100}), 0))
		     handle _ => Word8.fromInt 0)
	   val s8 = sub (E.sub (a4, {start = 0w2, length = 0w1}), 0)
	   val j1 = E.join (a1, a2)
	   val j2 = E.join (a2, a3)
	   val j3 = E.join (a3, a4)
	   val j4 = E.join (j1, j3)
	   val (sp1a, sp1b) = E.split (a1, 0w10)
	   val (sp2a, sp2b) = E.split (a2, 0w0)
	   val (sp3a, sp3b) = E.split (a3, 0w99)
	   val (sp4a, sp4b) = E.split (j1, 0w100)
	   val (sp5a, sp5b) = E.split (j2, 0w99)
	   val (sp6a, sp6b) = E.split (j3, 0w100)
	   val (sp7a, sp7b) = E.split (j4, 0w1)
	   val sp8 = ((E.split (a3, 0w100); false) handle _ => true)
	   fun isbyte (n, byte) = byte = Word8.fromInt n
       in B.Test.test ("a1", fn _ => E.size a1 = 0w10000);
          B.Test.test ("a2", fn _ => E.size a2 = 0w1);
          B.Test.test ("a3", fn _ => E.size a3 = 0w99);
          B.Test.test ("a4", fn _ => E.size a4 = 0w4);
          B.Test.test ("s1", fn _ => isbyte (1, s1));
          B.Test.test ("s2", fn _ => isbyte (2, s2));
          B.Test.test ("s3", fn _ => isbyte (0, s3));
          B.Test.test ("s4", fn _ => isbyte (4, s4));
          B.Test.test ("s5", fn _ => isbyte (6, s5));
          B.Test.test ("s6", fn _ => isbyte (7, s6));
          B.Test.test ("s7", fn _ => isbyte (0, s7));
          B.Test.test ("s8", fn _ => isbyte (9, s8));
	  B.Test.test ("j1", fn _ => E.size j1 = 0w10001);
          B.Test.test ("j2", fn _ => E.size j2 = 0w100);
          B.Test.test ("j3", fn _ => E.size j3 = 0w103);
          B.Test.test ("j4", fn _ => E.size j4 = 0w10104);
          B.Test.test ("sp1a", fn _ => E.size sp1a = 0w10);
          B.Test.test ("sp1b", fn _ => E.size sp1b = 0w10000 - 0w10);
          B.Test.test ("sp2a", fn _ => E.size sp2a = 0w0);
          B.Test.test ("sp2b", fn _ => E.size sp2b = 0w1);
          B.Test.test ("sp3a", fn _ => E.size sp3a = 0w99);
          B.Test.test ("sp3b", fn _ => E.size sp3b = 0w0);
          B.Test.test ("sp4a", fn _ => E.size sp4a = 0w100);
          B.Test.test ("sp4b", fn _ => E.size sp4b = 0w10001 - 0w100);
          B.Test.test ("sp5a", fn _ => E.size sp5a = 0w99);
          B.Test.test ("sp5b", fn _ => E.size sp5b = 0w1);
          B.Test.test ("sp6a", fn _ => E.size sp6a = 0w100);
          B.Test.test ("sp6b", fn _ => E.size sp6b = 0w3);
          B.Test.test ("sp7a", fn _ => E.size sp7a = 0w1);
          B.Test.test ("sp7b", fn _ => E.size sp7b = 0w10104 - 0w1);
          B.Test.test ("sp8", fn _ => sp8)
       end

(*
---------------------------------------------------------------------
	3.	function run
*)

  fun run () = 
       if B.Debug.include_tests then
        B.Test.tests ("protocol external", 31, run_test)
       else ()

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
---------------------------------------------------------------------
	4.	structure Test_Protocol_External
*)

structure Test_Protocol_External =
    Test_Protocol_External (structure E =
			     Protocol_External (structure B = Fox_Basis
						val debug_level = NONE)
			    structure B = Fox_Basis
			    val debug_level = SOME (ref 3))
