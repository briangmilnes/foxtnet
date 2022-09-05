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

	Test code to test the Store utility.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Store
	2.      structure Test_Store

		iii.	RCS Log
	
$Log: store.tst,v $
Revision 1.9  1996/04/30  20:22:30  esb
changed to suit new interfaces.

Revision 1.8  1995/06/20  17:37:38  esb
minor fix.

Revision 1.7  1995/03/07  20:25:01  esb
eliminated timing board.

Revision 1.6  1994/08/02  20:11:12  esb
adapted to uncurried test.sig.

Revision 1.5  1994/06/16  16:51:43  danwang
Updated for functorized Fox_Basis

Revision 1.4  1994/05/10  07:42:11  esb
adapted to new store.sig.

Revision 1.3  94/04/06  23:06:42  esb
added tests for remove_selected.

Revision 1.2  94/01/17  17:54:12  esb
Standardized the interface.

Revision 1.1  1993/10/06  01:23:44  esb
Initial revision


		1.	functor Test_Store
*)

functor Test_Store (structure Store: STORE
		    structure Debug: DEBUG
		    structure V: VENDOR
		    structure Test: TEST
		    val do_prints: bool): TEST_STRUCTURE =
 struct

  val do_if_debug = if Debug.include_prints andalso do_prints then
                     (fn (f, x) => if ! Debug.do_prints then f x else ())
		    else (fn (f, x) => ())

  fun test_modulo modulo =
       let fun hash n = n mod modulo
           fun eq (a, b) = a = b
	   fun compare (result, expected) () = expected = result
           val s0 = Store.new (hash, eq)
           val r00 = Store.size s0
	   val s1 = Store.add (s0, 0w9, "hello")
	   val r10 = case Store.look (s1, 0w9) of
	                SOME (_, x) => x | NONE => "never"
	   val r11 = case Store.look (s1, 0w7) of
	                SOME (_, x) => x | NONE => "maybe"
	   val (_, r12) = Store.find (s1, 0w9)
	   val (_, r13) = (Store.find (s1, 0w5)
			   handle Store.Not_Present_In_Store => (s1, "bye"))
	   val (_, r14) = (Store.find (s1, 0w0)
			   handle Store.Not_Present_In_Store => (s1, "xxx"))
	   val s2 = Store.add (s1, 0w6, "world")
	   val (_, r20) = Store.find (s2, 0w9)
	   val (_, r21) = Store.find (s2, 0w6)
	   val (_, r22) = (Store.find (s2, 0w0)
			   handle Store.Not_Present_In_Store => (s2, "yes"))
	   val s3 = Store.add (s2, 0w8, "foo")
	   val s4 = Store.add (s3, 0w24, "bar")
	   val (_, r40) = Store.find (s4, 0w24)
	   val (_, r41) = Store.find (s4, 0w8)
	   val (_, r42) = Store.find (s4, 0w6)
	   val (_, r43) = Store.find (s4, 0w9)
	   val (_, r44) = (Store.find (s4, 0w0)
			   handle Store.Not_Present_In_Store => (s2, "no"))
	   val (_, r45) = (Store.find (s4, 0w5)
			   handle Store.Not_Present_In_Store => (s2, "green"))
	   val s5 = Store.remove (s4, 0w0)
	   val s6 = Store.remove (s5, 0w5)
	   val r60 = Store.size s6
	   val s7 = Store.remove (s6, 0w9)
	   val s8 = Store.remove (s7, 0w6)
	   val (_, r80) = Store.find (s8, 0w8)
	   val s9 = Store.remove (s8, 0w24)
	   val sA = Store.remove (s9, 0w8)
	   val rA0 = Store.size sA
	   fun remove_foobar (0w8, "foo") = true
	     | remove_foobar (0w24, "bar") = true
	     | remove_foobar _ = false
	   val sB = Store.remove_selected (s4, remove_foobar)
	   val rB0 = Store.size sB
       in Test.test ("empty size", (compare (r00, 0)));
	  Test.test ("look", (compare (r10, "hello")));
	  Test.test ("unsuccessful look", (compare (r11, "maybe")));
	  Test.test ("hello", (compare (r12, "hello")));
	  Test.test ("goodbye", (compare (r13, "bye")));
	  Test.test ("unsuccessful find", (compare (r14, "xxx")));
	  Test.test ("hello again", (compare (r20, "hello")));
	  Test.test ("world", (compare (r21, "world")));
	  Test.test ("second unsuccessful find", (compare (r22, "yes")));
	  Test.test ("bar", (compare (r40, "bar")));
	  Test.test ("foo", (compare (r41, "foo")));
	  Test.test ("world again", (compare (r42, "world")));
	  Test.test ("hello again", (compare (r43, "hello")));
	  Test.test ("third unsuccessful find", (compare (r44, "no")));
	  Test.test ("fourth unsuccessful find", (compare (r45, "green")));
	  Test.test ("full size", (compare (r60, 4)));
	  Test.test ("foo again", (compare (r80, "foo")));
	  Test.test ("emptied size", (compare (rA0, 0)));
	  Test.test ("removed foobar", (compare (rB0, 2)))
       end

  fun run_tests () =
       (test_modulo 0w64;
        test_modulo 0w8;
        test_modulo 0w3;
        test_modulo 0w1)

  fun run () = 
       if Debug.include_tests then
	Test.tests ("Store", 76, run_tests)
       else ()

  val _ = if ! Debug.do_tests then run () else ()

 end (* struct *)

(*
		2. structure Test_Store
*)

structure Test_Store = Test_Store (structure Store = Fox_Basis.Store
				   structure Debug = Fox_Basis.Debug
				   structure V = Fox_Basis.V
				   structure Test = Fox_Basis.Test
				   val do_prints = true)




