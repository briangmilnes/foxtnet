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

	Test code for word arrays.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Single_Word_Array
	2.	function run_test
	3.	function run
	4.	functor Test_Fwd_Rev
	5.	functor Test_Byte_Access
	6.	functor Test_Word_Array 
	7.	structure W1
	8.	structure W2
	9.	structure W4
	10.	structure W8
	11.	structure W16
	12.	structure W32N
	13.	structure W64
	14.	structure W128
	15.	structure W256
	16.	function run
	17.	structure Test_Word_Array
	18.	structure Test_Word_Big
	19.	structure Test_Word_Little

		iii.	RCS Log
	
$Log: wordarray.tst,v $
Revision 1.7  1996/04/30  20:23:32  esb
now checks all functors and many more functions within them.

Revision 1.6  1996/04/25  20:35:06  esb
major re-implementation, only partly debugged, checked in for sharing.

Revision 1.5  1996/02/06  23:43:10  esb
monor restructuring, made work with new wordarray.str.

Revision 1.4  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.3  1995/11/12  16:46:02  esb
adapted to new WORD_ARRAY signature.

Revision 1.2  1995/07/05  23:33:14  esb
adapted to new wordarray signature.

Revision 1.1  1995/06/20  17:33:50  esb
Initial revision


	1.	functor Test_Single_Word_Array

	Functions tested: new, next, seek, head, tail, map, app, fold,
	filter, create, tabulate, equal, less, length, append,
	reverse, create_uninitialized, write, update, read.
*)

functor Test_Single_Word_Array (structure A: ARRAY_SEQ
				structure W: FOXWORD
			          sharing type A.element = W.word
				structure V: VENDOR
				structure Test: TEST
				structure Trace: TRACE
				val name: string
				val debug_level: int ref option) =
 struct

(*
	2.	function run_test
*)

  local
   val zero = W.fromInt 0
   val one = W.fromInt 1
   val two = W.fromInt 2
   val three = W.fromInt 3
   val four = W.fromInt 4

   fun generate (_, 0) = NONE
     | generate (initial, count) =
        SOME (initial, (W.+ (initial, one), count - 1))

   fun tab_fun offset index = W.fromInt (offset + Word.toInt index)

   fun map_fun offset value = W.+ (W.fromInt offset, value)

   fun app_fun reference value = reference := W.xorb (value, ! reference)

   fun app_equiv (start, 0w0) = zero
     | app_equiv (start, count) =
        W.xorb (start, app_equiv (W.+ (start, one), count - 0w1))

   fun fold_fun (value, previous) = W.xorb (W.notb value, previous)

   fun even value = if W.andb (value, one) = zero then SOME value else NONE

  in
   fun run () =
        let fun makestring_array (a, 0) = ""
 	      | makestring_array (a, count) =
 	         case A.next a of
 		    NONE => ""
 		  | SOME (element, new_a) =>
 		     (W.toString element ^ ", " ^
 		      makestring_array (new_a, count - 1))
 	    fun is (n, word, a, size) =
 	         if word = W.fromInt n andalso
		  A.length a = Word.fromInt size then true
		 else
		  (Trace.local_print ("expecting " ^
				      W.toString (W.fromInt n) ^ "/" ^
				      Word.toString (Word.fromInt size) ^
				      ", got " ^ W.toString word ^ "/" ^
				      Word.toString (A.length a) ^
				      ", array (first 10) is " ^
				      makestring_array (a, 10));
		   false)
	    fun isnone NONE = true
	      | isnone _ = false

            val a1 = A.new generate (one, 10000)
	    val a2 = A.new generate (two, 0)
	    val a3 = A.new generate (three, 99)
	    val a4 = A.new generate (four, 1)
	    val _ = Test.test (name ^ "/a1", fn _ => A.length a1 = 0w10000)
	    val _ = Test.test (name ^ "/a2", fn _ => A.length a2 = 0w0)
	    val _ = Test.test (name ^ "/a3", fn _ => A.length a3 = 0w99)
	    val _ = Test.test (name ^ "/a4", fn _ => A.length a4 = 0w1)

	    val n1 = A.next a1
	    val n2 = A.next a2
	    val n3 = A.next a3
	    val n4 = A.next a4
	    val _ = Test.test (name ^ "/n1", fn _ =>
 		     case n1 of SOME (b, a) => is (1, b, a, 9999) | _ => false)
	    val _ = Test.test (name ^ "/n2", fn _ =>
			       case n2 of NONE => true | _ => false)
	    val _ = Test.test (name ^ "/n3", fn _ =>
 		     case n3 of SOME (b, a) => is (3, b, a, 98) | _ => false)
 	    val _ = Test.test (name ^ "/n4", fn _ =>
 		     case n4 of SOME (b, a) => is (4, b, a, 0) | _ => false)

 	    val u1 = case A.write a1 of
 	                NONE => NONE
 		      | SOME u =>
			 case A.update (u, W.fromInt 6) of
			    NONE => NONE
			  | SOME x => A.next (A.read x)
 	    val u2 = case A.write a2 of NONE => NONE | SOME u => SOME u
 	    val u3 = case A.write a3 of
 	                NONE => NONE
 		      | SOME u =>
			 case A.update (u, W.fromInt 8) of
			    NONE => NONE
			  | SOME x => A.next (A.read x)
 	    val u4 = case A.write a4 of
 	                NONE => NONE
 		      | SOME u =>
			 case A.update (u, W.fromInt 9) of
			    NONE => NONE
			  | SOME x => A.next (A.read x)
 	    val _ = Test.test (name ^ "/u1",
			       fn _ => case u1 of
			       SOME (b, a) => is (2, b, a, 9998) | _ => false)
 	    val _ = Test.test (name ^ "/u2", fn _ => isnone u2)
 	    val _ = Test.test (name ^ "/u3",
			       fn _ => case u3 of
			       SOME (b, a) => is (4, b, a, 97) | _ => false)
 	    val _ = Test.test (name ^ "/u4", fn _ => isnone u4)

 	    val n5 = A.next a1
 	    val n6 = A.next a2
 	    val n7 = A.next a3
 	    val n8 = A.next a4
 	    val _ = Test.test (name ^ "/n5", fn _ =>
 		     case n5 of SOME (b, a) => is (6, b, a, 9999) | _ => false)
 	    val _ = Test.test (name ^ "/n6", fn _ =>
 		     case n6 of NONE => true | _ => false)
 	    val _ = Test.test (name ^ "/n7", fn _ =>
 		     case n7 of SOME (b, a) => is (8, b, a, 98) | _ => false)
 	    val _ = Test.test (name ^ "/n8", fn _ =>
 		     case n8 of SOME (b, a) => is (9, b, a, 0) | _ => false)

 	    val s1 = A.seek (a1, 0w10000)
	    val s2 = A.seek (a2, 0w0)
	    val s3 = A.seek (a3, 0w33)
	    val s4 = A.seek (a4, 0w0)
	    val _ = Test.test (name ^ "/s1", fn _ => A.length s1 = 0w0)
	    val _ = Test.test (name ^ "/s2", fn _ => A.length s2 = 0w0)
	    val _ = Test.test (name ^ "/s3", fn _ => A.length s3 = 0w99 - 0w33)
	    val _ = Test.test (name ^ "/s4", fn _ => A.length s4 = 0w1)

	    val ns1 = A.next s1
	    val ns2 = A.next s2
	    val ns3 = A.next s3
	    val ns4 = A.next s4
	    val _ = Test.test (name ^ "/ns1", fn _ => isnone ns1)
	    val _ = Test.test (name ^ "/ns3", fn _ => isnone ns2)
	    val _ = Test.test (name ^ "/ns3", fn _ =>
			       case ns3 of
 			          SOME (b, a) => is (33 + 3, b, a, 99 - 34)
 			        | _ => false)
 	    val _ = Test.test (name ^ "/ns4", fn _ =>
 			       case ns4 of
 			          SOME (b, a) => is (9, b, a, 0)
 			        | _ => false)
	    val c1 = A.create (one, 0w10000)
	    val c2 = A.create (two, 0w0)
	    val c3 = A.create (three, 0w99)
	    val c4 = A.create (four, 0w1)
	    val (nc1a, nc1b) = (fn x => (A.head x, A.tail x)) (A.tail c1)
	    val nc2 = A.next c2
	    val nc3 = A.next (A.tail c3)
	    val nc4 = A.next c4
	    val _ = Test.test (name ^ "/c1", fn _ => is (1, nc1a, nc1b, 9998))
	    val _ = Test.test (name ^ "/c2", fn _ =>
			       case nc2 of NONE => true | _ => false)
	    val _ = Test.test (name ^ "/c3", fn _ =>
			       case nc3 of SOME (b, a) => is (3, b, a, 97)
			                 | _ => false)
 	    val _ = Test.test (name ^ "/c4", fn _ =>
			       case nc4 of SOME (b, a) => is (4, b, a, 0)
			                 | _ => false)
	    val t1 = A.tabulate (tab_fun 1, 0w10000)
	    val t2 = A.tabulate (tab_fun 2, 0w0)
	    val t3 = A.tabulate (tab_fun 3, 0w99)
	    val t4 = A.tabulate (tab_fun 4, 0w1)
	    val (nt1a, nt1b) = (fn x => (A.head x, A.tail x)) (A.tail t1)
	    val nt2 = A.next t2
	    val nt3 = A.next (A.tail t3)
	    val nt4 = A.next t4
	    val _ = Test.test (name ^ "/t1", fn _ => is (2, nt1a, nt1b, 9998))
	    val _ = Test.test (name ^ "/t2", fn _ =>
			       case nt2 of NONE => true | _ => false)
	    val _ = Test.test (name ^ "/t3", fn _ =>
			       case nt3 of SOME (b, a) => is (4, b, a, 97)
			                 | _ => false)
 	    val _ = Test.test (name ^ "/t4", fn _ =>
			       case nt4 of SOME (b, a) => is (4, b, a, 0)
			                 | _ => false)
	    val m1 = A.map (map_fun 3) t1
	    val m2 = A.map (map_fun 2) t2
	    val m3 = A.map (map_fun 1) t3
	    val m4 = A.map (map_fun 9) t4
	    val (nm1a, nm1b) = (fn x => (A.head x, A.tail x)) (A.tail m1)
	    val nm2 = A.next m2
	    val nm3 = A.next (A.tail m3)
	    val nm4 = A.next m4
	    val _ = Test.test (name ^ "/m1", fn _ => is (5, nm1a, nm1b, 9998))
	    val _ = Test.test (name ^ "/m2", fn _ =>
			       case nm2 of NONE => true | _ => false)
	    val _ = Test.test (name ^ "/m3", fn _ =>
			       case nm3 of SOME (b, a) => is (5, b, a, 97)
			                 | _ => false)
 	    val _ = Test.test (name ^ "/m4", fn _ =>
			       case nm4 of SOME (b, a) => is (13, b, a, 0)
			                 | _ => false)
	    val app_ref1 = ref zero
	    val app_ref2 = ref zero
	    val app_ref3 = ref zero
	    val app_ref4 = ref zero
	    val _ = A.app (app_fun app_ref1) t1
	    val _ = A.app (app_fun app_ref2) t2
	    val _ = A.app (app_fun app_ref3) t3
	    val _ = A.app (app_fun app_ref4) t4
(* the args to app_equiv should correspond to the args to A.tabulate, above *)
	    val expected1 = app_equiv (one, 0w10000)
	    val expected2 = app_equiv (two, 0w0)
	    val expected3 = app_equiv (three, 0w99)
	    val expected4 = app_equiv (four, 0w1)
	    val _ = Test.test (name ^ "/ap1", fn _ => ! app_ref1 = expected1)
	    val _ = Test.test (name ^ "/ap2", fn _ => ! app_ref2 = expected2)
	    val _ = Test.test (name ^ "/ap3", fn _ => ! app_ref3 = expected3)
	    val _ = Test.test (name ^ "/ap4", fn _ => ! app_ref4 = expected4)
(* note: since xor (not a, not b) = xor (a, b), the result is inverted
   exactly for those arrays (t3, t4) that have odd length. *)
	    val f1 = A.fold fold_fun zero t1
	    val f2 = A.fold fold_fun zero t2
	    val f3 = A.fold fold_fun zero t3
	    val f4 = A.fold fold_fun zero t4
	    val _ = Test.test (name ^ "/f1", fn _ => f1 = expected1)
	    val _ = Test.test (name ^ "/f2", fn _ => f2 = expected2)
	    val _ = Test.test (name ^ "/f3", fn _ => W.notb f3 = expected3)
	    val _ = Test.test (name ^ "/f4", fn _ => W.notb f4 = expected4)
	    val fl1 = A.next (A.filter even t1)
	    val fl2 = A.next (A.filter even t2)
	    val fl3 = A.next (A.filter even t3)
	    val fl4 = A.next (A.filter even t4)
	    val _ = Test.test (name ^ "/fl1", fn _ =>
			       case fl1 of SOME (b, a) => is (2, b, a, 4999)
			                 | _ => false)
	    val _ = Test.test (name ^ "/fl2", fn _ =>
			       case fl2 of NONE => true | _ => false)
	    val _ = Test.test (name ^ "/fl3", fn _ =>
			       case fl3 of SOME (b, a) => is (4, b, a, 48)
			                 | _ => false)
 	    val _ = Test.test (name ^ "/fl4", fn _ =>
			       case fl2 of NONE => true | _ => false)
	    val _ = Test.test (name ^ "/eq1",
			       fn _ => A.equal (A.append (t1, t2), t1))
	    val _ = Test.test (name ^ "/eq2",
			       fn _ => A.equal (t1, A.append (t2, t1)))
	    val _ = Test.test (name ^ "/eq3",
			       fn _ => A.equal (t2, A.append (t2, t2)))
	    val _ = Test.test (name ^ "/eq4", fn _ => not (A.equal (t3, c3)))
	    val _ = Test.test (name ^ "/eq5",
			       fn _ => A.equal (A.append (c3, c3),
						A.append (c3, c3)))
	    val _ = Test.test (name ^ "/eq6", fn _ => A.equal (t4, c4))
	    val _ = Test.test (name ^ "/le1", fn _ => A.less (t2, t1))
	    val _ = Test.test (name ^ "/le2", fn _ => not (A.less (t1, t1)))
	    val _ = Test.test (name ^ "/le3", fn _ => not (A.less (t3, t1)))
	    val _ = Test.test (name ^ "/le4", fn _ => A.less (t3, t4))
	    val _ = Test.test (name ^ "/le5", fn _ => A.less (c3, t3))
	    val _ = Test.test (name ^ "/le6", fn _ => not (A.less (t4, c4)))
	    fun check_length size =
	         A.length (A.create_uninitialized size) = size
	    val _ = Test.test (name ^ "/cu1", fn _ => check_length 0w0)
	    val _ = Test.test (name ^ "/cu2", fn _ => check_length 0w1)
	    val _ = Test.test (name ^ "/cu3", fn _ => check_length 0w3)
	    val _ = Test.test (name ^ "/cu4", fn _ => check_length 0w32768)
	    val _ = Test.test (name ^ "/rv1",
			       fn _ => A.equal (A.reverse (A.reverse t1), t1))
	    val _ = Test.test (name ^ "/rv2",
			       fn _ => A.equal (A.reverse c3, c3))
	    val _ = Test.test (name ^ "/rv3",
			       fn _ => A.equal (A.reverse t2, c2))
	    val _ = Test.test (name ^ "/rv4",
			       fn _ => not (A.equal (A.reverse t1, t1)))
        in ()
        end
  end

 end (* struct *)

(*
	4.	functor Test_Fwd_Rev
*)

functor Test_Fwd_Rev (structure A: REV_ARRAY_SEQ
		      structure W: FOXWORD
		        sharing type A.element = W.word
		      structure V: VENDOR
		      structure Test: TEST
		      structure Trace: TRACE
		      val name: string
		      val debug_level: int ref option) =
 struct

  structure Fwd = Test_Single_Word_Array (structure A = A.F
					  structure W = W
					  structure V = V
					  structure Test = Test
					  structure Trace = Trace
					  val name = name ^ "F"
					  val debug_level = debug_level)

  structure Rev = Test_Single_Word_Array (structure A = A.R
					  structure W = W
					  structure V = V
					  structure Test = Test
					  structure Trace = Trace
					  val name = name ^ "R"
					  val debug_level = debug_level)

  val run = Rev.run o Fwd.run
  
 end

(*
	5.	functor Test_Byte_Access
*)

functor Test_Byte_Access (structure A: BYTE_ACCESS_ARRAY
			  structure W: FOXWORD
		            sharing type A.element = W.word
			  structure V: VENDOR
			  structure Test: TEST
			  structure Trace: TRACE
			  val name: string
			  val debug_level: int ref option) =
 struct

  structure Native = Test_Fwd_Rev (structure A = A.Native
				   structure W = W
				   structure V = V
				   structure Test = Test
				   structure Trace = Trace
				   val name = name ^ ".N."
				   val debug_level = debug_level)

  structure Big = Test_Fwd_Rev (structure A = A.Big
				structure W = W
				structure V = V
				structure Test = Test
				structure Trace = Trace
				val name = name ^ ".B."
				val debug_level = debug_level)

  structure Little = Test_Fwd_Rev (structure A = A.Little
				   structure W = W
				   structure V = V
				   structure Test = Test
				   structure Trace = Trace
				   val name = name ^ ".L."
				   val debug_level = debug_level)

  structure Fwd_Big = Test_Single_Word_Array (structure A = A.F_Big
					      structure W = W
					      structure V = V
					      structure Test = Test
					      structure Trace = Trace
					      val name = name ^ ".FB."
					      val debug_level = debug_level)

  structure Rev_Big = Test_Single_Word_Array (structure A = A.R_Big
					      structure W = W
					      structure V = V
					      structure Test = Test
					      structure Trace = Trace
					      val name = name ^ ".RB."
					      val debug_level = debug_level)

  structure Fwd_Little = Test_Single_Word_Array (structure A = A.F_Little
						 structure W = W
						 structure V = V
						 structure Test = Test
						 structure Trace = Trace
						 val name = name ^ ".FL."
						 val debug_level = debug_level)

  structure Rev_Little = Test_Single_Word_Array (structure A = A.R_Little
						 structure W = W
						 structure V = V
						 structure Test = Test
						 structure Trace = Trace
						 val name = name ^ ".RL."
						 val debug_level = debug_level)

  structure U_Big = Test_Fwd_Rev (structure A = A.U_Big
				  structure W = W
				  structure V = V
				  structure Test = Test
				  structure Trace = Trace
				  val name = name ^ ".UB."
				  val debug_level = debug_level)

  structure U_Little = Test_Fwd_Rev (structure A = A.U_Little
				     structure W = W
				     structure V = V
				     structure Test = Test
				     structure Trace = Trace
				     val name = name ^ ".UL."
				     val debug_level = debug_level)


  val run = U_Little.run o U_Big.run o Rev_Little.run o Fwd_Little.run o
            Rev_Big.run o Fwd_Big.run o Little.run o Big.run o Native.run
  
 end

(*
	6.	functor Test_Word_Array 
*)

functor Test_Word_Array (structure A: WORD_ARRAY
			 structure Debug: DEBUG
			 structure V: VENDOR
			 structure Test: TEST
			 val debug_level: int ref option) =
 struct
  structure Trace = Trace (structure V = V
			   val debug_level = debug_level
			   val module_name = "wordarray.tst"
			   val makestring = fn _ => NONE)

(*
(*
	7.	structure W1
*)

  structure W1 = Test_Fwd_Rev (structure A = A.W1.U_Big
					 structure W = Word1
					 structure V = V
					 structure Test = Test
					 structure Trace = Trace
					 val name = "w1"
					 val debug_level= debug_level)

(*
	8.	structure W2
*)

  structure W2 = Test_Fwd_Rev (structure A = A.W2.U_Big
					 structure W = Word2
					 structure V = V
					 structure Test = Test
					 structure Trace = Trace
					 val name = "w2"
					 val debug_level= debug_level)

(*
	9.	structure W4
*)

  structure W4 = Test_Fwd_Rev (structure A = A.W4.U_Big
					 structure W = Word4
					 structure V = V
					 structure Test = Test
					 structure Trace = Trace
					 val name = "w4"
					 val debug_level= debug_level)
*)

(*
	10.	structure W8
*)

  structure W8 = Test_Byte_Access (structure A = A.W8
				   structure W = Word8
				   structure V = V
				   structure Test = Test
				   structure Trace = Trace
				   val name = "w8"
				   val debug_level= debug_level)

(*
	11.	structure W16
*)

  structure W16 = Test_Byte_Access (structure A = A.W16
				    structure W = Word16
				    structure V = V
				    structure Test = Test
				    structure Trace = Trace
				    val name = "w16"
				    val debug_level= debug_level)

(*
	12.	structure W32N
*)

  structure W32 = Test_Byte_Access (structure A = A.W32
				    structure W = Word32
				    structure V = V
				    structure Test = Test
				    structure Trace = Trace
				    val name = "W32"
				    val debug_level= debug_level)

(*
	13.	structure W64
*)

  structure W64 = Test_Byte_Access (structure A = A.W64
				    structure W = Word64
				    structure V = V
				    structure Test = Test
				    structure Trace = Trace
				    val name = "w64"
				    val debug_level= debug_level)

(*
	14.	structure W128
*)

  structure W128 = Test_Byte_Access (structure A = A.W128
				     structure W = Word128
				     structure V = V
				     structure Test = Test
				     structure Trace = Trace
				     val name = "w128"
				     val debug_level= debug_level)

(*
	15.	structure W256
*)

  structure W256 = Test_Byte_Access (structure A = A.W256
				     structure W = Word256
				     structure V = V
				     structure Test = Test
				     structure Trace = Trace
				     val name = "w256"
				     val debug_level= debug_level)

(*
	16.	function run
*)

  val individual_tests = 68
  val modules_tested = 14
  val num_tests = individual_tests * modules_tested

  fun run () = 
       if Debug.include_tests then
        (Test.tests ("word8-array"  , num_tests, W8.run);
	 Test.tests ("word16-array" , num_tests, W16.run);
         Test.tests ("word32-array" , num_tests, W32.run);
         Test.tests ("word64-array" , num_tests, W64.run);
         Test.tests ("word128-array", num_tests, W128.run);
         Test.tests ("word256-array", num_tests, W256.run))
       else ()

  val _ = if ! Debug.do_tests then run () else ()

 end (* struct *)

(*
	17.	structure Test_Word_Array
*)

structure Test_Word_Array = Test_Word_Array (structure A = Word_Array
					     structure Debug = Fox_Basis.Debug
					     structure V = Fox_Basis.V
					     structure Test = Fox_Basis.Test
					     val debug_level = SOME (ref 3))

(*
structure V = V ()
structure Test = Test (structure V = V)
structure Debug: DEBUG = Debug (val include_tests = true
				val do_tests = false
				val include_prints = true
				val do_prints = true
				val include_timings = true
				val do_timings = false)

structure Test_Word_Array = Test_Word_Array (structure A = Word_Array
					     structure Debug = Debug
					     structure V = V
					     structure Test = Test
					     val debug_level = SOME (ref 3))
*)


