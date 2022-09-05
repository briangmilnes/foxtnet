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

	Test code for the dynamic array utility.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Dyn_Array
	2.	function run_test
	3.	function run
	4.	structure Test_Dyn_Array

		iii.	RCS Log
	
$Log: dynarray.tst,v $
Revision 1.4  1995/03/10  03:51:29  esb
adapted to new vendor.sig.

Revision 1.3  1995/02/13  23:00:05  esb
redesigned map, added app and the string conversion functions.

Revision 1.2  1994/09/30  16:25:52  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.1  1994/08/02  21:16:42  esb
Initial revision


---------------------------------------------------------------------
	1.	functor Test_Dyn_Array
*)

functor Test_Dyn_Array (structure D: DYNAMIC_BYTE_ARRAY
			structure Debug: DEBUG
			structure V: VENDOR
			structure Test: TEST
			val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = V
			   val debug_level = debug_level
			   val module_name = "dynarray.tim")
  val local_print = Trace.local_print

(*
---------------------------------------------------------------------
	2.	function run_test
*)

  fun run_test () =
       let fun fill n = FoxWord8.intToWord (n mod 256)
	   fun to16 (n1, n2) =
	        let val a = ByteArray.array (2, 0)
		in FoxWord8.update (a, 0, n1);
		   FoxWord8.update (a, 1, n2);
		   FoxWord16.sub (a, 0)
		end
	   fun to32 (n1, n2, n3, n4) =
	        let val a = ByteArray.array (4, 0)
		in FoxWord8.update (a, 0, n1);
		   FoxWord8.update (a, 1, n2);
		   FoxWord8.update (a, 2, n3);
		   FoxWord8.update (a, 3, n4);
		   FoxWord32.sub (a, 0)
		end
           fun map1 (n, _) = fill n
           fun map2 (n, _) = to16 (fill n, fill (n + 1))
           fun map4 (n, _) =
	        to32 (fill n, fill (n + 1), fill (n + 2), fill (n + 3))
	   fun static_range (test, value, index, 0) = true
	     | static_range (test, value, index, bytes) =
	        if ByteArray.sub (test, index) = value then
		 static_range (test, value, index + 1, bytes - 1)
		else
		 (local_print ("static value check fails at index " ^
			       V.Integer.makestring index ^ ", value is " ^
			       V.Integer.makestring (ByteArray.sub (test,
								    index)) ^
			       ", expecting " ^ V.Integer.makestring value);
		  false)
	   fun fill_range (test, value, index, 0) = true
	     | fill_range (test, value, index, bytes) =
	        if ByteArray.sub (test, index) = value then
		 fill_range (test, (value + 1) mod 256, index + 1, bytes - 1)
		else
		 (local_print ("fill_range check fails at index " ^
			       V.Integer.makestring index ^ ", value is " ^
			       V.Integer.makestring (ByteArray.sub (test,
								    index)) ^
			       ", expecting " ^ V.Integer.makestring value);
		  false)
           val d0 = ByteArray.array (9999, 6)
	   val p0 = D.init d0
	   val _ = D.update (p0, 55, ByteArray.array (222, 42))
	   val h0 = ByteArray.array (55, 60)
	   val p1 = D.init1 (77, fill)
	   val d2 = ByteArray.array (1, 99)
	   val one = FoxWord8.intToWord 1
	   val two = FoxWord8.intToWord 2
	   val three = FoxWord8.intToWord 3
	   val p2 = D.init_list1 [one, two, three]
	   val p3 = D.init (D.read p0)
	   val p4 = D.append [p0, p1, p2]
	   val r0 = D.read p0
	   val r1 = D.read p1
	   val r2 = D.read p2
	   val r3 = D.read p3
	   val r4 = D.read p4
	   fun t0 () = static_range (r0, 6, 0, 55) andalso
		       static_range (r0, 42, 55, 222) andalso
		       static_range (r0, 6, 222 + 55, 9999 - (222 + 55))
	   fun t1 () = fill_range (r1, 0, 0, 77)
	   fun t2 () = fill_range (r2, 1, 0, 3)
	   fun t3 () = static_range (r3, 6, 0, 55) andalso
		       static_range (r3, 42, 55, 222) andalso
		       static_range (r3, 6, 222 + 55, 9999 - (222 + 55))
	   fun t4 () = static_range (r4, 6, 0, 55) andalso
		       static_range (r4, 42, 55, 222) andalso
		       static_range (r4, 6, 222 + 55, 9999 - (222 + 55))
		       andalso fill_range (r4, 0, 9999, 77) andalso
		       fill_range (r4, 1, 9999 + 77, 3)
	   val a15 = D.init1 (15, fill)
	   val b14 = D.tail (a15, 1)
	   val c13 = D.tail (a15, 2)
	   val d12 = D.tail (a15, 3)
	   val e11 = D.tail (d12, 1)
	   val m13 = D.map1 map1 c13
	   val m14 = D.map2 map2 b14
	   val m12 = D.map4 map4 d12
	   val f8 = D.tail (e11, 3)
	   val g6 = D.tail (a15, 9)
	   val h5 = D.tail (g6, 1)
	   val a8 = D.app4 (f8, map4)
	   val a6 = D.app2 (g6, map2)
	   val a5 = D.app1 (h5, map1)
	   val a15d = D.read a15
	   fun t5 () = fill_range (D.read m13, 0, 0, 13) andalso
	               fill_range (D.read m14, 0, 0, 14) andalso
	               fill_range (D.read m12, 0, 0, 12) andalso
	               fill_range (a15d, 0, 0, 7) andalso
	               fill_range (a15d, 0, 7, 2) andalso
	               fill_range (a15d, 0, 9, 1) andalso
	               fill_range (a15d, 0, 10, 5)
       in Test.test ("0", t0);
          Test.test ("1", t1);
          Test.test ("2", t2);
          Test.test ("3", t3);
          Test.test ("4", t4);
          Test.test ("map and app", t5)
       end

(*
---------------------------------------------------------------------
	3.	function run
*)

  fun run () = 
       if Debug.include_tests then
        Test.tests ("Dyn_Array", 6, run_test)
       else ()

  val _ = if ! Debug.do_tests then run () else ()

 end (* struct *)

(*
---------------------------------------------------------------------
	4.	structure Test_Dyn_Array
*)

structure Test_Dynamic_Array =
    Dynamic_Array (structure Copy = Fox_Basis.Copy
		   structure Create = Fox_Basis.Create
		   structure Checksum = Fox_Basis.Checksum
		   structure Access = Fox_Basis.Access
		   structure V = Fox_Basis.V)

structure Test_Dyn_Array = Test_Dyn_Array (structure D = Test_Dynamic_Array
					   structure Debug = Fox_Basis.Debug
					   structure V = TV
					   structure Test = Fox_Basis.Test
					   val debug_level = NONE)
