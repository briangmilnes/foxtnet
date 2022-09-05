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

	Test code to test the Checksum utility.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Checksum
	2.      structure Test_Checksum

		iii.	RCS Log
	
$Log: checksum.tst,v $
Revision 1.22  1997/06/04  11:55:25  esb
changed to use "mod" instead of "quot".

Revision 1.21  1996/04/30  20:22:30  esb
changed to suit new interfaces.

Revision 1.20  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.19  1995/09/26  16:32:32  esb
added some tests that specifically verify wraparound errors.

Revision 1.18  1995/09/23  20:13:31  esb
added a test on data known to tickle a bug (since fixed)

Revision 1.17  1995/06/23  17:53:38  cline
updated to use Word_Array's

Revision 1.16  1995/06/20  17:36:55  esb
adapted to word-arrays.

Revision 1.15  1995/03/12  17:58:46  esb
adapted to new trace.sig.

Revision 1.14  1995/03/10  03:52:24  esb
adapted to new vendor.sig.

Revision 1.13  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.12  1994/10/06  19:49:52  cline
removed bogus "mod 256"

Revision 1.11  1994/09/30  16:27:06  esb
changed to work with FoxWord instead of ByteN

Revision 1.10  1994/08/02  20:10:39  esb
adapted to uncurried test.sig.

Revision 1.9  1994/06/16  16:51:18  danwang
Updated for functorized Fox_Basis

Revision 1.8  1994/02/08  14:10:50  esb
added additional checks.

Revision 1.7  1994/01/31  03:03:58  esb
added a large-array test.

Revision 1.6  1993/12/23  23:13:23  esb
changed check_sum to checksum and added checklist.

Revision 1.5  1993/10/25  19:37:24  cline
removed .U from Byte[421].U

Revision 1.4  1993/09/02  15:46:39  esb
minor changes.

Revision 1.3  93/07/23  17:37:43  esb
deleted the old comment.

Revision 1.2  1993/07/23  17:36:56  esb
wrote from scratch.

Revision 1.1  1993/06/10  22:41:20  milnes
Initial revision


		1.	functor Test_Checksum
*)

functor Test_Checksum (structure Checksum: CHECKSUM
		       structure Debug: DEBUG
		       structure V: VENDOR
		       structure Test: TEST
		       val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = V
			   val debug_level = debug_level
			   val module_name = "checksum.tst"
			   val makestring = fn _ => NONE)
  structure Array_Seq = Word_Array.W8.U_Big.F
  val local_print = Trace.local_print

  val known_test1 =
        [17, 17, 34, 34, 98, 89, 98, 224, 2, 246, 96, 224, 80, 
	 24, 0, 16, 0, 0, 0, 0, 96, 97, 98, 99, 100, 101, 102, 
	 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 
	 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 
	 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 
	 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 
	 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 
	 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 
	 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 
	 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 
	 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 
	 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 
	 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 
	 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 
	 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 
	 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 
	 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 
	 253, 254, 255, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
	 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 
	 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 
	 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 
	 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 
	 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 
	 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 
	 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 
	 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 
	 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 
	 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 
	 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 
	 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 
	 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 
	 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 
	 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 
	 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 
	 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 
	 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 
	 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 
	 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 
	 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 
	 248, 249, 250, 251, 252, 253, 254, 255, 0, 1, 2, 3, 
	 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
	 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 
	 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 
	 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 
	 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 
	 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 
	 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 
	 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 
	 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119]

  val known_test2 =
        [17, 17, 34, 34, 184, 210, 86, 242, 184, 234, 23, 162,
	 80, 24, 16, 0, 0, 0, 0, 0, 160, 161, 162, 163, 164,
	 165, 166, 167, 168, 169, 170, 171, 172, 173, 174,
	 175, 176, 177, 178, 179, 180, 181, 182, 183, 184,
	 185, 186, 187, 188, 189, 190, 191, 192, 193, 194,
	 195, 196, 197, 198, 199, 200, 201, 202, 203, 204,
	 205, 206, 207, 208, 209, 210, 211, 212, 213, 214,
	 215, 216, 217, 218, 219, 220, 221, 222, 223, 224,
	 225, 226, 227, 228, 229, 230, 231, 232, 233, 234,
	 235, 236, 237, 238, 239, 240, 241, 242, 243, 244,
	 245, 246, 247, 248, 249, 250, 251, 252, 253, 254,
	 255, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
	 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
	 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
	 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
	 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65,
	 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,
	 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
	 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103,
	 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
	 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
	 124, 125, 126, 127, 128, 129, 130, 131, 132, 133,
	 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
	 144, 145, 146, 147, 148, 149, 150, 151, 152, 153,
	 154, 155, 156, 157, 158, 159, 160, 161, 162, 163,
	 164, 165, 166, 167, 168, 169, 170, 171, 172, 173,
	 174, 175, 176, 177, 178, 179, 180, 181, 182, 183,
	 184, 185, 186, 187, 188, 189, 190, 191, 192, 193,
	 194, 195, 196, 197, 198, 199, 200, 201, 202, 203,
	 204, 205, 206, 207, 208, 209, 210, 211, 212, 213,
	 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
	 224, 225, 226, 227, 228, 229, 230, 231, 232, 233,
	 234, 235, 236, 237, 238, 239, 240, 241, 242, 243,
	 244, 245, 246, 247, 248, 249, 250, 251, 252, 253,
	 254, 255, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
	 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
	 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
	 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
	 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
	 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77,
	 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
	 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102,
	 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
	 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,
	 123, 124, 125, 126, 127, 128, 129, 130, 131, 132,
	 133, 134, 135, 136, 137, 138, 139, 140, 141, 142,
	 143, 144, 145, 146, 147, 148, 149, 150, 151, 152,
	 153, 154, 155, 156, 157, 158, 159, 160, 161, 162,
	 163, 164, 165, 166, 167, 168, 169, 170, 171, 172,
	 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183]

  fun test_bounds (low, high) () =
       let val insure = Word16.< (low, Word16.fromInt 16000) andalso
                        Word16.< (high, Word16.fromInt 16000) andalso
		        Word16.< (low, high)
           val ilow = Word16.toInt low
           val ihigh = Word16.toInt high

	   val data = Word_Array.from8
	               (Array_Seq.seek
			(Array_Seq.tabulate
			 (fn n => Word8.fromInt (Word.toInt n),
			  Word.fromInt ihigh), Word.fromInt ilow))
           val check = Checksum.checksum data
	               handle _ => (local_print ("exception in checksum (0x" ^
						 Word16.toString low ^
						 ", 0x" ^
						 Word16.toString high ^ ")");
				    Word16.fromInt 0)
           fun unwrapped_check (low, high) =
	        if low >= high then 0
	        else if (low + 1) = high then (low mod 256) * 256
	        else ((low mod 256) * 256 + (low + 1) mod 256 +
		      (unwrapped_check (low + 2, high)))
           fun wrap n = if n < 256 * 256 then Word16.fromInt n
		        else wrap ((n - (256 * 256)) + 1)
           val my_check = wrap (unwrapped_check (ilow, ihigh))
       in if insure andalso check = my_check then true
          else if insure then
           (local_print ("checksum value = 0x" ^ Word16.toString check ^
			 ", expected value = 0x" ^ Word16.toString my_check);
	    false)
          else
           (local_print "illegal bounds";
	    false)
       end

  fun test_given data () =
       let val check = Checksum.checksum data
	               handle _ => (local_print "exception in checksum";
				    Word16.fromInt 0)
	   fun odd n = Bits.andb (n, 1) = 1
	   fun norm n = if n <= 65535 then Word16.fromInt n
			else norm (n - 65535)
           fun unwrapped_check (_, NONE) = 0
	     | unwrapped_check (index, SOME (first, rest)) =
	        Word8.toInt first * (if odd index then 1 else 256) +
		unwrapped_check (index + 1, Array_Seq.next rest)
           val my_check = norm (unwrapped_check (0, Array_Seq.next
						      (Word_Array.to8 data)))
       in if check = my_check then true
          else
           (local_print ("checksum value = 0x" ^ Word16.toString check ^
			 ", expected value = 0x" ^ Word16.toString my_check);
	    false)
       end

  fun test_num (low, high) =
       Integer.toString low ^ "..." ^ Integer.toString high

  fun test_single (low, high) =
       let val string = test_num (low, high)
	   val result_fun = test_bounds (Word16.fromInt low,
			                 Word16.fromInt high)
       in Test.test (string, result_fun)
       end

  fun loop_inner (low, high) =
       if high <= low then ()
       else (test_single (low, high);
	     loop_inner (low, high - 3))

  fun test_op (operation, actual, expected) () =
       if actual <> expected then
        (local_print ("testing " ^ operation ^ ", result 0x" ^
		      Word16.toString actual ^ ", expecting 0x" ^
		      Word16.toString expected);
         false)
       else true

  fun test_known () =
       let fun gen [] = NONE
	     | gen (head :: rest) = SOME (Word8.fromInt head, rest)
	   val data1 = Array_Seq.new gen known_test1
	   val result_fun1 = test_given (Word_Array.from8 data1)
	   val data2 = Array_Seq.new gen known_test2
	   val result_fun2 = test_given (Word_Array.from8 data2)
       in Test.test ("known array 1", result_fun1);
          Test.test ("known array 2", result_fun2)
       end

  fun test_ops () =
       (Test.test ("+",
		   test_op ("one_s_add",
			    Checksum.one_s_add (Word16.fromInt 0xf02f,
						Word16.fromInt 0x2142),
			    Word16.fromInt 0x1172));
        Test.test ("complement",
		   (test_op ("one_s_complement",
			     Checksum.one_s_complement (Word16.fromInt
							0xf02f),
			     Word16.fromInt 0x0fd0)));
        let val l1 = Array_Seq.create (Word8.fromInt 99, 0w99)
            val l2 = Array_Seq.create (Word8.fromInt 91, 0w10)
            val l3 = Array_Seq.create (Word8.fromInt 7, 0w0)
            val l4 = Array_Seq.create (Word8.fromInt 33, 0w91)
            val l5 = Array_Seq.create (Word8.fromInt 13, 0w100)
	    val list = map Word_Array.from8 [l1, l2, l3, l4, l5]
	    val expected = Word_Array.from8
	                     (Array_Seq.append
			       (Array_Seq.append
				 (Array_Seq.append
				   (Array_Seq.append (l1,l2), l3), l4), l5))
	    fun checklist l =
		Checksum.complete_partial
		  (foldl Checksum.check_partial Checksum.initial_state l)
        in Test.test ("list", (test_op ("list",
					checklist list,
					Checksum.checksum expected)))
        end)

  fun loop_outer (x, y) () =
       if x >= y then
	(test_single (0, 1460);
	 test_single (0, 15056);
	 test_single (3, 99);
	 test_ops ();
	 test_known ())
       else (loop_inner (x, y);
	     if x mod 4 = 0 then	(* x is a multiple of four *)
	      loop_outer (x + 1, y) ()	(* make x odd *)
	     else if x mod 4 = 2 then	(* x is even *)
	      loop_outer (x * 4, y) ()	(* bake x into multiple of four *)
	     else			(* x is odd *)
	      loop_outer (x * 2, y) ())	(* make x even *)

  fun run () = 
       if Debug.include_tests then
	Test.tests ("Check", 34, (loop_outer (1, 22)))
       else ()

  val _ = if ! Debug.do_tests then run () else ()

 end (* struct *)

(*
		2. structure Test_Checksum
*)

structure Test_Checksum =
    Test_Checksum (structure Checksum = Fox_Basis.Checksum
		   structure Debug = Fox_Basis.Debug
		   structure V = Fox_Basis.V
		   structure Test = Fox_Basis.Test
		   val debug_level = NONE)




