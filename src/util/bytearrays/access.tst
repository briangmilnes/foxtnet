(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Access
	2.	structureTest_Access

		iii.	RCS Log
	
$Log: access.tst,v $
Revision 1.8  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.7  1995/02/09  19:49:55  esb
corrected an anacronism.

Revision 1.6  1994/12/05  22:08:36  esb
Added a call to Test.tests.

Revision 1.5  1994/09/30  16:24:38  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.4  1994/08/02  19:31:05  esb
adapted to uncurried test.sig.

Revision 1.3  1994/06/29  16:49:29  robby
*** empty log message ***

Revision 1.2  94/06/29  16:48:05  robby
Checked itin with extra stuff at bottom, last time.

Revision 1.1  94/06/27  17:04:18  robby
Initial revision


	1.	functor Test_Access
*)

functor Test_Access (structure Test: TEST
                     structure Debug: DEBUG
		     structure V: VENDOR): TEST_STRUCTURE = 
 struct
  structure A = Access (structure V = V)
  val array = Word8Array.array (5, 0w0)
  val to_list_test = A.to_list array
  val zero = Word8.fromInt 0
  val one = Word8.fromInt 1
  val two = Word8.fromInt 2
  val three = Word8.fromInt 3
  val four = Word8.fromInt 4
  val to_list_expected =[zero, zero, zero, zero, zero]
  val all = (A.app_range (A.All, (fn _ => one), array);
	     A.to_list array)
  val all_expected =[one, one, one, one, one]
  val range =(A.app_range (A.Range {first = 0, last = 3}, fn _=> two, array);
	     A.to_list array)
  val range_expected =[two, two, two, two, one]
  val test =(A.app_range (A.Test {first = 2, test =(fn x =>x = one)},
			 fn _=> three, array);
	    A.to_list array)
  val test_expected =[two, two, three, three, one]
  val len =(A.app_range (A.Length {first = 1, length = 2}, fn _=> four, array);
	      A.to_list array)
  val len_expected =[two, four, four, three, one]

  val array = A.from_string "test--test"
  val range_to_string =
       A.range_to_string (array, A.Range {first = 0, last = 9})
  val range_to_string_expected ="test--test"

  fun run_tests ()=
       (Test.test ("to_list", (fn () => to_list_test = to_list_expected));
	Test.test ("all", (fn () => all = all_expected));
	Test.test ("range", (fn () => range = range_expected));
	Test.test ("test", (fn () => test = test_expected));
	Test.test ("length", (fn ()=> len = len_expected));
	Test.test ("range_to_string",
		   fn () => range_to_string = range_to_string_expected))

  fun run () = 
       if Debug.include_tests then
	Test.tests ("Access", 6, run_tests)
       else ()

  val _ = if ! Debug.do_tests then run () else ()


 end

(*
	2.	structureTest_Access
*)

structure Test_Access = Test_Access (structure Test = Fox_Basis.Test
				     structure Debug = Fox_Basis.Debug
				     structure V = Fox_Basis.V)
