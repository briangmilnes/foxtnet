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

	Test code to test the Copy utility.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Order
	2.	structure Byte_Order
	3.	structure Test_Order1
	4.	structure Test_Order2
	5.	structure Test_Order4

		iii.	RCS Log
	
$Log: order.tst,v $
Revision 1.13  1996/04/30  20:22:47  esb
changed to suit new interfaces.

Revision 1.12  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.11  1995/06/20  17:38:04  esb
minor fix.

Revision 1.10  1995/03/12  17:58:40  esb
adapted to new trace.sig.

Revision 1.9  1995/03/10  03:52:35  esb
adapted to new vendor.sig.

Revision 1.8  1994/12/05  22:09:17  esb
turned print statements into debugging statements.

Revision 1.7  1994/09/30  16:36:36  esb
changed bytes to foxwords.

Revision 1.6  1994/08/02  20:11:30  esb
adapted to uncurried test.sig.

Revision 1.5  1994/06/17  18:07:32  esb
re-adapted to use the functors.

Revision 1.4  1994/06/17  14:45:10  esb
minor changes.

Revision 1.3  1994/06/17  00:18:27  danwang
Updated to use functorized Fox_Basis

Revision 1.2  1994/06/13  23:42:11  esb
wrote from scratch (initial revision never worked).

Revision 1.1  1994/02/21  00:20:10  esb
Initial revision

		1.	functor Test_Order
*)

functor Test_Order (structure W: FOXWORD
		    structure Order: BYTE_ORDER
		     sharing type Order.T = W.word
		    structure Debug: DEBUG
		    structure V: VENDOR
		    structure Test: TEST
		    val name: string
		    val little: bool
		    val size: int	(* in bits *)
		    val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = V
			   val debug_level = debug_level
			   val module_name = "order.tst"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  fun makestring_array (array, index) =
       if Word8Array.length array > index then
	Word8.toString (Word8Array.sub (array, index)) ^ ", " ^
	makestring_array (array, index + 1)
       else ""

  fun test_invert v =
       let val bytes_per_T = size quot 8
	   val store = Word8Array.array (bytes_per_T * 2, 0w0)
	   val _ = W.update (store, 0, v)
	   val _ = W.update (store, bytes_per_T, Order.invert v)
	   val first = Word8Array.sub (store, 0)
	   val last = Word8Array.sub (store, bytes_per_T * 2 - 1)
	   val mid_low = Word8Array.sub (store, bytes_per_T - 1)
	   val mid_high = Word8Array.sub (store, bytes_per_T)
       in debug_print (fn _ => "first = " ^ Word8.toString first ^
		       ", last =  " ^ Word8.toString last ^
		       ", mid_low = " ^ Word8.toString mid_low ^
		       ", mid_high =  " ^ Word8.toString mid_high);
	  debug_print (fn _ => makestring_array (store, 0));
	  first = last andalso mid_low = mid_high
       end

  fun test_order v () =
       if little then
        Order.to_little v = v andalso
	Order.to_big v = Order.invert v andalso
	Order.from_big v = Order.invert v andalso
	Order.from_little v = v andalso
	test_invert v
       else
	Order.to_big v = v andalso
        Order.to_little v = Order.invert v andalso
	Order.from_big v = v andalso
	Order.from_little v = Order.invert v andalso
	test_invert v

  fun test_orders () =
       (Test.test ("1", test_order (W.fromInt 1));
        Test.test ("3", test_order (W.fromInt 3));
        Test.test ("66", test_order (W.fromInt 66));
        Test.test ("777", test_order (W.fromInt 777));
        Test.test ("8888", test_order (W.fromInt 8888));
        Test.test ("99999", test_order (W.fromInt 99999)))

  fun run () =
       if Debug.include_tests then Test.tests (name, 6, test_orders)
       else ()

  val _ = if ! Debug.do_tests then run () else ()

 end (* struct *)

(*
		2.	structure Byte_Order
*)

structure Byte_Order = Byte_Orders ()

(*
		3.	structure Test_Order1
*)

structure Test_Order1 = Test_Order (structure W = Word8
				    structure Order = Byte_Order.B1
				    structure Debug = Fox_Basis.Debug
				    structure V = Fox_Basis.V
				    structure Test = Fox_Basis.Test
				    val name = "Order1"
				    val little = not Word8.bigEndian
				    val size = 8
				    val debug_level = NONE)

(*
		4.	structure Test_Order2
*)

structure Test_Order2 = Test_Order (structure W = Word16
				    structure Order = Byte_Order.B2
				    structure Debug = Fox_Basis.Debug
				    structure V = Fox_Basis.V
				    structure Test = Fox_Basis.Test
				    val name = "Order2"
				    val little = not Word16.bigEndian
				    val size = 16
				    val debug_level = NONE)

(*
		5.	structure Test_Order4
*)

structure Test_Order4 = Test_Order (structure W = Word32
				    structure Order = Byte_Order.B4
				    structure Debug = Fox_Basis.Debug
				    structure V = Fox_Basis.V
				    structure Test = Fox_Basis.Test
				    val name = "Order4"
				    val little = not Word32.bigEndian
				    val size = 32
				    val debug_level = NONE)

