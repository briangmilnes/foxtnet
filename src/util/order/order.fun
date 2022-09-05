(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Efficient copy functions in ML.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Byte_Order
	2.	internal functions invert_b2, invert_b4
	3.	internal function id
	4.	function invert
	5.	functions to/from_big/little
	6.	structure B1
	7.	structure B2
	8.	structure B3

		iii.	RCS Log
	
$Log: order.fun,v $
Revision 1.9  1997/02/06  16:37:58  cline
added Word32fox structure for compatibility with CM

Revision 1.8  1996/03/12  22:28:03  esb
adapted to new FOXWORD.

Revision 1.7  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.6  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.5  1994/09/30  16:36:22  esb
changed bytes to foxwords.

Revision 1.4  1994/07/04  21:40:01  esb
made the machine byte-order determination local.

Revision 1.3  1994/07/01  02:40:37  danwang
Added machine_order_little parameter to functor.

Revision 1.2  1994/06/16  16:53:09  danwang
Updated for functorized Fox_Basis

Revision 1.1  1994/02/21  00:19:55  esb
Initial revision

	1.	functor Byte_Order
*)

functor Byte_Order (structure Byte: FOXWORD
		    val size: int
		    val machine_order_little: bool): BYTE_ORDER =
 struct

  type T = Byte.word

(*
	2.	internal functions invert_b2, invert_b4
*)

  fun invert_b2 n = Byte.orb (Byte.>> (n, 0w8), Byte.<< (n, 0w8))

  fun invert_b4 n =
       let val b0 = Byte.>> (Byte.<< (n, 0w24), 0w24)
	   val b1 = Byte.>> (Byte.<< (n, 0w16), 0w24)
	   val b2 = Byte.>> (Byte.<< (n, 0w8), 0w24)
	   val b3 = Byte.>> (n, 0w24)
       in Byte.orb (Byte.orb (Byte.<< (b0, 0w24), Byte.<< (b1, 0w16)),
		    Byte.orb (Byte.<< (b2, 0w8), b3))
       end

(*
	3.	internal function id
*)

  fun id x = x

(*
	4.	function invert
*)

  exception Byte_Order_Size_Not_Implemented

  val invert =
       case size of
	  1 => id
	| 2 => invert_b2
	| 4 => invert_b4
	| _ => raise Byte_Order_Size_Not_Implemented

(*
	5.	functions to/from_big/little
*)

  val (to_big, from_big) =
       if machine_order_little then (invert, invert) else (id, id)

  val (to_little, from_little) =
       if machine_order_little then (id, id) else (invert, invert)

 end (* struct *)


functor Byte_Orders (): BYTE_ORDERS =
 struct

  val little_endian = not Word32fox.bigEndian

(*
	6.	structure B1
*)

  structure B1 =
     Byte_Order (structure Byte = Word8
		 val size = 1
		 val machine_order_little = little_endian)

(*
	7.	structure B2
*)
	 
  structure B2 =
      Byte_Order (structure Byte = Word16
		  val size = 2
		  val machine_order_little = little_endian)
	 
(*
	8.	structure B3
*)

  structure B4 =
      Byte_Order (structure Byte = Word32fox
		  val size = 4
		  val machine_order_little = little_endian)
 end (* struct *)
