(*

	FoxNet:  The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	ByteArray_Linearize linearizes the bytearray, but without
	any size information. When it unmarshals, it just uses the
	rest of the bytearray.

	ByteArray2_Linearize reads a two bytes unsigned integer and
	uses that as the size of the array.

	ByteArray3_Linearize reads two bytes and uses the first byte
	as the size of an array and then the second byte as the
	size of a second array.

	ByteArray4_Linearize reads one byte and uses that as the length
	of the bytearray.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor ByteArray_Linearize
	2.	functor ByteArray2_Linearize
	3.	functor ByteArray4_Linearize

		iii.	RCS Log
	
$Log: bytearray.fun,v $
Revision 1.13  1996/03/11  20:06:57  cline
updated for 109.6

Revision 1.12  1996/01/19  23:09:46  esb
adapted to the new wordarray signature.

Revision 1.11  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.10  1995/09/18  19:27:13  esb
eliminated use of structure Format.

Revision 1.9  1995/06/27  18:57:26  cline
adapted to new extern.sig

Revision 1.8  1995/06/20  17:46:38  esb
minor fix.

Revision 1.7  1995/03/12  17:59:54  esb
adapted to new trace.sig.

Revision 1.6  1995/03/10  03:53:53  esb
adapted to new vendor.sig.

Revision 1.5  1994/11/09  20:49:09  esb
adapted to new extern.sig.

Revision 1.4  1994/09/30  16:55:23  esb
replaced ubytes by fox words.

Revision 1.3  1994/08/25  23: 48: 18  robby
updated for new basis

Revision 1.2  1994/08/08  15: 12: 02  danwang
Made changes so authentication for zephyr works.

Revision 1.1  1994/07/14  20: 28: 26  robby
Initial revision

Revision 1.1  94/07/13  18: 43: 10  robby
Initial revision


	1.	functor ByteArray_Linearize
*)

functor ByteArray1_Linearize (structure Copy: COPY
			      structure Create: CREATE): BYTEARRAY_LINEARIZE = 
 struct

  type T = Word8Array.array
  type extern_in = Word8Array.array
  type extern_out = Word8Array.array
  type cursor = Word.word
  exception Extern

  val size = Word.fromInt o Word8Array.length

  fun marshal (array, value) pos = 
       let val length = size value
       in Copy.copy (value, 0, Word.toInt length, array, Word.toInt pos);
          pos + length
       end

  fun unmarshal (array, pos) = 
       let val length = size array - pos
       in (Create.copy_create (array, Word.toInt pos, Word.toInt length),
	   length + pos)
       end

 end

(*
	2.	functor ByteArray2_Linearize
*)

functor ByteArray2_Linearize (structure V: VENDOR
			      structure Copy: COPY
			      structure Create: CREATE
			      structure Ubyte2: UBYTE2_LINEARIZE
			      val debug_level: int ref option):
                        BYTEARRAY_LINEARIZE = 
 struct
  exception Extern
  fun makestring_exn Extern = SOME "extern/linearize/bytearray.fun: Extern"
    | makestring_exn _ = NONE

  structure Trace = Trace (structure V = V
			   val debug_level = debug_level
			   val module_name = "extern/linearize/bytearray.fun"
			   val makestring = makestring_exn)
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  type T = Word8Array.array
  type extern_in = Word8Array.array
  type extern_out = Word8Array.array
  type cursor = Word.word

  fun size b = Word.fromInt (Word8Array.length b) + 0w2

  fun marshal (array, value) pos = 
       let val length = Word8Array.length value
           val new = Ubyte2.marshal (array, Word16.fromInt length) pos
       in Copy.copy (value, 0, length, array, Word.toInt new);
          new + (Word.fromInt length)
       end

  fun unmarshal (array, start) = 
       let val (size, mid) = Ubyte2.unmarshal (array, start)
	   val size = Word16.toInt size
	   val _ = if Trace.debug_on () then
	            local_print ("Unmarshalling at " ^
				 Integer.toString (Word.toInt mid) ^ "...")
		   else ()
	   val result = Create.copy_create (array, Word.toInt mid, size)
       in (result, mid + (Word.fromInt size))
       end
 end


functor ByteArray3_Linearize (structure Ubyte1: UBYTE1_LINEARIZE
			      structure V: VENDOR
			      structure Copy: COPY
			      structure Create: CREATE
			      val debug_level: int ref option): 
                        BYTEARRAY3_LINEARIZE = 
 struct
  exception Extern
  fun makestring_exn Extern = SOME "extern/linearize/bytearray.fun: Extern"
    | makestring_exn _ = NONE

  structure Trace = Trace (structure V = V
			   val debug_level = debug_level
			   val module_name = "extern/linearize/bytearray.fun"
			   val makestring = makestring_exn)
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  datatype array_pair = Pair of (Word8Array.array * Word8Array.array)
  type T = array_pair
  type extern_in = Word8Array.array
  type extern_out = Word8Array.array
  type cursor = Word.word

  fun size (Pair (b, c)) =
       Word.fromInt (Word8Array.length b + Word8Array.length c) + 0w2

  fun marshal (array, Pair (b, c)) start = 
       let val length_b = Word8Array.length b
           val length_c = Word8Array.length c
	   val fst = Ubyte1.marshal (array, Word8.fromInt length_b) start
	   val snd = Ubyte1.marshal (array, Word8.fromInt length_c) fst
       in Copy.copy (b, 0, length_b, array, Word.toInt snd);
	  Copy.copy (c, 0, length_c, array, (Word.toInt snd) + length_b);
	  snd + Word.fromInt (length_b + length_c)
       end
        handle (Copy.Illegal_Copy _) => raise Extern

  fun unmarshal (array, start) = 
       let val (length_b, fst) = Ubyte1.unmarshal (array, start)
	   val (length_c, snd) = Ubyte1.unmarshal (array, fst)
	   val int_length_b = Word8.toInt length_b
	   val int_length_c = Word8.toInt length_c
	   val b = Create.copy_create (array, Word.toInt snd, int_length_b)
	   val c = Create.copy_create (array, (Word.toInt snd) + int_length_b,
				       int_length_c)
       in (Pair (b, c), snd + Word.fromInt (int_length_b + int_length_c))
       end
 end

(*
	3.	functor ByteArray4_Linearize
*)

functor ByteArray4_Linearize (structure Ubyte1: UBYTE1_LINEARIZE
			      structure V: VENDOR
			      structure Copy: COPY
			      structure Create: CREATE
			      val debug_level: int ref option): 
                        BYTEARRAY_LINEARIZE = 
 struct
  exception Extern
  fun makestring_exn Extern = SOME "extern/linearize/bytearray.fun: Extern"
    | makestring_exn _ = NONE

  structure Trace = Trace (structure V = V
			   val debug_level = debug_level
			   val module_name = "extern/linearize/bytearray.fun"
			   val makestring = makestring_exn)
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  type T = Word8Array.array
  type extern_in = Word8Array.array
  type extern_out = Word8Array.array
  type cursor = Word.word

  fun size b = Word.fromInt (Word8Array.length b) + 0w1

  fun marshal (array, value) start = 
       let val length = Word8Array.length value
	   val mid = Ubyte1.marshal (array, Word8.fromInt length) start
       in Copy.copy (value, 0, length, array, Word.toInt mid);
	  mid + (Word.fromInt length)
       end

  fun unmarshal (array, start) = 
       let val (size, mid) = Ubyte1.unmarshal (array, start)
           val int_size = Word8.toInt size
	   val value = Create.copy_create (array, Word.toInt mid, int_size)
       in (value, mid + (Word.fromInt int_size))
       end
 end
