(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

Unaligned byteN arithmetic structures.

This code has not been optimized.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	structure unaligned
	2.	structure Byte2u
	3.	structure Byte4u

		iii.	RCS Log
	
$Log: unaligned.sml,v $
Revision 1.8  1997/02/06  16:37:58  cline
added Word32fox structure for compatibility with CM

Revision 1.7  1996/03/12  22:28:34  esb
adapted to new FOXWORD.

Revision 1.6  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.5  1995/04/05  15:11:24  esb
added sub_big and sub_little, update_big and update_little

Revision 1.4  1994/09/30  16:28:13  esb
changed to use FOXWORD instead of UBYTES.

Revision 1.3  1994/08/03  19:50:13  esb
got rid of '#' in RCS log

Revision 1.2  1993/11/08  18:36:50  cline
added structure Byte2u

Revision 1.1  1993/11/05  20:35:55  cline
Initial revision

		1.	structure unaligned
*)

structure Unaligned =
 struct

(*
		2.	structure Byte2u
 *)

  structure Byte2u: UNALIGNED_UBYTES = (* unaligned byte2 operations *)
   struct
    type word = {b0: Word8.word, b1: Word8.word}
    type aligned = Word16.word
    type wordarray = Word8Array.array

    val mask = 0xff
    val mask16 = Word16.fromInt mask

    fun unalign x =
         let val b0 = Word16.andb (x, mask16)
	     val b1 = Word16.andb (Word16.~>> (x, 0w8), mask16)
         in {b0 = Word8.fromInt (Word16.toInt b0),
	     b1 = Word8.fromInt (Word16.toInt b1)}
         end

    fun align {b0, b1} =
         let val c0 = Word16.fromInt (Word8.toInt b0)
             val c1 = Word16.fromInt (Word8.toInt b1)
         in Word16.+ (c0, Word16.<< (c1, 0w8))
         end

    fun op *   (x, y) = unalign (Word16.*   (align x, align y))
    fun op div (x, y) = unalign (Word16.div (align x, align y))
    fun op +   (x, y) = unalign (Word16.+   (align x, align y))
    fun op -   (x, y) = unalign (Word16.-   (align x, align y))
    fun op mod (x, y) = unalign (Word16.mod (align x, align y))
    fun op >   (x, y) =          Word16.>   (align x, align y)
    fun op >=  (x, y) =          Word16.>=  (align x, align y)
    fun op <   (x, y) =          Word16.<   (align x, align y)
    fun op <=  (x, y) =          Word16.<=  (align x, align y)
    fun op min (x, y) = unalign (Word16.min (align x, align y))
    fun op max (x, y) = unalign (Word16.max (align x, align y))

    fun op <<   (x, i) = unalign (Word16.<<  (align x, i))
    fun op ~>>  (x, i) = unalign (Word16.~>>  (align x, i))
    fun op >>   (x, i) = unalign (Word16.>> (align x, i))
    val notb           = unalign o Word16.notb o align
    fun xorb   (x, y) = unalign (Word16.xorb (align x, align y))
    fun orb    (x, y) = unalign (Word16.orb (align x, align y))
    fun andb   (x, y) = unalign (Word16.andb  (align x, align y))

    val toInt = Word16.toInt o align
    val toIntX = Word16.toIntX o align
    val fromInt = unalign o Word16.fromInt
    val toLargeWord = Word16.toLargeWord o align
    val toLargeWordX = Word16.toLargeWordX o align
    val fromLargeWord = unalign o Word16.fromLargeWord
    val toString = Word16.toString o align
    fun fromString s =
      case Word16.fromString s of
	NONE => NONE
      | SOME w => SOME (unalign w)

    fun update_little (a, k, {b0, b1} ) =
         (Word8Array.update (a, k, b0);
          Word8Array.update (a, Integer.+ (k, 1), b1))

    fun sub_little (a, k) =
         {b0 = Word8Array.sub (a, k),
	  b1 = Word8Array.sub (a, Integer.+ (k, 1))}

    fun update_big (a, k, {b0, b1} ) =
         (Word8Array.update (a, Integer.+ (k, 1), b0);
          Word8Array.update (a, k, b1))

    fun sub_big (a, k) =
         {b0 = Word8Array.sub (a, Integer.+ (k, 1)),
	  b1 = Word8Array.sub (a, k)}

   val bigEndian = Word32fox.bigEndian

   val (sub, update) =
        if bigEndian
	  then (sub_big, update_big)
	  else (sub_little, update_little)

   end (* structure Word16 *)

(*
		3.	structure Byte4u
 *)

  structure Byte4u: UNALIGNED_UBYTES = (* unaligned byte4 operations *)
   struct

    type word = {b0: Word8.word, b1: Word8.word,
		 b2: Word8.word, b3: Word8.word}
    type aligned = Word32.word
    type wordarray = Word8Array.array

    val mask = 0xff
    val mask32 = Word32.fromInt mask

    fun unalign x =
      let val b0 = Word32.andb (mask32, x)
          val b1 = Word32.andb (Word32.~>> (x, 0w8), mask32)
          val b2 = Word32.andb (Word32.~>> (x, 0w16), mask32)
          val b3 = Word32.andb (Word32.~>> (x, 0w24), mask32)
      in {b0 = Word8.fromInt (Word32.toInt b0),
	  b1 = Word8.fromInt (Word32.toInt b1),
	  b2 = Word8.fromInt (Word32.toInt b2),
	  b3 = Word8.fromInt (Word32.toInt b3)}
      end

    fun align {b0, b1, b2, b3} =
      let val c0 = Word32.fromInt (Word8.toInt b0)
          val c1 = Word32.fromInt (Word8.toInt b1)
          val c2 = Word32.fromInt (Word8.toInt b2)
          val c3 = Word32.fromInt (Word8.toInt b3)
      in Word32.+ (Word32.+ (c0, Word32.<< (c1, 0w8)),
		   Word32.+ (Word32.<< (c2, 0w16),
			     Word32.<< (c3, 0w24)))
      end

    fun op *   (x, y) = unalign (Word32.*   (align x, align y))
    fun op div (x, y) = unalign (Word32.div (align x, align y))
    fun op +   (x, y) = unalign (Word32.+   (align x, align y))
    fun op -   (x, y) = unalign (Word32.-   (align x, align y))
    fun op mod (x, y) = unalign (Word32.mod (align x, align y))
    fun op >   (x, y) =          Word32.>   (align x, align y)
    fun op >=  (x, y) =          Word32.>=  (align x, align y)
    fun op <   (x, y) =          Word32.<   (align x, align y)
    fun op <=  (x, y) =          Word32.<=  (align x, align y)
    fun op min (x, y) = unalign (Word32.min (align x, align y))
    fun op max (x, y) = unalign (Word32.max (align x, align y))

    fun op <<   (x, i) = unalign (Word32.<<  (align x, i))
    fun op ~>>  (x, i) = unalign (Word32.~>>  (align x, i))
    fun op >>   (x, i) = unalign (Word32.>> (align x, i))
    val notb           = unalign o Word32.notb o align
    fun xorb    (x, y) = unalign (Word32.xorb (align x, align y))
    fun orb     (x, y) = unalign (Word32.orb (align x, align y))
    fun andb    (x, y) = unalign (Word32.andb  (align x, align y))

    val toInt = Word32.toInt o align
    val toIntX = Word32.toIntX o align
    val fromInt = unalign o Word32.fromInt
    val toLargeWord = Word32.toLargeWord o align
    val toLargeWordX = Word32.toLargeWordX o align
    val fromLargeWord = unalign o Word32.fromLargeWord
    val toString = Word32.toString o align
    fun fromString s =
      case Word32.fromString s of
	NONE => NONE
      | SOME w => SOME (unalign w)

    fun update_little (a, k, {b0, b1, b2, b3} ) =
         (Word8Array.update (a, k, b0);
          Word8Array.update (a, Integer.+ (k, 1), b1);
          Word8Array.update (a, Integer.+ (k, 2), b2);
          Word8Array.update (a, Integer.+ (k, 3), b3))

    fun sub_little (a, k) =
         {b0 = Word8Array.sub (a, k),
	  b1 = Word8Array.sub (a, Integer.+ (k, 1)),
	  b2 = Word8Array.sub (a, Integer.+ (k, 2)),
	  b3 = Word8Array.sub (a, Integer.+ (k, 3))}

    fun update_big (a, k, {b0, b1, b2, b3} ) =
         (Word8Array.update (a, k, b3);
          Word8Array.update (a, Integer.+ (k, 1), b2);
          Word8Array.update (a, Integer.+ (k, 2), b1);
          Word8Array.update (a, Integer.+ (k, 3), b0))

    fun sub_big (a, k) =
         {b3 = Word8Array.sub (a, k),
	  b2 = Word8Array.sub (a, Integer.+ (k, 1)),
	  b1 = Word8Array.sub (a, Integer.+ (k, 2)),
	  b0 = Word8Array.sub (a, Integer.+ (k, 3))}

    val bigEndian = Word32fox.bigEndian

    val (sub, update) =
         if bigEndian
	   then (sub_big, update_big)
	   else (sub_little, update_little)

  end (* structure Byte4 *)

end (* structure unaligned *)
