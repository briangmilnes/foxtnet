(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

        WORD signature for Fox (probably temporary).


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	structure FoxWord32
	2.	structure FoxWord16
	3.	structure FoxWord8
	4.	structure FoxMakestring

		iii.	RCS Log
	
$Log: word.093.fun,v $
Revision 1.4  1995/02/20  21:54:03  esb
added foxword64.

Revision 1.3  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.2  1994/09/30  17:16:48  esb
added foxnet header.


		1.	structure FoxWord32
*)

structure FoxWord32 (* : FOXWORD *) =
 struct
  open Byte4
  val op * = Byte4.Unsafe_U.*

  type word = Byte4.ubytes
  type wordarray = ByteArray.bytearray
  val intToWord = Byte4.from_int
  val wordToInt = Byte4.to_int
  val lshift = Byte4.<<
  val rshift = Byte4.>>
  val rshiftl = Byte4.>>
  val shift = rshift
  val ashift = rshift
  val notb = Byte4.!!
  val orb = Byte4.||
  val andb = Byte4.&&
  val xorb = Byte4.xor
  val signExtend = wordToInt		(* incorrect implementation *)
  fun endian () = Byte4.endian

 end

(*
		2.	structure FoxWord16
*)

structure FoxWord16 (* : FOXWORD *) =
 struct
  open Byte2

  type word = Byte2.ubytes
  type wordarray = ByteArray.bytearray
  val intToWord = Byte2.from_int
  val wordToInt = Byte2.to_int
  val lshift = Byte2.<<
  val rshift = Byte2.>>
  val rshiftl = Byte2.>>
  val shift = rshift
  val ashift = rshift
  val notb = Byte2.!!
  val orb = Byte2.||
  val andb = Byte2.&&
  val xorb = Byte2.xor
  val signExtend = wordToInt		(* incorrect implementation *)
  fun endian () = Byte2.endian

 end (* struct *)

(*
		3.	structure FoxWord8
*)

structure FoxWord8 (* : FOXWORD *) =
 struct
  open Byte1

  type word = Byte1.ubytes
  type wordarray = ByteArray.bytearray
  val intToWord = Byte1.from_int
  val wordToInt = Byte1.to_int
  val lshift = Byte1.<<
  val rshift = Byte1.>>
  val rshiftl = Byte1.>>
  val shift = rshift
  val ashift = rshift
  val notb = Byte1.!!
  val orb = Byte1.||
  val andb = Byte1.&&
  val xorb = Byte1.xor
  val signExtend = wordToInt		(* incorrect implementation *)
  fun endian () = Byte1.endian

 end (* struct *)


structure FoxWord64 (* : FOXWORD *) =
 struct

(*
		4.	foxword64: types and exceptions
*)

  type b4 = FoxWord32.word
  type word = b4 * b4
  type wordarray = FoxWord32.wordarray
  exception Div
  exception Subscript

(*
		5.	foxword64: integer conversion
*)

  fun wordToInt (high, low) = FoxWord32.wordToInt low
  fun intToWord n = (FoxWord32.intToWord 0, FoxWord32.intToWord n)
  val signExtend = wordToInt		(* is this correct? *)

(*
		6.	foxword64: logical operations
*)

  fun notb (high, low) = (FoxWord32.notb high, FoxWord32.notb low)
  fun andb ((high1, low1), (high2, low2)) =
       (FoxWord32.andb (high1, high2), FoxWord32.andb (low1, low2))
  fun orb ((high1, low1), (high2, low2)) =
       (FoxWord32.orb (high1, high2), FoxWord32.orb (low1, low2))
  fun xorb ((high1, low1), (high2, low2)) =
       (FoxWord32.xorb (high1, high2), FoxWord32.xorb (low1, low2))

(*
		7.	foxword64: sub, update, and endian
*)

  datatype endian = Big | Little
  val my_endian = case FoxWord32.endian () of
                     FoxWord32.Little => Little
		   | FoxWord32.Big => Big
  fun endian () = my_endian

  local
   val size32 = 4

   fun sub_little (array, index) =
        ((FoxWord32.sub (array, index + size32), FoxWord32.sub (array, index))
	 handle _ => raise Subscript)
   fun sub_big (array, index) =
        ((FoxWord32.sub (array, index), FoxWord32.sub (array, index + size32))
	 handle _ => raise Subscript)

   fun update_little (array, index, (high, low)) =
        ((FoxWord32.update (array, index + size32, high);
	  FoxWord32.update (array, index, low))
	 handle _ => raise Subscript)
   fun update_big (array, index, (high, low)) =
        ((FoxWord32.update (array, index, high);
	  FoxWord32.update (array, index + size32, low))
	 handle _ => raise Subscript)

  in
   val sub = case my_endian of Little => sub_little | Big => sub_big

   val update = case my_endian of Little => update_little | Big => update_big
  end (* local *)

(*
		8.	foxword64: shifts

	lshift shifts left, rshift does an arithmetic right shift,
	rshiftl does a logical right shift.  ashift and lshift do
	arithmetic/logical shifts left for positive shifts,
	right for negative arguments.

*)

  fun lshift ((high, low), shift) =
       if shift >= 32 then lshift ((low, FoxWord32.intToWord 0), shift - 32)
       else if shift = 0 then (high, low)
       else
	let val lowlow = FoxWord32.lshift (low, shift)
	    val lowhigh = FoxWord32.rshiftl (low, 32 - shift)
	    val highhigh = FoxWord32.lshift (high, shift)
	in (FoxWord32.orb (highhigh, lowhigh), lowlow)
	end

  fun rshift ((high, low), shift) =
       if shift >= 32 then
	let val zero = FoxWord32.intToWord 0
	    val ones = FoxWord32.- (zero, FoxWord32.intToWord 1)
	    val high_bit = FoxWord32.xorb (ones, FoxWord32.rshiftl (ones, 1))
	    val new_high = if high_bit = zero then zero else ones
	in rshift ((new_high, high), shift - 32)
	end
       else if shift = 0 then (high, low)
       else
	let val highhigh = FoxWord32.rshift (high, shift)
	    val highlow = FoxWord32.lshift (high, 32 - shift)
	    val lowlow = FoxWord32.rshift (low, shift)
	in (highhigh, FoxWord32.orb (highlow, lowlow))
	end

  fun rshiftl ((high, low), shift) =
       if shift >= 32 then rshiftl ((FoxWord32.intToWord 0, high), shift - 32)
       else if shift = 0 then (high, low)
       else
	let val highhigh = FoxWord32.rshiftl (high, shift)
	    val highlow = FoxWord32.lshift (high, 32 - shift)
	    val lowlow = FoxWord32.rshiftl (low, shift)
	in (highhigh, FoxWord32.orb (highlow, lowlow))
	end

  fun ashift (value, sh) =
       if sh >= 0 then lshift (value, sh) else rshift (value, ~sh)

  fun shift (value, sh) =
       if sh >= 0 then lshift (value, sh) else rshiftl (value, ~sh)

(*
		9.	foxword64: plus
*)

  local
   val zero = FoxWord32.intToWord 0
   val one = FoxWord32.intToWord 1
  in
   fun (high1, low1) + (high2, low2) =
        let val low = FoxWord32.+ (low1, low2)
	    val over = if FoxWord32.< (low, low1) orelse
	                  FoxWord32.< (low, low2) then one
		       else zero
	    val high = FoxWord32.+ (FoxWord32.+ (high1, high2), over)
        in (high, low)
        end
  end

(*
		10.	foxword64: difference
*)

  local
   val one = intToWord 1
   val ones32 = FoxWord32.- (FoxWord32.intToWord 0, FoxWord32.intToWord 1)
   val ones64 = (ones32, ones32)
   fun negate n = one + (xorb (ones64, n))
  in
   fun n1 - n2 = n1 + (negate n2)
  end (* local *)

(*
		11.	foxword64: relations
*)

  fun (high1, low1) < (high2, low2) =
       FoxWord32.< (high1, high2) orelse
       (high1 = high2 andalso FoxWord32.< (low1, low2))

  fun (high1, low1) <= (high2, low2) =
       FoxWord32.< (high1, high2) orelse
       (high1 = high2 andalso FoxWord32.<= (low1, low2))

  fun (high1, low1) > (high2, low2) =
       FoxWord32.> (high1, high2) orelse
       (high1 = high2 andalso FoxWord32.> (low1, low2))

  fun (high1, low1) >= (high2, low2) =
       FoxWord32.> (high1, high2) orelse
       (high1 = high2 andalso FoxWord32.>= (low1, low2))

(*
		12.	foxword64: min and max
*)

  fun min (a, b) = if a < b then a else b

  fun max (a, b) = if a > b then a else b

(*
		13.	foxword64: times

	Algorithm used is shift and add.  Not terribly slow, but not
	terribly fast either, requiring 64 loops.  This is tail-recursive
	for zero bits, but who cares about stack depth anyway.
*)

  local
   val zero = intToWord 0
   val one32 = FoxWord32.intToWord 1
   fun low_bit (_, low) = FoxWord32.andb (low, one32) = one32
  in
   fun n1 * n2 =
        if n2 = zero then zero
	else if low_bit n2 then
	 (n1 + (lshift (n1, 1) * rshiftl (n2, 1)))
	else
	 (lshift (n1, 1) * rshiftl (n2, 1))
  end

(*
		14.	foxword64: div

	Algorithm used is shift, compare, and subtract.
*)

  local
   val zero = intToWord 0
   val one = intToWord 1
   val one32 = FoxWord32.intToWord 1
   fun high_bit (high, _) = FoxWord32.rshiftl (high, 31) = one32

   fun find_shift (n1, n2) =
        if n1 < n2 then ~1		(* went too far *)
	else if high_bit n2 then 0	(* found *)
	else Integer.+ (1, find_shift (n1, lshift (n2, 1)))

   (* div_loop invariant: rshiftl (n2, shifts) = divisor. *)
   fun div_loop (n1, n2, 0) = if n1 < n2 then zero else one
     | div_loop (n1, n2, shifts) =
        if n1 < n2 then
	 div_loop (n1, rshiftl (n2, 1), Integer.- (shifts, 1))
	else
	 lshift (one, shifts) +
	 div_loop (n1 - n2, rshiftl (n2, 1), Integer.- (shifts, 1))

  in
   fun n1 div n2 =
        if n2 = zero then raise Div
	else if n1 < n2 then zero
	else
	 let val shifts = find_shift (n1, n2)
	 in div_loop (n1, lshift (n2, shifts), shifts)
	 end
  end

(*
		15.	foxword64: mod
*)

  fun n1 mod n2 = n1 - ((n1 div n2) * n2)

 end (* struct *)

(*
		4.	structure FoxMakestring
*)

structure FoxMakestring =
 struct
  val word32 = Byte4.makestring
  val word16 = Byte2.makestring
  val word8 = Byte1.makestring
  val int = Integer.makestring
  val real = Real.makestring
  val bool = Bool.makestring

  local
   val ten = FoxWord64.intToWord 10
   val makedigit = word32 o FoxWord32.intToWord o FoxWord64.wordToInt
  in
   fun word64 value =
        if FoxWord64.< (value, ten) then
	 makedigit value
	else
	 word64 (FoxWord64.div (value, ten)) ^
	 makedigit (FoxWord64.mod (value, ten))
  end (* local *)
 end (* struct *)
