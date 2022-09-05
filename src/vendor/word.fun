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
	1.	structure Integer
	2.	functor Limit_Word
	3.	signature FOXWORD_DOUBLE
	4.	functor Word_Double
	5.	Word_Double: types
	6.	Word_Double: integer and word conversions
	7.	Word_Double: logical operations
	8.	Word_Double: shifts
	9.	Word_Double: plus
	10.	Word_Double: difference
	11.	Word_Double: relations
	12.	Word_Double: min, max
	13.	Word_Double: times
	14.	Word_Double: div
	15.	Word_Double: mod
	16.	Word_Double: string conversions
	17.	Word_Double: array and vector operations
	18.	structure Word32fox
	19.	structure Word16
	20.	structure Word16/alternate implementation
	21.	structure Word8
	22.	structure Word4
	23.	structure Word2
	24.	structure Word1
	25.	structure Word64
	26.	structure Word128
	27.	structure Word256
	28.	structure Word24
	29.	structure Word48

		iii.	RCS Log
	
$Log: word.fun,v $
Revision 1.25  1997/02/11  20:34:18  esb
removed compiler warning from 109.25

Revision 1.24  1997/02/06  16:37:58  cline
added Word32fox structure for compatibility with CM

Revision 1.23  1997/01/24  14:38:10  cline
added semicolons between modules to work around compiler bug

Revision 1.22  1996/12/18  22:19:00  esb
added fmt, scan, and wordSize, which are now standard.

Revision 1.21  1996/03/22  22:49:34  necula
Added vector access functions. Fixed a bug in sub_little.

Revision 1.20  1996/03/12  22:21:13  esb
removed signature FOXWORD_OPERATIONS, tried to optimizie Word48.sub/update.

Revision 1.19  1996/02/13  15:23:15  cline
added structure Integer = Int

Revision 1.18  1996/02/06  22:06:45  esb
added Word24 and Word24_Ops.

Revision 1.17  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.16  1995/11/29  21:55:18  cline
ported to SML/NJ 108.13

Revision 1.15  1995/11/10  23:29:10  esb
made more modular, possibly slower.

Revision 1.14  1995/10/24  21:55:50  cstone
oops...shouldn't refer to the basis here.

Revision 1.13  1995/10/24  21:15:35  cstone
Completed the hexword collection with hexword48

Revision 1.12  1995/10/24  21:06:58  cstone
Added hexword1, hexword8, hexword16, hexword32, hexword64

Revision 1.11  1995/10/19  17:02:45  cstone
Fixed FoxWord8.signExtend

Revision 1.10  1995/10/17  21:43:18  esb
added FoxWord1.

Revision 1.9  1995/09/23  19:45:30  esb
implementation with boxed word32s.

Revision 1.8  1995/09/14  15:38:18  esb
replaced word32s with pairs of integers.

Revision 1.7  1995/09/13  15:14:49  esb
added a datatype constructor in front of word64s.

Revision 1.6  1995/05/08  19:20:57  esb
fixed a bug in FoxWord32.sub_big

Revision 1.5  1995/04/13  18:28:00  esb
fixed a bug

Revision 1.4  1995/04/05  15:09:39  esb
added sub/update_big/little, FoxWord48, fixed a bug.

Revision 1.3  1995/03/24  15:59:13  esb
worked around a bug in Word32.intToWord, Word32.signExtend.

Revision 1.2  1995/03/07  20:23:56  esb
fixed many bugs uncovered by word.tst.

Revision 1.1  1995/03/06  19:45:40  esb
Initial revision


		1.	structure Integer
*)

structure Integer = Int;

(*
		2.	functor Limit_Word

	Invariant: any value of type W is normalized, that is,
	the topmost (base_bits - word_bits) bits are always clear
	when passed into or returned by a top-level function.

	The sub and update operations provided as functor parameters
	must access exactly word_bits, no more, and raise any
	unaligned exception only if the access is not on a word_bits
	boundary.  Note that we cannote use Base.sub/Base.update since
	the alignment requirements are most likely different for
	Base.word and Limit_Word.word.
*)

functor Limit_Word (structure Base: FOXWORD
		    val word_bits: Word.word
		    val sub_big: Word8Array.array * int -> Base.word
		    val sub_little: Word8Array.array * int -> Base.word
		    val subv_big: Word8Vector.vector * int -> Base.word
		    val subv_little: Word8Vector.vector * int -> Base.word
		    val update_big: Word8Array.array * int * Base.word -> unit
		    val update_little: Word8Array.array * int * Base.word
		                     -> unit): FOXWORD =
 struct

  datatype word = W of Base.word

  val wordSize = Word.toInt word_bits

  local
   val make = Base.fromInt

   val zero = make 0
   val one = make 1
   val ones = Base.- (zero, one)

   val limit = Base.<< (one, word_bits)
   val mask = Base.- (limit, one)
   val sign = Base.<< (one, word_bits - 0w1)
   val upper_mask = Base.<< (ones, word_bits)

   fun norm x = W (Base.andb (x, mask))	(* normalize: clear upper bits. *)

  in
   fun op* (W a, W b) = norm (Base.* (a, b))
   fun op+ (W a, W b) = norm (Base.+ (a, b))
   fun op- (W a, W b) = norm (Base.- (a, b))
   fun op div (W dividend, W divisor) = norm (Base.div (dividend, divisor))
   fun op mod (W a, W b) = norm (Base.mod (a, b))
   fun op< (W a, W b) = Base.< (a, b)
   fun op<= (W a, W b) = Base.<= (a, b)
   fun op> (W a, W b) = Base.> (a, b)
   fun op>= (W a, W b) = Base.>= (a, b)
   fun andb (W a, W b) = W (Base.andb (a, b))
   fun orb (W a, W b) = W (Base.orb (a, b))
   fun xorb (W a, W b) = W (Base.xorb (a, b))
   fun notb (W value) = W (Base.xorb (value, mask))
   fun min (W a, W b) = W (Base.min (a, b))
   fun max (W a, W b) = W (Base.max (a, b))
   fun >> (W a, shift) = W (Base.>> (a, shift))
   fun ~>> (W a, shift) = norm (Base.~>> (a, shift))
   fun << (W a, shift) = norm (Base.<< (a, shift))
   val fromInt = norm o Base.fromInt
   val fromLargeWord = norm o Base.fromLargeWord
   fun toInt (W value) = Base.toInt value
   fun toIntX (W value) =
        if Base.< (value, sign) then Base.toInt value
        else Base.toInt (Base.orb (value, upper_mask))
   fun toLargeWord (W value) = Base.toLargeWord value
   fun toLargeWordX (W value) =
        if Base.< (value, sign) then Base.toLargeWord value
        else Base.toLargeWord (Base.orb (value, upper_mask))
   fun fromString s =
     case Base.fromString s of
       NONE => NONE
     | SOME w => SOME (norm w)
   fun toString (W value) = Base.toString value
   fun fmt radix (W value) = Base.fmt radix value
   fun scan radix =
        let fun result reader chars =
	         case (Base.scan radix reader chars) of
		    NONE => NONE
		  | SOME (word, new_chars) => SOME (norm word, new_chars)
	in result
	end

   val bigEndian = Base.bigEndian

   val sub_big = W o sub_big
   val sub_little = W o sub_little

   val subv_big = W o subv_big
   val subv_little = W o subv_little
   (* we do not use "fun update_big ( ..." to avoid undesired recursion. *)
   val update_big = (fn (a, i, W value) => update_big (a, i, value))
   val update_little = (fn (a, i, W value) => update_little (a, i, value))

   val sub = if bigEndian then sub_big else sub_little
   val subv = if bigEndian then subv_big else subv_little
   val update = if bigEndian then update_big else update_little

  end (* local *)
 end; (* struct *)

(*
		3.	signature FOXWORD_DOUBLE

	Make the implementation externally visibile, to facilitate the
	implementation of e.g. Word48 sub and update.
*)

signature FOXWORD_DOUBLE =
 sig
  include FOXWORD

  type base_word
  datatype T = W of base_word * base_word
  sharing type word = T
 end;

(*
		4.	functor Word_Double
*)

functor Word_Double (structure Base: FOXWORD
		     val base_bits: Word.word
		     val to_word: Base.word * Base.word -> Word32.word
		     val from_word: Word32.word
		                  -> Base.word * Base.word): FOXWORD_DOUBLE =
 struct

(*
		5.	Word_Double: types
*)

  type base_word = Base.word
  datatype T = W of Base.word * Base.word
  type word = T
  val wordSize = 2 * Word.toInt base_bits

(*
		6.	Word_Double: integer and word conversions
*)

  fun fromLargeWord w = W (from_word w)
  val fromInt = fromLargeWord o Word32.fromInt

  fun toLargeWord (W value) = to_word value
  fun toLargeWordX (W value) = to_word value
  val toInt = Word32.toInt o toLargeWord
  val toIntX = Word32.toIntX o toLargeWordX

(*
		7.	Word_Double: logical operations
*)

  fun notb (W (high, low)) = W (Base.notb high, Base.notb low)
  fun andb (W (high1, low1), W (high2, low2)) =
       W (Base.andb (high1, high2), Base.andb (low1, low2))
  fun orb (W (high1, low1), W (high2, low2)) =
       W (Base.orb (high1, high2), Base.orb (low1, low2))
  fun xorb (W (high1, low1), W (high2, low2)) =
       W (Base.xorb (high1, high2), Base.xorb (low1, low2))

(*
		8.	Word_Double: shifts

	<< shifts left, ~>> does an arithmetic right shift,
	>> does a logical right shift.

*)

  val base_zero = Base.fromInt 0

  fun op << (W (high, low), shift) =
       if shift >= base_bits then << (W (low, base_zero), shift - base_bits)
       else
	 let val lowlow = Base.<< (low, shift)
	     val lowhigh = Base.>> (low, base_bits - shift)
	     val highhigh = Base.<< (high, shift)
	 in W (Base.orb (highhigh, lowhigh), lowlow)
	 end

  fun op ~>> (W (high, low), shift) =
       if shift >= base_bits then
	 ~>> (W (Base.~>> (high, base_bits), high), shift - base_bits)
       else
	let val highhigh = Base.~>> (high, shift)
	    val highlow = Base.<< (high, base_bits - shift)
	    val lowlow = Base.>> (low, shift)
	in W (highhigh, Base.orb (highlow, lowlow))
	end

  fun op >> (W (high, low), shift) =
       if shift >= base_bits then
	 >> (W (base_zero, high), shift - base_bits)
       else
	 let val highhigh = Base.>> (high, shift)
	     val highlow = Base.<< (high, base_bits - shift)
	     val lowlow = Base.>> (low, shift)
	 in W (highhigh, Base.orb (highlow, lowlow))
	 end

(*
		9.	Word_Double: plus
*)

  local
   val zero = Base.fromInt 0
   val one = Base.fromInt 1
  in
   fun (W (high1, low1)) + (W (high2, low2)) =
	let val low = Base.+ (low1, low2)
	    val over = if Base.< (low, low1) orelse Base.< (low, low2) then one
		       else zero
	    val high = Base.+ (Base.+ (high1, high2), over)
	in W (high, low)
	end
  end

(*
		10.	Word_Double: difference
*)

  local
   val one = fromInt 1
   val base_ones = Base.- (Base.fromInt 0, Base.fromInt 1)
   val word_ones = W (base_ones, base_ones)
   fun negate n = one + (xorb (word_ones, n))
  in
   fun n1 - n2 = n1 + (negate n2)
  end (* local *)

(*
		11.	Word_Double: relations
*)

  fun (W (high1, low1)) < (W (high2, low2)) =
       Base.< (high1, high2) orelse
       (high1 = high2 andalso Base.< (low1, low2))

  fun (W (high1, low1)) <= (W (high2, low2)) =
       Base.< (high1, high2) orelse
       (high1 = high2 andalso Base.<= (low1, low2))

  fun (W (high1, low1)) > (W (high2, low2)) =
       Base.> (high1, high2) orelse
       (high1 = high2 andalso Base.> (low1, low2))

  fun (W (high1, low1)) >= (W (high2, low2)) =
       Base.> (high1, high2) orelse
       (high1 = high2 andalso Base.>= (low1, low2))

(*
		12.	Word_Double: min, max
*)

  fun max (w0, w1) = if w0 >= w1 then w0 else w1

  fun min (w0, w1) = if w0 <= w1 then w0 else w1

(*
		13.	Word_Double: times

	Algorithm used is shift and add.  Not terribly slow, but not
	terribly fast either, requiring 64 loops for 64-bit words.
	This is tail-recursive for zero bits, but who cares about
	stack depth anyway.
*)

  local
   val zero = fromInt 0
   val base_one = Base.fromInt 1
   fun low_bit (W (_, low)) = Base.andb (low, base_one) = base_one
  in
   fun n1 * n2 =
	if n2 = zero then zero
	else if low_bit n2 then (n1 + (<< (n1, 0w1) * >> (n2, 0w1)))
	else (<< (n1, 0w1) * >> (n2, 0w1))
  end

(*
		14.	Word_Double: div

	Algorithm used is shift, compare, and subtract.
*)

  local
   val zero = fromInt 0
   val one = fromInt 1
   val base_one = Base.fromInt 1
   val base_shift = Word31.- (base_bits, 0w1)
   fun high_bit (W (high, _)) = Base.>> (high, base_shift) = base_one

   fun find_shift (n1, n2) =
	(if n1 < n2 then Word31.notb 0w0	(* went too far *)
	else if high_bit n2 then 0w0	(* found *)
	else Word31.+ (0w1, find_shift (n1, << (n2, 0w1))))

   (* div_loop invariant: rshiftl (n2, shifts) = divisor. *)
   fun div_loop (n1, n2, 0w0) = if n1 < n2 then zero else one
     | div_loop (n1, n2, shifts) =
	if n1 < n2 then div_loop (n1, >> (n2, 0w1), Word31.- (shifts, 0w1))
	else
	 << (one, shifts) +
	 div_loop (n1 - n2, >> (n2, 0w1), Word31.- (shifts, 0w1))

  in
   fun n1 div n2 =
	if n2 = zero then raise Div
	else if n1 < n2 then zero
	else
	 let val shifts = find_shift (n1, n2)
	 in div_loop (n1, << (n2, shifts), shifts)
	 end
  end

(*
		15.	Word_Double: mod
*)

  fun n1 mod n2 = n1 - ((n1 div n2) * n2)

(*
		16.	Word_Double: string conversions
*)

  local 
   val iord0 = Char.ord #"0"
   val iordA = Char.ord #"A"
   val ord0 = fromInt (Char.ord #"0")
   val ord9 = fromInt (Char.ord #"9")
   val ordA = fromInt (Char.ord #"A")
   val ordZ = fromInt (Char.ord #"Z")
   val orda = fromInt (Char.ord #"a")
   val ordz = fromInt (Char.ord #"z")
   val zero = fromInt 0
   val two = fromInt 2
   val eight = fromInt 8
   val ten = fromInt 10
   val sixteen = fromInt 16

   fun digitstring d =
	if Integer.< (d, 10) then String.str (Char.chr (Integer.+ (d, iord0)))
	else String.str (Char.chr (Integer.+ (Integer.- (d, 10), iordA)))

   fun to_base_string basis =
        let fun loop value =
	         if value < basis then digitstring (toInt value)
		 else
		  loop (value div basis) ^
		  digitstring (toInt (value mod basis))
	in loop
	end

   fun parse_loop (base, partial, c :: cs) = 
        let val ord_of = fromInt (ord c)
	    val num = if ord0 <= ord_of andalso ord_of <= ord9 then
	               ord_of - ord0
	              else if orda <= ord_of andalso ord_of <= ordz then
		       (ord_of - orda) + ten
		      else if ordA <= ord_of andalso ord_of <= ordZ then
		       (ord_of - ordA) + ten
		      else
		       partial
	in if base <= num then partial
	   else parse_loop (base, num + base * partial, cs)
	end
     | parse_loop (_, result, []) = result

  in
   val toString = to_base_string ten

   fun fmt StringCvt.DEC = to_base_string ten
     | fmt StringCvt.BIN = to_base_string two
     | fmt StringCvt.HEX = to_base_string sixteen
     | fmt StringCvt.OCT = to_base_string eight

   fun fromString string =
        case explode string of
	   (#"0" :: #"x" :: rest) => SOME (parse_loop (sixteen, zero, rest))
	 | number => SOME (parse_loop (ten, zero, number))

   exception Word_Scan_Not_Implemented
   fun scan radix = raise Word_Scan_Not_Implemented

  end

(*
		17.	Word_Double: array and vector operations
*)

  val bigEndian = Base.bigEndian


  local
   val base_bytes = Int.+ (Int.div (Int.- (Word.toInt base_bits, 1), 8), 1)
  in

   fun sub_big (array, index) =
	W (Base.sub_big (array, index),
	   Base.sub_big (array, Int.+ (index, base_bytes)))

   fun subv_big (vector, index) =
	W (Base.subv_big (vector, index),
	   Base.subv_big (vector, Int.+ (index, base_bytes)))

   fun sub_little (array, index) =
	W (Base.sub_little (array, Int.+ (index, base_bytes)),
	   Base.sub_little (array, index))

   fun subv_little (vector, index) =
	W (Base.subv_little (vector, Int.+ (index, base_bytes)),
	   Base.subv_little (vector, index))

   fun update_big (array, index, W (high, low)) =
	(Base.update_big (array, index, high);
	 Base.update_big (array, Int.+ (index, base_bytes), low))

   fun update_little (array, index, W (high, low)) =
	(Base.update_little (array, Int.+ (index, base_bytes), high);
	 Base.update_little (array, index, low))
  end

  val sub = if bigEndian then sub_big else sub_little
  val subv = if bigEndian then subv_big else subv_little
  val update = if bigEndian then update_big else update_little

 end; (* struct *)

(*
		18.	structure Word32fox
*)

structure Word32fox (* : FOXWORD -- export fmt *) =
 struct
  open Word32				(* inherit the built-in operations *)

  fun min (a, b) = if a >= b then b else a

  fun max (a, b) = if a >= b then a else b

  val bigEndian = false

  fun sub_big (array, index) =
       if Bits.andb (index, 3) = 0 then
	Pack32Big.subArr (array, Bits.>> (index, 0w2))
       else raise Subscript

  fun subv_big (vector, index) =
       if Bits.andb (index, 3) = 0 then
	Pack32Big.subVec (vector, Bits.>> (index, 0w2))
       else raise Subscript


  fun sub_little (array, index) =
       if Bits.andb (index, 3) = 0 then
        Pack32Little.subArr (array, Bits.>> (index, 0w2))
       else raise Subscript

  fun subv_little (vector, index) =
       if Bits.andb (index, 3) = 0 then
        Pack32Little.subVec (vector, Bits.>> (index, 0w2))
       else raise Subscript

  val sub = if bigEndian then sub_big else sub_little

  val subv = if bigEndian then subv_big else subv_little

  fun update_big (array, index, value) =
       if Bits.andb (index, 3) = 0 then
	Pack32Big.update (array, Bits.>> (index, 0w2), value)
       else raise Subscript

  fun update_little (array, index, value) =
       if Bits.andb (index, 3) = 0 then
	Pack32Little.update (array, Bits.>> (index, 0w2), value)
       else raise Subscript

  val update = if bigEndian then update_big else update_little

(* override fromString to use conventional parsing, e.g. use 0x for hex *)
  local
   val ord0 = fromInt (Char.ord #"0")
   val ord9 = fromInt (Char.ord #"9")
   val ordA = fromInt (Char.ord #"A")
   val ordZ = fromInt (Char.ord #"Z")
   val orda = fromInt (Char.ord #"a")
   val ordz = fromInt (Char.ord #"z")
   val zero = fromInt 0
   val ten = fromInt 10
   val sixteen = fromInt 16

  in
   fun fromString string =
        let exception String
	    fun helper (base, c :: cs) = 
	         let val ord_of = fromInt (ord c)
		     val num = if ord0 <= ord_of andalso ord_of <= ord9 then
			        ord_of - ord0
		               else if orda <= ord_of andalso 
		                       ord_of <= ordz then
			        (ord_of - orda) + ten
			       else if ordA <= ord_of andalso
		                       ord_of <= ordZ then
			        (ord_of - ordA) + ten
			       else
				(print "unknown character in string\n";
				 raise String)
		 in if base <= num then
	             (print "some character bigger than the base\n";
		      raise String)

		    else num + base * (helper (base, cs))
		 end
	      | helper (_, []) = zero
        in (case explode string of
	       (#"0" :: #"x" :: rest) => SOME (helper (sixteen, rev rest))
	     | number => SOME (helper (ten, rev number)))
	    handle _ => NONE
        end
  end

 end;

(*
		19.	structure Word16
*)

structure Word16: FOXWORD =
 struct
  local
   fun sub_big (array, index) =
        let val low = Word8.toLargeWord (Word8Array.sub (array, index + 1))
	    val high = Word8.toLargeWord (Word8Array.sub (array, index))
	in Word32.orb (low, Word32.<< (high, 0w8))
	end

   fun subv_big (vector, index) =
        let val low = Word8.toLargeWord (Word8Vector.sub (vector, index + 1))
	    val high = Word8.toLargeWord (Word8Vector.sub (vector, index))
	in Word32.orb (low, Word32.<< (high, 0w8))
	end

   fun sub_little (array, index) =
        let val low = Word8.toLargeWord (Word8Array.sub (array, index))
	    val high = Word8.toLargeWord (Word8Array.sub (array, index + 1))
	in Word32.orb (low, Word32.<< (high, 0w8))
	end

   fun subv_little (vector, index) =
        let val low = Word8.toLargeWord (Word8Vector.sub (vector, index))
	    val high = Word8.toLargeWord (Word8Vector.sub (vector, index + 1))
	in Word32.orb (low, Word32.<< (high, 0w8))
	end

   val mask = Word32.fromInt 0xFF

   fun update_big (array, index, value) =
        let val low = Word32.andb (value, mask)
	    val high = Word32.>> (value, 0w8)
	in Word8Array.update (array, index + 1, Word8.fromLargeWord low);
	   Word8Array.update (array, index, Word8.fromLargeWord high)
	end

   fun update_little (array, index, value) =
        let val low = Word32.andb (value, mask)
	    val high = Word32.>> (value, 0w8)
	in Word8Array.update (array, index, Word8.fromLargeWord low);
	   Word8Array.update (array, index + 1, Word8.fromLargeWord high)
	end

   structure W = Limit_Word (structure Base = Word32fox
			     val word_bits = 0w16
			     val sub_big = sub_big
			     val sub_little = sub_little
			     val subv_big = subv_big
			     val subv_little = subv_little
			     val update_big = update_big
			     val update_little = update_little)
  in
   open W
  end
 end;

(*
(*
		20.	structure Word16/alternate implementation

	Alternative implementation as a pair of Word8s.
*)

structure Word16: FOXWORD =
 struct
  local
   val zero = Word8.fromInt 0
   fun to_word (high, low) = low
   fun from_word low = (zero, low)
   structure W = Word_Double (structure Base = Word8
			      val base_bits = 0w8
			      val to_word = to_word
			      val from_word = from_word)
  in
   open W
  end
 end
*)

(*
		21.	structure Word8
*)

structure Word8 (* : FOXWORD -- export fmt *) =
 struct
  open Word8				(* inherit the built-in operations *)

  local
   val high_bit_mask = fromInt 0x80

  in
   fun op~>> (a, shift) =		(* bug in SML/NJ v. 109 *)
        if shift = 0w0 then a
	else if andb (a, high_bit_mask) = high_bit_mask then
	 ~>> (orb (Word8.>> (a, 0w1), high_bit_mask), Word.- (shift, 0w1))
	else
	 Word8.>> (a, shift)
  end

  fun min (a, b) = if a >= b then b else a

  fun max (a, b) = if a >= b then a else b

  val bigEndian = false

  val sub = Word8Array.sub
  val sub_big = Word8Array.sub
  val sub_little = Word8Array.sub

  val subv = Word8Vector.sub
  val subv_big = Word8Vector.sub
  val subv_little = Word8Vector.sub

  val update = Word8Array.update
  val update_big = Word8Array.update
  val update_little = Word8Array.update

(* override fromString to use conventional parsing, e.g. use 0x for hex *)
  local
   val ord0 = fromInt (Char.ord #"0")
   val ord9 = fromInt (Char.ord #"9")
   val ordA = fromInt (Char.ord #"A")
   val ordZ = fromInt (Char.ord #"Z")
   val orda = fromInt (Char.ord #"a")
   val ordz = fromInt (Char.ord #"z")
   val zero = fromInt 0
   val ten = fromInt 10
   val sixteen = fromInt 16

  in
   fun fromString string =
        let exception String
	    fun helper (base, c :: cs) = 
	         let val ord_of = fromInt (ord c)
		     val num = if ord0 <= ord_of andalso ord_of <= ord9 then
			        ord_of - ord0
		               else if orda <= ord_of andalso 
		                       ord_of <= ordz then
			        (ord_of - orda) + ten
			       else if ordA <= ord_of andalso
		                       ord_of <= ordZ then
			        (ord_of - ordA) + ten
			       else
				(print "unknown character in string\n";
				 raise String)
		 in if base <= num then
	             (print "some character bigger than the base\n";
		      raise String)

		    else num + base * (helper (base, cs))
		 end
	      | helper (_, []) = zero
        in (case explode string of
	       (#"0" :: #"x" :: rest) => SOME (helper (sixteen, rev rest))
	     | number => SOME (helper (ten, rev number)))
	    handle _ => NONE
        end
  end
 end;

(*
		22.	structure Word4
*)

structure Word4: FOXWORD =
 struct
  local
   val low_mask = Word8.fromInt 0x0f
   val high_mask = Word8.fromInt 0xf0

   fun sub_big (array, index) =
        let val byte_index = Bits.>> (index, 0w1)
	    val byte = Word8Array.sub (array, byte_index)
	in if Bits.<< (byte_index, 0w1) = index then Word8.>> (byte, 0w4)
	   else Word8.andb (byte, low_mask)
	end

   fun sub_little (array, index) =
        let val byte_index = Bits.>> (index, 0w1)
	    val byte = Word8Array.sub (array, byte_index)
	in if Bits.<< (byte_index, 0w1) <> index then Word8.>> (byte, 0w4)
	   else Word8.andb (byte, low_mask)
	end

   fun subv_big (vector, index) =
        let val byte_index = Bits.>> (index, 0w1)
	    val byte = Word8Vector.sub (vector, byte_index)
	in if Bits.<< (byte_index, 0w1) = index then Word8.>> (byte, 0w4)
	   else Word8.andb (byte, low_mask)
	end

   fun subv_little (vector, index) =
        let val byte_index = Bits.>> (index, 0w1)
	    val byte = Word8Vector.sub (vector, byte_index)
	in if Bits.<< (byte_index, 0w1) <> index then Word8.>> (byte, 0w4)
	   else Word8.andb (byte, low_mask)
	end

   fun update_big (array, index, value) =
        let val byte_index = Bits.>> (index, 0w1)
	    val byte = Word8Array.sub (array, byte_index)
	in Word8Array.update (array, byte_index,
			      if Bits.<< (byte_index, 0w1) = index then
			       Word8.orb (Word8.<< (value, 0w4),
					  Word8.andb (byte, low_mask))
			      else
			       Word8.orb (value, Word8.andb (byte, high_mask)))
	end

   fun update_little (array, index, value) =
        let val byte_index = Bits.>> (index, 0w1)
	    val byte = Word8Array.sub (array, byte_index)
	in Word8Array.update (array, byte_index,
			      if Bits.<< (byte_index, 0w1) <> index then
			       Word8.orb (Word8.<< (value, 0w4),
					  Word8.andb (byte, low_mask))
			      else
			       Word8.orb (value, Word8.andb (byte, high_mask)))
	end

   structure W = Limit_Word (structure Base = Word8
			     val word_bits = 0w4
			     val sub_big = sub_big
			     val sub_little = sub_little
			     val subv_big = subv_big
			     val subv_little = subv_little
			     val update_big = update_big
			     val update_little = update_little)
  in
   open W
  end
 end;

(*
		23.	structure Word2
*)

structure Word2: FOXWORD =
 struct
  local
   val low_mask = Word8.fromInt 0x03
   val index_mask = 0x03

   fun sub_big (array, index) =
        let val byte_index = Bits.>> (index, 0w2)
	    val shift = Bits.<< (3 - (Bits.andb (index, index_mask)), 0w1)
	    val byte = Word8Array.sub (array, byte_index)
	in Word8.andb (Word8.>> (byte, Word.fromInt shift), low_mask)
	end

   fun sub_little (array, index) =
        let val byte_index = Bits.>> (index, 0w2)
	    val shift = Bits.<< (Bits.andb (index, index_mask), 0w1)
	    val byte = Word8Array.sub (array, byte_index)
	in Word8.andb (Word8.>> (byte, Word.fromInt shift), low_mask)
	end

   fun subv_big (vector, index) =
        let val byte_index = Bits.>> (index, 0w2)
	    val shift = Bits.<< (3 - (Bits.andb (index, index_mask)), 0w1)
	    val byte = Word8Vector.sub (vector, byte_index)
	in Word8.andb (Word8.>> (byte, Word.fromInt shift), low_mask)
	end

   fun subv_little (vector, index) =
        let val byte_index = Bits.>> (index, 0w2)
	    val shift = Bits.<< (Bits.andb (index, index_mask), 0w1)
	    val byte = Word8Vector.sub (vector, byte_index)
	in Word8.andb (Word8.>> (byte, Word.fromInt shift), low_mask)
	end

   fun update_big (array, index, value) =
        let val byte_index = Bits.>> (index, 0w2)
	    val shift = Word.fromInt (Bits.<< (3 - (Bits.andb
						    (index, index_mask)), 0w1))
	    val byte = Word8Array.sub (array, byte_index)
	    val mask = Word8.notb (Word8.<< (low_mask, shift))
	    val res = Word8.orb (Word8.andb (byte, mask),
				 Word8.<< (value, shift))
	in Word8Array.update (array, byte_index, res)
	end

   fun update_little (array, index, value) =
        let val byte_index = Bits.>> (index, 0w2)
	    val shift = Word.fromInt (Bits.<<
				      (Bits.andb (index, index_mask), 0w1))
	    val byte = Word8Array.sub (array, byte_index)
	    val mask = Word8.notb (Word8.<< (low_mask, shift))
	    val res = Word8.orb (Word8.andb (byte, mask),
				 Word8.<< (value, shift))
	in Word8Array.update (array, byte_index, res)
	end

   structure W = Limit_Word (structure Base = Word8
			     val word_bits = 0w2
			     val sub_big = sub_big
			     val sub_little = sub_little
			     val subv_big = subv_big
			     val subv_little = subv_little
			     val update_big = update_big
			     val update_little = update_little)
  in
   open W
  end
 end;

(*
		24.	structure Word1
*)

structure Word1: FOXWORD =
 struct
  type word = bool
  val wordSize = 1

  local
   val low_mask = Word8.fromInt 0x01
   val index_mask = 0x07
   val zero = Word8.fromInt 0
   val one = Word8.fromInt 1
  in

   val bigEndian = false

   fun sub_big (array, index) =
        let val byte_index = Bits.>> (index, 0w3)
	    val shift = 7 - (Bits.andb (index, index_mask))
	    val byte = Word8Array.sub (array, byte_index)
	in Word8.andb (Word8.>> (byte, Word.fromInt shift), low_mask) <> zero
	end

   fun sub_little (array, index) =
        let val byte_index = Bits.>> (index, 0w2)
	    val shift = Bits.andb (index, index_mask)
	    val byte = Word8Array.sub (array, byte_index)
	in Word8.andb (Word8.>> (byte, Word.fromInt shift), low_mask) <> zero
	end

   fun subv_big (vector, index) =
        let val byte_index = Bits.>> (index, 0w3)
	    val shift = 7 - (Bits.andb (index, index_mask))
	    val byte = Word8Vector.sub (vector, byte_index)
	in Word8.andb (Word8.>> (byte, Word.fromInt shift), low_mask) <> zero
	end

   fun subv_little (vector, index) =
        let val byte_index = Bits.>> (index, 0w2)
	    val shift = Bits.andb (index, index_mask)
	    val byte = Word8Vector.sub (vector, byte_index)
	in Word8.andb (Word8.>> (byte, Word.fromInt shift), low_mask) <> zero
	end

   fun update_big (array, index, bool_value) =
        let val value = if bool_value then one else zero
	    val byte_index = Bits.>> (index, 0w3)
	    val shift = Word.fromInt (7 - (Bits.andb (index, index_mask)))
	    val byte = Word8Array.sub (array, byte_index)
	    val mask = Word8.notb (Word8.<< (low_mask, shift))
	    val res = Word8.orb (Word8.andb (byte, mask),
				 Word8.<< (value, shift))
	in Word8Array.update (array, byte_index, res)
	end

   fun update_little (array, index, bool_value) =
        let val value = if bool_value then one else zero
            val byte_index = Bits.>> (index, 0w3)
	    val shift = Word.fromInt (Bits.andb (index, index_mask))
	    val byte = Word8Array.sub (array, byte_index)
	    val mask = Word8.notb (Word8.<< (low_mask, shift))
	    val res = Word8.orb (Word8.andb (byte, mask),
				 Word8.<< (value, shift))
	in Word8Array.update (array, byte_index, res)
	end

   val sub = if bigEndian then sub_big else sub_little
   val subv = if bigEndian then subv_big else subv_little
   val update = if bigEndian then update_big else update_little

  end

  fun op+ (true, false) = true | op+ (false, true) = true | op+ _ = false
  val op- = op+
  fun op* (a, b) = a andalso b
  fun op div (dividend, divisor) = if not divisor then raise Div else dividend
  fun op mod (a, b) = if not b then raise Div else false
  fun andb (a, b) = a andalso b
  fun orb (a, b) = a orelse b
  fun xorb (a, b) = a + b
  fun notb value = not value
  fun min (false, _) = false | min (_, false) = false | min _ = true
  fun max (true, _) = true | max (_, true) = true | max _ = false
  fun op<< (a, s) = s=0w0 andalso a
  val op>> = op<<
  fun op~>> (a, _) = a
  fun op< (a, b) = not a andalso b
  fun op<= (a, b) = not a orelse b
  fun op> (a, b) = a andalso not b
  fun op>= (a, b) = a orelse not b
  fun fromInt 0 = false
    | fromInt 1 = true
    | fromInt n = fromInt (Integer.mod (n, 2))
  fun fromLargeWord w = if w = Word32.fromInt 0 then false
                        else if w = Word32.fromInt 1 then true
                        else fromLargeWord (Word32.andb (w, Word32.fromInt 1))
  fun toInt false = 0 | toInt true = 1
  fun toIntX false = 0 | toIntX true = ~1
  fun toLargeWord false = Word32.fromInt 0
    | toLargeWord true = Word32.fromInt 1
  fun toLargeWordX false = Word32.fromInt 0
    | toLargeWordX true = Word32.notb (toLargeWordX false)
  fun fromString s =
    case Word32.fromString s of
      NONE => NONE
    | SOME w => SOME (fromLargeWord w)
  fun toString false = "0" | toString true = "1"
  fun fmt _ = toString
  exception Word_Scan_Not_Implemented
  fun scan radix = raise Word_Scan_Not_Implemented

 end;

(*
		25.	structure Word64
*)

structure Word64: FOXWORD_DOUBLE =
 struct
  local
   val zero = Word32.fromInt 0
   fun to_word (high, low) = low
   fun from_word low = (zero, low)
   structure W = Word_Double (structure Base = Word32fox
			      val base_bits = 0w32
			      val to_word = to_word
			      val from_word = from_word)
  in
   open W
  end
 end;

(*
		26.	structure Word128
*)

structure Word128: FOXWORD =
 struct
  local
   val zero = Word64.fromInt 0
   fun to_word (high, low) = Word64.toLargeWord low
   fun from_word low = (zero, Word64.fromLargeWord low)
   structure W = Word_Double (structure Base = Word64
			      val base_bits = 0w64
			      val to_word = to_word
			      val from_word = from_word)
  in
   open W
  end
 end;

(*
		27.	structure Word256
*)

structure Word256: FOXWORD =
 struct
  local
   val zero = Word128.fromInt 0
   fun to_word (high, low) = Word128.toLargeWord low
   fun from_word low = (zero, Word128.fromLargeWord low)
   structure W = Word_Double (structure Base = Word128
			      val base_bits = 0w128
			      val to_word = to_word
			      val from_word = from_word)
  in
   open W
  end
 end;

(*
		28.	structure Word24

	Note: sub and update can only use Word32 sub/update
	for some parts of the word, depending on the alignment,
	since the array is not guaranteed to contain an integral
	number of 24-bit words.
*)

structure Word24: FOXWORD =
 struct
  local
   val andb = Word32.andb
   val orb = Word32.orb
   val sub8 = Word8Array.sub
   val subv8 = Word8Vector.sub
   val update8 = Word8Array.update
   val x8to32 = Word32.fromInt o Word8.toInt
   val byte_mask = Word32.fromInt 0xff
   fun x32to8 w = Word8.fromInt (Word32.toInt (andb (w, byte_mask)))

   fun sub_big (array, index) =
        orb (Word32.<< (x8to32 (sub8 (array, index)), 0w16),
	     orb (Word32.<< (x8to32 (sub8 (array, index + 1)), 0w8),
		  x8to32 (sub8 (array, index + 2))))

   fun sub_little (array, index) =
        orb (Word32.<< (x8to32 (sub8 (array, index + 2)), 0w16),
	     orb (Word32.<< (x8to32 (sub8 (array, index + 1)), 0w8),
		  x8to32 (sub8 (array, index))))


   fun subv_big (vector, index) =
        orb (Word32.<< (x8to32 (subv8 (vector, index)), 0w16),
	     orb (Word32.<< (x8to32 (subv8 (vector, index + 1)), 0w8),
		  x8to32 (subv8 (vector, index + 2))))

   fun subv_little (vector, index) =
        orb (Word32.<< (x8to32 (subv8 (vector, index + 2)), 0w16),
	     orb (Word32.<< (x8to32 (subv8 (vector, index + 1)), 0w8),
		  x8to32 (subv8 (vector, index))))

   fun update_big (array, index, value) =
        (update8 (array, index, x32to8 (Word32.>> (value, 0w16)));
         update8 (array, index + 1, x32to8 (Word32.>> (value, 0w8)));
         update8 (array, index + 2, x32to8 value))

   fun update_little (array, index, value) =
        (update8 (array, index + 2, x32to8 (Word32.>> (value, 0w16)));
         update8 (array, index + 1, x32to8 (Word32.>> (value, 0w8)));
         update8 (array, index, x32to8 value))

   structure W = Limit_Word (structure Base = Word32fox
			     val word_bits = 0w24
			     val sub_big = sub_big
			     val sub_little = sub_little
			     val subv_big = subv_big
			     val subv_little = subv_little
			     val update_big = update_big
			     val update_little = update_little)
  in
   open W
  end
 end;

(*
		29.	structure Word48

	Note: sub and update can only use Word32 sub/update
	for some parts of the word, depending on the alignment,
	since the array is not guaranteed to contain an integral
	number of 48-bit words.
*)

structure Word48: FOXWORD =
 struct
  local
   val andb = Word64.andb
   val orb = Word64.orb
   val x16to32 = Word32.fromInt o Word16.toInt
   val x32to16 = Word16.fromInt o Word32.toInt
   val word16_mask32 = Word32.fromInt 0xffff
   fun x32to16_mask value =
        Word16.fromInt (Word32.toInt (Word32.andb (value, word16_mask32)))
   fun combine_half (high, low) =
        Word32.orb (Word32.<< (high, 0w16), Word32.>> (low, 0w16))
   val word32_index_mask = 0x7		(* address mask of 32-bit word *)
   fun build_64_half (value, lower) =
        Word64.W (Word32.>> (value, 0w16),
		  Word32.orb (Word32.<< (value, 0w16), lower))

   fun sub_big (array, index) =
        if Bits.andb (index, word32_index_mask) = 0 then
	 build_64_half (Word32fox.sub_big (array, index),
			x16to32 (Word16.sub_big (array, index + 4)))
	else
	 Word64.W (x16to32 (Word16.sub_big (array, index)),
		   Word32fox.sub_big (array, index + 2))

   fun sub_little (array, index) =
        if Bits.andb (index, word32_index_mask) = 0 then
	 Word64.W (x16to32 (Word16.sub_little (array, index + 4)),
		   Word32fox.sub_little (array, index))
	else
	 build_64_half (Word32fox.sub_little (array, index + 2),
			x16to32 (Word16.sub_little (array, index)))

   fun subv_big (vector, index) =
        if Bits.andb (index, word32_index_mask) = 0 then
	 build_64_half (Word32fox.subv_big (vector, index),
			x16to32 (Word16.subv_big (vector, index + 4)))
	else
	 Word64.W (x16to32 (Word16.subv_big (vector, index)),
		   Word32fox.subv_big (vector, index + 2))

   fun subv_little (vector, index) =
        if Bits.andb (index, word32_index_mask) = 0 then
	 Word64.W (x16to32 (Word16.subv_little (vector, index + 4)),
		   Word32fox.subv_little (vector, index))
	else
	 build_64_half (Word32fox.subv_little (vector, index + 2),
			x16to32 (Word16.subv_little (vector, index)))

   fun update_big (array, index, Word64.W (high, low)) =
        if Bits.andb (index, word32_index_mask) = 0 then
	 (Word32fox.update_big (array, index, combine_half (high, low));
	  Word16.update_big (array, index + 4, x32to16_mask low))
	else
	 (Word16.update_big (array, index, x32to16 high);
	  Word32fox.update_big (array, index + 2, low))

   fun update_little (array, index, Word64.W (high, low)) =
        if Bits.andb (index, word32_index_mask) = 0 then
	 (Word32fox.update_little (array, index, low);
	  Word16.update_little (array, index + 4, x32to16_mask high))
	else
	 (Word16.update_little (array, index, x32to16_mask low);
	  Word32fox.update_little (array, index + 2, combine_half (high, low)))

   structure W = Limit_Word (structure Base = Word64
			     val word_bits = 0w48
			     val sub_big = sub_big
			     val sub_little = sub_little
			     val subv_big = subv_big
			     val subv_little = subv_little
			     val update_big = update_big
			     val update_little = update_little)
  in
   open W
  end
 end;

