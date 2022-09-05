(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A one's complement, 16-bit checksum module. 

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Checksum
	2.	constants
	3.	one_s functions
	4.	function check_partial
	5.	function complete_partial
	6.	function checksum

		iii.	RCS Log
	
$Log: checksum.fun,v $
Revision 1.24  1997/02/05  20:09:05  esb
added some optimized code, commented out for now.

Revision 1.23  1996/10/13  18:50:19  esb
adapted to value restriction.

Revision 1.22  1996/03/12  22:28:34  esb
adapted to new FOXWORD.

Revision 1.21  1996/01/19  23:09:10  esb
adapted to the new wordarray signature.

Revision 1.20  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.19  1995/11/12  16:42:23  esb
adapted to new Word_Array.

Revision 1.18  1995/09/23  19:47:29  esb
fixed a bug.

Revision 1.17  1995/06/22  17:25:04  esb
changed endianness of result so it is always big-endian.

Revision 1.16  1995/06/20  17:36:55  esb
adapted to word-arrays.

Revision 1.15  1995/03/10  03:52:24  esb
adapted to new vendor.sig.

Revision 1.14  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.13  1994/09/30  16:27:06  esb
changed to work with FoxWord instead of ByteN

Revision 1.12  1994/08/12  04:09:47  esb
Now returns zero for checksumming zero-sized arrays.

Revision 1.11  1994/06/16  16:51:18  danwang
Updated for functorized Fox_Basis

Revision 1.10  1994/02/25  22:56:29  esb
replaced mod with and.

Revision 1.9  94/02/25  18:20:56  milnes
Added safe/unsafe switch.

Revision 1.8  1994/02/08  14:10:14  esb
added checklistoff and Checksum_Bounds; improved speed twofold.

Revision 1.7  1994/01/31  01:08:27  esb
optimized.

Revision 1.6  1993/12/23  23:13:23  esb
changed check_sum to checksum and added checklist.

Revision 1.5  1993/10/25  19:37:07  cline
removed .U from Byte[421].U

Revision 1.4  1993/07/23  17:36:32  esb
made it work for odd high and low, and for small arrays.

Revision 1.3  1993/07/22  18:57:27  nickh
Adjusted offsets for sub and update to use byte offsets.

Revision 1.2  1993/06/19  01:25:21  esb
added functor argument do_prints, and do_if_debug.
also commented out some print statemets that might slow us down.

Revision 1.1  1993/06/10  22:41:20  milnes
Initial revision


		1.	functor Checksum
*)


functor Checksum (structure V: VENDOR 
                  structure Debug: DEBUG): CHECKSUM =
 struct

  type partial_state = {previous: Word16.word, odd: Word8.word option} 
  val initial_state = {previous = Word16.fromInt 0,
		       odd = NONE}: partial_state

  exception Checksum_Bounds

(*
		2.	constants
*)

  val max_int = 0xffff
  val max_word = Word.fromInt 0xffff
  val max16 = Word16.fromInt max_int
  val max32 = Word32.fromInt max_int
  val limit = 0x10000
  val limit32 = Word32.fromInt limit
  val byte_mask = 0xff
  val byte_mask32 = Word32.fromInt byte_mask
  val one16 = Word16.fromInt 1
  val one32 = Word32.fromInt 1
  val zero8 = Word8.fromInt 0
  val zero16 = Word16.fromInt 0
  val zero32 = Word32.fromInt 0

(*
		3.	one_s functions
*)

  val one_s_complement = Word16.notb

  fun one_s_add (a, b) =
       let val sum = Word16.toInt a + Word16.toInt b
	   fun andb (i,j) =
	     Word32.toInt (Word32.andb (Word32.fromInt i, Word32.fromInt j))
	   val masked = andb (sum, max_int)
	   val corrected = if masked = sum then sum else masked + 1
       in Word16.fromInt corrected
       end

(*
		4.	function check_partial

	RFC 1071 gives various techniques for computing fast checksums.
	Of the four, this loop implements 2:  Deferred carries,
	where we add the 16-bit values in 32-bit accumulators and
	only fold the result into a 16-bit value at the end, and
	unwinding loops: we read 4 bytes at a time.
*)

  local
   fun build_bytes (left, right) =
        let val left_int = Word8.toInt left
	    val right_int = Word8.toInt right
	    fun orb (i,j) =
	      Word32.toInt (Word32.orb (Word32.fromInt i, Word32.fromInt j))
	    fun lshift (i,j) =
	      Word32.toInt (Word32.<< (Word32.fromInt i, Word31.fromInt j))
	    val result =
	          if Word16.bigEndian then
		   orb (lshift (left_int, 8), right_int)
		  else
		   orb (left_int, lshift (right_int, 8))
	in Word16.fromInt result
	end

   fun swap_bytes value =
        Word16.orb (Word16.>> (value, 0w8),
		       Word16.<< (value, 0w8))

   fun make_big_endian value =
        if Word16.bigEndian then value else swap_bytes value

   fun convert_array array =
        let val alignment_f = (0w4 - Word_Array.alignment_f array) mod 0w4
            val alignment_r = Word_Array.alignment_r array mod 0w4
	    val unaligned32 = Word_Array.to32 array
            val w8 = Word_Array.to8 array
	    val length8 = Word_Array.W8.U_Big.F.length w8
	in (* 
	   print ("length8 = " ^
		  (Int.toString o Word.toInt) length8 ^ ", alignment_f = " ^
		  (Int.toString o Word.toInt) alignment_f ^
		  ", alignment_r = " ^
		  (Int.toString o Word.toInt) alignment_r ^ "\n");
*)
	   if length8 < 0w4 then
	     (w8,
	      Word_Array.W32.Big.F.create (Word32.fromInt 0, 0w0),
	      Word_Array.W8.Big.F.create (0w0, 0w0))
	   else
	     (Word_Array.W8.U_Big.R.seek (w8, length8 - alignment_f),
	      Word_Array.W32.align unaligned32,
	      Word_Array.W8.U_Big.F.seek (w8, length8 - alignment_r))
	end

   fun initial_odd (previous, odd, NONE) = (previous, odd)
     | initial_odd (previous, NONE, SOME (head, rest)) =
        initial_odd (previous, SOME head, Word_Array.W8.U_Big.F.next rest)
     | initial_odd (previous, SOME odd, SOME (head, rest)) =
        initial_odd (one_s_add (previous, build_bytes (odd, head)), NONE,
		     Word_Array.W8.U_Big.F.next rest)

   fun final_odd (previous, NONE, result, NONE) =
        {previous = one_s_add (previous, result), odd = NONE}
     | final_odd (previous, NONE, result, SOME new_odd) =
        {previous = one_s_add (previous, result), odd = SOME new_odd}
     | final_odd (previous, SOME old_odd, result, NONE) =
        {previous = one_s_add (previous, swap_bytes result),
	 odd = SOME old_odd}
     | final_odd (previous, SOME old_odd, result, SOME new_odd) =
        {previous = one_s_add (one_s_add (previous, swap_bytes result),
			       build_bytes (old_odd, new_odd)), odd = NONE}

   fun final_loop (NONE, accumulator, odd) = (accumulator, odd)
     | final_loop (SOME (head, rest), accumulator, NONE) =
        final_loop (Word_Array.W8.U_Big.F.next rest, accumulator, SOME head)
     | final_loop (SOME (head, rest), accumulator, SOME odd) =
        final_loop (Word_Array.W8.U_Big.F.next rest,
		    one_s_add (accumulator, build_bytes (odd, head)), NONE)

   fun add_carry n =
        if Word32.<= (n, max32) then n
	else
	 add_carry (Word32.+ (Word32.>> (n, 0w16), Word32.andb (n, max32)))

   fun check_partial_loop (array8, {previous, odd}, fold_check) =
        let val (initial_bytes, array32, final_bytes) = convert_array array8
	    val (new_previous, new_odd) =
	          initial_odd (previous, odd,
			       Word_Array.W8.U_Big.F.next initial_bytes)
	    val result = Word_Array.W32.Native.F.fold fold_check zero32 array32
	    val folded = add_carry result
	    val result16 = Word16.fromInt (Word32.toInt folded) 
	    val (final_result, final_odd_value) =
	          final_loop (Word_Array.W8.U_Big.F.next final_bytes,
			      result16, NONE)
	in final_odd (new_previous, new_odd, final_result, final_odd_value)
	end

   fun fold_64k_max (new, accumulator) =
        Word32.+ (Word32.+ (Word32.>> (new, 0w16), Word32.andb (new, max32)),
		  accumulator)

   fun fold_unlimited (new, accumulator) =
        Word32.+ (Word32.+ (Word32.>> (new, 0w16), Word32.andb (new, max32)),
		  Word32.+ (Word32.>> (accumulator, 0w16),
			    Word32.andb (accumulator, max32)))

(*
   fun fast_check_64k_max (array8, {previous, odd}) =
        let val (initial_bytes, array32, final_bytes) = convert_array array8
	    val (new_previous, new_odd) =
	          initial_odd (previous, odd,
			       Word_Array.W8.U_Big.F.next initial_bytes)
	    val (data, first_w, last_w) =
	            Word_Array.expose
		       (Word_Array.from32
		        (Word_Array.W32.unalign array32))
	    fun ones_add (new, accumulator) =
	         Word32.+ (Word32.+ (Word32.>> (new, 0w16),
				     Word32.andb (new, max32)), accumulator)
	    val last = Word.toInt last_w div 4
	    fun loop (index, value) =
		 if index > last then value
		 else
		  loop (index + 1,
			ones_add (Pack32Little.subArr (data, index), value))
	    val result = loop (Word.toInt first_w div 4, zero32)
	    val folded = add_carry result
	    val result16 = Word16.fromInt (Word32.toInt folded) 
	    val (final_result, final_odd_value) =
	          final_loop (Word_Array.W8.U_Big.F.next final_bytes,
			      result16, NONE)
	in final_odd (new_previous, new_odd, final_result, final_odd_value)
	end
*)

  in
   fun check_partial (array, partial) =
        let val size = Word_Array.W8.U_Big.F.length (Word_Array.to8 array)
	    val fold = if size <= max_word then fold_64k_max
		       else fold_unlimited
	in check_partial_loop (array, partial, fold)
(*
	in if size <= max_word then fast_check_64k_max (array, partial)
	   else check_partial_loop (array, partial, fold_unlimited)
*)
	end

(*
		5.	function complete_partial
*)

   fun complete_partial {previous, odd = NONE} = make_big_endian previous
     | complete_partial {previous, odd = SOME byte} =
        make_big_endian (one_s_add (previous, build_bytes (byte, zero8)))

  end (* local *)

(*
		6.	function checksum
*)

  fun checksum array =
       complete_partial (check_partial (array, initial_state))

 end (* struct *)


