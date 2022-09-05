(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
        George Necula (George.Necula@@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@@cs.cmu.edu)
        Nick Haines (Nick.Haines@@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

DES encryption support

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature BLOCK

		iii.	RCS Log
	
$Log: block.sig,v $
Revision 1.3  1996/03/22  22:41:41  necula
Completely rewritten implementation. Changed from locatives and Flat_Vector to
Word8Array and Word64 (FOXWORD).

Revision 1.2  1994/06/14  14:05:58  robby
added functionality to enternalize and internalize

Revision 1.1  94/01/13  18:46:31  necula
Initial revision

		1.	signature BLOCK
*)

			 (* BLOCK.T is the base type in the implementatation 
                          * of DES. It encodes a block of 64 bits. We will 
                          * refer to the bits as the bits 0 .. 63 following 
                          * the DES specification.  *)
signature BLOCK =
  sig

      type T
			 (* 64 zero bits  *)
      val zero   : T

			 (* Builds a list of blocks from an array (vector). 
                          * The array is padded with 0 to make its length a 
                          * multiple of 8. The first block is loaded from the 
                          * first 8 bytes, such that the most significative 
                          * bit in byte 0 is bit DES 0.  *)
      val fromArray : Word8Array.array -> T list
      val fromVector : Word8Vector.vector -> T list

			 (* The inverse of the fromArray and fromVector 
                          * functions. The resulting array has a length which 
                          * is mutiple of 8 *)
      val toArray   : T list -> Word8Array.array
      val toVector   : T list -> Word8Vector.vector
	  
			 (* Split the 64 bits in two portions of 32 bits. 
                          * That is the original DES 0 becomes bit 0 in the 
                          * first result, while DES32 becoems bit 0 in the 
                          * second result.  *)
      val split   : T -> T * T

			 (* The inverse of split *)
      val combine : T * T -> T

			 (* Shift left the DES bits such that old bit n 
                          * becomes new bit 0  *)
      val shleft : T * int -> T

			 (* Bit-wise xor *)
      val xor     : T * T -> T

			 (* Extract a range (start + length ) of bits from a 
                          * block. The last bit extracted is the least 
                          * significative bit of the result. *)
      val extractbits : T * Word31.word * Word31.word -> int
			 (* Sets a range of bits (start + length). The last 
                          * bit set is the least significative of the input 
                          * integer  *)
      val setbits     : T * Word31.word * Word31.word * int -> T

			 (* Permutes a block according to a permutation 
                          * table. The i-th element in the permutation table 
                          * specifies the position in the input block of the 
                          * i-th result bit. The permutation name is also 
                          * given for debugging printouts.  *)
      val permute : (string  * Word31.word Vector.vector) -> T -> T

			 (* Reverses the order of bits in a block *)
      val reverse : T -> T


			 (* Performs two simultaneous circular left shifts of 
                          * the bits 0-27 and 32-59. Strange but required by 
                          * DES  *)
      val shift28 : T * Word31.word -> T


			 (* Checks the DES parity bits (no 7, 15, ...). The 
                          * parity requirement is that each group of 8 bits 
                          * has odd parity.  *)
      val check_parity : T -> bool
			 (* Fixed the parity as above *)
      val fix_parity   : T -> T

			 (* For debugging, prints the 64 bits, bit 0 first *)
      val print_block : T -> unit
  end;
  
