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
Block implementation

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	structure HalfBlock
	2.      functor BlockFun

		iii.	RCS Log
	
$Log: block.fun,v $
Revision 1.1  1994/06/14  14:05:22  robby
Initial revision

Revision 1.2  94/01/17  08:27:03  necula
*** empty log message ***

Revision 1.1  94/01/13  18:46:22  necula
Initial revision

		1.	structure HalfBlock
*)

			      (* This is the implementation of the 32 DES 
                               * bit data structure. The actual 
                               * implementation is 32 bits packed into a 
                               * ubyte4 (bit DES 0 is the most significative 
                               * bit in the word).*)  
                               
structure HalfBlock : HALFBLOCK =
  struct

	type T = ubyte4
	val debug = false

	fun internalize u=u
	  
	val xor = Byte4.xor
	  
	val empty = 4u0
			      (* Circulary left shifts the first 28 bits in a 
                               * ubyte4  *)
	fun shift28 (block, n) =
	  let
	    val mask = 4uxFFFFFFFF - Byte4.>> (4uxFFFFFFFF, 32 - n)
			      (* Mask to 0 the upper bits *)
	    val word = Byte4.&&(block, 4uxFFFFFFF0)
	    val discard = Byte4.>> (Byte4.&& (word, mask), 28 - n)
	  in
	    Byte4.|| ( Byte4.<< (word, n), discard)
	  end
			      (* Leftshifts the ubyte4 and returns also the 
                               * discarded bits in the low part of a ubyte4. 
                               * As it may be seen from the signature the 
                               * ubyte4 in the representation of the discarded 
                               * bits has nothing to do with the 
                               * representation of HALFBLOCK.  *)
	fun leftshift (block, n) =
	  let
	    val mask = 4uxFFFFFFFF - Byte4.>> (4uxFFFFFFFF, 32 - n)
			      (* Mask to 0 the upper bits *)
	    val discard = Byte4.>> (Byte4.&& (block, mask), 28 - n)
	  in
	    (Byte4.<< (block, n), discard)
	  end
			      (* Extracts a range of bits in the lower 
                               * portions of a ubyte4 *)
	fun extractbits (block, start, len) =
	  let
	    val mask = Byte4.<<(4u1, len) - 4u1 (*This is a mask of len ones *)
	  in
	    Byte4.&& (mask, Byte4.>>(block, 32 - start -len))
	  end

	    
			      (* Function to set a range of LEN bits starting 
                               * at START with the values contained in VALUE *)
	fun setbits (block, start, len, value) =
	  let
	    val mask = Byte4.&&(value, Byte4.<<(4u1, len) - 4u1)
	  in
	    Byte4.||(block, Byte4.<<(mask, 32 - start -len))
	  end

			      (* Reverse the 32 bits in a ubyte4 *)
	fun reverse block =
	  let
	    fun loop (4u0, _, rez) = rez
	      | loop (maskl, maskr, rez) =
		let
		  val newrez = if Byte4.&&(maskl, block) = 4u0
				 then
				   rez else Byte4.||(rez, maskr)
		in
		  loop( Byte4.<<(maskl, 1), Byte4.>>(maskr, 1), newrez)
		end
	  in
	    loop(4u1, 4ux80000000, 4u0)
	  end
	
		    
			      (* Parity bits are DES 8, 16, 24, 32 ,..  . In 
                               * our numbering system are 7, 15, ... DES 
                               * specifies that each group of 8 bits must have 
                               * odd parity (an odd number of bits one).
			       *)
	local
			      (* Computes the parity. 1u0 means an evennumber 
                               * of bits one. *)
	    fun byte_parity 4u0 = 4u0
	      | byte_parity b = Byte4.xor(Byte4.&& (b, 4u1) ,
					  byte_parity (Byte4.>>(b,1)))
	in 
	  fun check_parity (block) =
	    let

	      fun loop (lw, 32) = true
		| loop (lw, shift) =
		  if byte_parity(Byte4.&&(4uxFF, Byte4.>>(lw, shift))) = 4u0
		    then
		      false
		  else
		    loop(lw, shift + 8)
	    in
	      loop(block, 0)
	    end

	  fun fix_parity (block) =
	    let
	      fun loop (lw, 32, mask) = (true, mask)
		| loop (lw, shift, mask) =
		  if byte_parity(Byte4.&&(4uxFF, Byte4.>>(lw, shift))) = 4u0
		    then
		      loop(lw, shift + 8, Byte4.||(4ux01000000,
						   Byte4.>>(mask, 8)))
		  else
		    loop(lw, shift + 8, Byte4.>>(mask, 8))
		    
	      val (fixit, mask) = loop(block, 0, 4u0)
	    in
	      if fixit then
		Byte4.xor(block, mask)
	      else
		block
	    end
	end
 
			      (* Prints a ubyte4 as a sequence of bits only if 
                               * debug is turned on *)
	fun print value =     
	  let
	    fun Byte4binary (_, 4u0, _) = ()
	      | Byte4binary (value, mask, nr_bit) =
		(
		 if nr_bit mod 8 = 0 then String.print " " else ();
		   String.print (if Byte4.&& (value, mask) = 4u0
				   then "0" else "1");
		   Byte4binary ( value, Byte4.>>(mask,1), nr_bit+1)
		   )
	  in
	    if debug then 
	      Byte4binary (value, 4ux80000000, 0)
	    else
	      ()
	  end

  end

(*
                    2.  functor BlockFun
*) 
			      (* The implementation of 64 DES bits as a 
                               * flat_vector of 2 HALFBLOCKs. Knows nothing 
                               * about the structure of HALFBLOCKs. Exports 
                               * roughly the same interface as HALFBLOCK but 
                               * expanded to 64 bit. The spliting of BLOCK 
                               * representation in BLOCK and HALFBLOCKs was 
                               * done mainly for convenience on machines with 
                               * 32 bit words but also because DES requires to 
                               * be able to split a 64 bit quantity in two 
                               * parts and to build one from two halves. *)
functor BlockFun (structure HalfBlock : HALFBLOCK) : BLOCK =
  struct

    open Flat_Vector
    val debug = false
      
    structure HalfBlock = HalfBlock
      
    type T = HalfBlock.T flat_vector

    fun split fv = (flat_sub (fv, 0), flat_sub(fv, 1))

    fun build (b0, b1) = flat_vector([b0,b1])

    val empty = build(HalfBlock.empty, HalfBlock.empty)

    fun print (fv, text) =
      let
	val (b0, b1) = split fv
      in
	if debug then
	  (String.print text;
	   String.print "=\n";
	   HalfBlock.print(b0);
	   String.print " - ";
	   HalfBlock.print(b1);
	   String.print "\n")
	else
	  ()
      end
 
    fun xor (fv0, fv1) =
      let
	val (b00, b01) = split fv0
	val (b10, b11) = split fv1

	val res = build(HalfBlock.xor(b00, b10),
			HalfBlock.xor(b01, b11))
	val  _ = print (res, "xor: output")
      in
	res
      end

			      (* The SHIFT28 function shifts totally separate 
                               * the two halves *)
    fun shift28 (fv, n) =
      let
	val (b0, b1) = split fv
	val res = build(HalfBlock.shift28(b0, n),
			HalfBlock.shift28(b1, n))
	val _ = print (res, "shift28: output")
      in
	res
      end
			      (* LEFTSHIFT propagates the discarded bits from 
                               * one half to the other *)
    fun leftshift (fv, n) =
      let
	val (b0, b1) = split fv
	val (newb0, _) = HalfBlock.leftshift(b0, n)
	val (newb1, overflow) = HalfBlock.leftshift(b1, n)
	  
      in
	build(HalfBlock.setbits(newb0, 32 - n, n, overflow),
	      newb1)
      end

			      (* EXTRACTBITS detectes where the wanted bits 
                               * are and calls the correspondent function on 
                               * the respective half. It is able to extract 
                               * also a range of bits that spans on both 
                               * halves. The bits are returned in the lower 
                               * portion of a ubyte4. *)
    fun extractbits (fv, start, len) =
      let
	val (b0, b1) = split fv
      in
	if start + len <= 32 then (* Look in the first block *)
	  HalfBlock.extractbits(b0, start, len)
	else
	  if start >= 32 then (* Look in the second block *)
	    HalfBlock.extractbits(b1, start - 32, len)
	  else
			      (* Part of the bits are in the first *)
	    let
	      val part0 = HalfBlock.extractbits(b0, start, 32 - start)
	      val part1 = HalfBlock.extractbits(b1, 32, start + len - 32)
	    in
	      Byte4.||(part1, Byte4.<<(part0, start + len - 32))
	    end
      end

			      (* SETBITS calls the same function fro the half 
                               * where the bits live but is also able to set a 
                               * range of bits that spans over both halves. 
                               * This feature is not used by now. *)
    fun setbits (fv, start, len, value) =
      let
	val (b0, b1) = split fv
      in
	if start + len <= 32 then (* Look in the first block *)
	  build(HalfBlock.setbits(b0, start, len, value),
		b1)
	else
	  if start >= 32 then (* Look in the second block *)
	    build(b0,
		  HalfBlock.setbits(b1, start - 32, len, value))
	  else
			      (* Part of the bits are in the first *)
	    let
	      val value0 = Byte4.>>(value, start + len - 32)
	    in
	      build(HalfBlock.setbits(b0, start, 32 - start, value0),
		    HalfBlock.setbits(b1, 32, start + len - 32, value))
	    end
      end

    
    fun reverse fv =
      let
	val (b0, b1) = split fv
      in
	build(HalfBlock.reverse(b1), HalfBlock.reverse(b0))
      end

    
			      (* Parity bits are DES 8, 16, 24, 32 ,..  . In 
                               * our numbering system are 7, 15, ...
			       *)
    fun check_parity (key) =
      let
	val (b0, b1) = split key 
      in
	HalfBlock.check_parity(b0) andalso
	HalfBlock.check_parity(b1)
      end

    fun fix_parity (key) =
      let
	val (b0, b1) = split key
	val res = build(HalfBlock.fix_parity(b0), HalfBlock.fix_parity(b1))
      in
	res
      end

    			      (* The following functions are for viewing the 
                               * block as a Key and externally as a ubyte1 
                               * flat_vector locative and a pair of ubyte4s.*)

    datatype ext = FVL of ubyte1 flat_vector locative
      | Pair of ubyte4 * ubyte4

    fun internalize (FVL extern)=
      let
	open Locative 
	fun loop(8 , rez) = rez
	  | loop(byte, rez) = loop(byte+1, 
				   setbits(rez, byte*8, 8,
 			  Byte4.from_int(Byte1.to_int(locget(locsub(extern,
								    byte))))))
      in
	loop (0, empty)
      end
      | internalize (Pair (u,u'))=
	Flat_Vector.flat_vector [HalfBlock.internalize u,
				 HalfBlock.internalize u']
	
    fun externalize key =
      let
	open Locative 
	val result = locative (Flat_Vector.flat_vector_const(1u0, 8))
	fun loop 8  = ()
	  | loop byte =
	    (locset(locsub(result, byte),
		    Byte1.from_int(Byte4.to_int(extractbits(key,
							    byte*8, 8))));
	     loop (byte+1))
	    
	val _ = loop 0
      in
	FVL result
      end

  end;

