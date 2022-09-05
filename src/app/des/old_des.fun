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
	1.	functor DES

		iii.	RCS Log
	
$Log: old_des.fun,v $
Revision 1.2  1994/07/13  19:16:55  robby
*** empty log message ***

Revision 1.4  1994/01/06  13:44:47  necula
*** empty log message ***

Revision 1.3  93/12/28  16:19:10  necula
Changed the interface to a more functional one.

Revision 1.2  93/12/22  16:36:00  cline
Added comments and TOC

Revision 1.1  93/12/22  16:32:26  necula
Initial revision


		1.	functor DES
*)
                              (* DES Functor *)

functor Old_DES (structure B:FOX_BASIS
		 val debug : bool) : OLD_DES =

  struct

    val InitMask = 4ux80000000
      
    exception Key_Parity 

			      (* CBlock is the basic data type for a DES 
                               * key. It stores the DES 64 bits on two 
                               * ubyte4 so that bit DES 1 is the most 
                               * significative in the first ubyte4, DES 31 
                               * is the least significative bit in the first 
                               * ubyte4 and DES 32 is the most significative 
                               * in the second ubyte4  *)

			      (* When a CBlock is represented as a ubyte1 
                               * flat_vector the first byte contains the DES 
                               * bits 1 to 8 (1 is the most significative) *)

			      (* In this program the bits are numbered 
                               * starting from 0 so that DES 1 is bit number 0 
                               * *)
    type CBlock = ubyte4 flat_vector locative
			      (* Schedule is the data type for a schedule. 
                               * It consists of a vector of 16 vectors each 
                               * with 2 ubyte4  *)
    type Schedule = ubyte4 flat_vector flat_vector locative


			      (* Local Functions *)


			      (* Conversion from ubyte1 to ubyte4 and back *)
    fun Byte1to4 b = Byte4.from_int(Byte1.to_int(b))
    fun Byte4to1 lw = Byte1.from_int(Byte4.to_int(lw))

			      (* Shortcuts for dereferrencing a locative *)
    fun lset (loc, idx, value) = Locative.locset(Locative.locsub(loc,idx),
						 value)
    fun lget (loc, idx) = Locative.locget(Locative.locsub(loc,idx))


      
			      (* Build a cblock from ubyte flat_vector*)
    fun ubyte_to_cblock (fv) =
      let
        val cb = locative (Flat_Vector.flat_vector([4u0,4u0]))
	  
      in

	lset(cb, 0, Byte4.||(Byte1to4(lget(fv,3)),
			     Byte4.||(Byte4.<<(Byte1to4(lget(fv,2)),8),
				      Byte4.||(Byte4.<<(Byte1to4(lget(fv,
								      1)),16),
					       Byte4.<<(Byte1to4(lget(fv,
								  0)),24)))));
	lset(cb, 1, Byte4.||(Byte1to4(lget(fv,7)),
			     Byte4.||(Byte4.<<(Byte1to4(lget(fv,6)),8),
				      Byte4.||(Byte4.<<(Byte1to4(lget(fv,
								      5)),16),
					       Byte4.<<(Byte1to4(lget(fv,
								  4)),24)))));
	cb 
      end

    
    fun cblock_to_ubyte_intern (cb, fv) =
      let
	val lw0 = lget(cb, 0)
	val lw1 = lget(cb, 1)
      in
	lset(fv, 0, Byte4to1(Byte4.&&(4uxFF, Byte4.>>(lw0, 24))));
	lset(fv, 1, Byte4to1(Byte4.&&(4uxFF, Byte4.>>(lw0, 16))));
	lset(fv, 2, Byte4to1(Byte4.&&(4uxFF, Byte4.>>(lw0, 8))));
	lset(fv, 3, Byte4to1(Byte4.&&(4uxFF, Byte4.>>(lw0, 0))));
	lset(fv, 4, Byte4to1(Byte4.&&(4uxFF, Byte4.>>(lw1, 24))));
	lset(fv, 5, Byte4to1(Byte4.&&(4uxFF, Byte4.>>(lw1, 16))));
	lset(fv, 6, Byte4to1(Byte4.&&(4uxFF, Byte4.>>(lw1, 8))));
	lset(fv, 7, Byte4to1(Byte4.&&(4uxFF, Byte4.>>(lw1, 0))))
      end

			      (* Functions to work with ubyte1 flat_vectors *)

			      (* COPY_BYTES *)
			      (*  inp   = source flat_vector locative *)
			      (*  ilen  = its length (if bytes are requested 
                               *    beyond this value the source vector would 
                               *    be considered as padded with 1u0   *)
			      (*  ipos  = starting position in source vector *)
			      (*  output= destination vector *)
			      (*  opos  = starting position in the destination 
                               * *)
			      (*  nrbytes= how many bytes to copy *)
    fun copy_bytes (inp, ilen, ipos, output, opos, nrbytes) =
      if nrbytes = 0 then
	()
      else
	let
	  val oval =
	    if ipos < ilen then
	      lget(inp, ipos)
	    else
	      1u0
	in
	  (
	   lset(output, opos, oval);
	   copy_bytes (inp, ilen, ipos+1, output, opos+1, nrbytes-1)
	   )
	end

			      (* BYTE_PARITY *)
			      (* Xors all the bits in a ubyte1 and returns the 
                               * result *)
     
    fun byte_parity 1u0 = 1u0
      | byte_parity b = Byte1.xor(Byte1.&& (b, 1u1) ,
				  byte_parity (Byte1.>>(b,1)))


			      (* XOR_BYTES *)
			      (* computes the bytewise xor between two arrays 
                               * and puts the result in the place of the first 
                               * argument  *)
    fun xor_bytes (fl1, fl2, nrbytes) =
      let
	fun iterate 0 = ()
	  | iterate n = (lset(fl1,n-1, Byte1.xor(lget(fl1,n-1),
						 lget(fl2,n-1)));
			 iterate (n-1)
			 )
      in
	iterate nrbytes
      end
    
      
                              (* The function to test one bit in a CBlock*)
    fun is_bit_set (fvl, bit) =
      let
	val idx = Byte1.to_int ( Byte1.>> ( bit, 5))
	val mask = Byte4.>>( InitMask, Byte1.to_int( Byte1.&& ( bit , 1u31)))
	val lc = Locative.locsub (fvl, idx)
      in
	Byte4.&&( Locative.locget(lc), mask) <> 4u0 
      end


    

			      (* Left shift the bits in the positions 0-27 and 
                               * put the result back
			       *)
    fun LeftShift ( loc, n ) =
      let
	val mask = 4uxFFFFFFFF - Byte4.>> (4uxFFFFFFFF, 32 - n)
			      (* Mask to 0 the upper bits *)
	val word = Byte4.&&( Locative.locget(loc), 4uxFFFFFFF0)
	val discard = Byte4.>> (Byte4.&& (word, mask), 28 - n)
      in
	Locative.locset( loc, Byte4.|| ( Byte4.<< (word, n), discard))
      end

      
			      (* Xor 2 Byte4 and substitute the result for 
                               * the first argument. This is a shortcut for 
                               * xor_bytes when we have the 4 bytes packed 
                               * in a ubyte4  *)
    fun Xor (l1 ,l2) =
      Locative.locset( l1, Byte4.xor( Locative.locget(l1),
				     Locative.locget(l2)))
  

			      (* Debugging functions *)
    
			      (* Print a Byte4 in binary *)
    fun PrintByte4 value =
      let
	fun Byte4binary (value, mask, nr_bit) =

	  (
	   if nr_bit mod 8 = 0 then
	     print " "
	   else
	     ();

	   if Byte4.&& (value, mask) = 4u0 then print "0" else  print "1";

	   if nr_bit = 31 then
	     ()
	   else
	     Byte4binary ( value, Byte4.>>(mask,1), nr_bit+1)
	  )
      in
	if debug then 
	  Byte4binary (value, InitMask, 0)
	else
	  ()
      end

			      (* Print one byte in hexadecimal *)
    fun PrintByteHex b =
      let
	val map = "0123456789abcdef"
	fun printdigit v =  print (substring(map, Byte1.to_int(v), 1))
      in
	(
	 printdigit (Byte1.&&(1uxF, Byte1.>>(b,4)));
	 printdigit (Byte1.&&(1uxF, b))
	 )
      end
    
	 	 
		       
			      (* Function to print a 64 bit CBlock*)
    fun PrintBitArray (ba, text:string)  =
      (
       if debug then
	 (
	  print text;
	  print "=\n";
	  PrintByte4( lget(ba, 0));
	  print " - ";
	  PrintByte4( lget(ba, 1));
	  print "\n"
	 )
       else
	 ()
      )

			      (* Print the schedule *)
    fun PrintSchedule sched =

      let
	fun iterate 17 = ()
	  | iterate n  =
	    (
	     PrintBitArray(Locative.locsub(sched, n - 1),
			   "Schedule " ^ Integer.makestring(n));
	     iterate (n+1)
	    )
      in
	iterate 1
      end


    fun PrintFlatVector (fv, len, str : string) =

      let
	  
	fun iterate n =
	  if n >= len then
	    ()
	  else
	    (
	     PrintByteHex(lget(fv,n));
	     iterate (n+1)
	     )
      in
	if debug then
	  (
	   print (str ^ "\n");
	   iterate 0;
	   print "\n"
	   )
	else
	  ()
      end
    

			      (* Parity bits are DES 8, 16, 24, 32 ,..  . In 
                               * our numbering system are 7, 15, ...
			       *)
    fun check_key_parity (key) =
      let
	fun iter_long_word (lw, 32) = ()
	  | iter_long_word (lw, shift) =
	    let
	      val to_check =
		Byte4to1(Byte4.&&(4uxFF, Byte4.>>(lw, shift)))
	    in
	      if byte_parity(to_check) = 1u0 then
		raise Key_Parity
	      else
		iter_long_word(lw, shift + 8)
	    end
	val _ = PrintBitArray(key, "Before check_key_parity")		
	val _ = iter_long_word( lget(key,0),0)
	val _ = iter_long_word( lget(key,1),0)
      in
	()
      end
				 

    fun fix_key_parity (key) =
      let
	fun iterate 8 = ()
	  | iterate n =
	    let
	      val loc = Locative.locsub(key,n)
	      val to_check = Locative.locget(loc)
	      val new_byte = 
		if byte_parity(to_check) = 1u0 then
		  Byte1.xor(to_check, 1u1)
		else
		  to_check
	    in
	      (
	       Locative.locset(loc, new_byte);
	       iterate (n+1)
	       )
	    end
      in
	(
	 PrintFlatVector(key, 8, "Before fix_key_parity");
	 iterate 0;
	 PrintFlatVector(key, 8, "After fix_key_parity")
	 )
      end
      		
    
			      (* TABLES *)

			      (* Permuted choice 1 - first part *)
    val PC1  = Flat_Vector.flat_vector([56, 48, 40, 32, 24, 16,  8,
				    0, 57, 49, 41, 33, 25, 17,
				    9, 1, 58, 50, 42, 34, 26,
				    18, 10, 2, 59, 51, 43, 35,
			      (* Padding 4 bits *)
				    0,0,0,0,
  			      (* Permuted choice 1 - second part *)
                                    62, 54, 46, 38, 30, 22, 14,
				    6, 61, 53, 45, 37, 29, 21,
				    13, 5, 60, 52, 44, 36, 28,
				    20, 12, 4, 27, 19, 11, 3]);
  


			      (* Permuted choice 2 *)
    val PC2 = Flat_Vector.flat_vector([13, 16, 10, 23, 0, 4,
				       2, 27, 14, 5, 20, 9,
				       22, 18, 11, 3, 25, 7,
				       15, 6, 26, 19, 12, 1,
				       44, 55, 34, 40, 50, 58,
				       33, 43, 54, 48, 36, 51,
				       47, 52, 42, 59, 37, 56,
				       49, 45, 53, 39, 32, 35]);
  
			      (* The shifts for each step in the schedule *)
    val Shifts = Flat_Vector.flat_vector([1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1]);
       
 

  
			      (* Initial Permutation *)

    val IP = Flat_Vector.flat_vector([
				      57,49,41,33,25,17,9,1,
				      59,51,43,35,27,19,11,3,
				      61,53,45,37,29,21,13,5,
				      63,55,47,39,31,23,15,7,
				      56,48,40,32,24,16,8,0,
				      58,50,42,34,26,18,10,2,
				      60,52,44,36,28,20,12,4,
				      62,54,46,38,30,22,14,6]);

  
			      (* Inverse Initial Permutation. It considers 
                               * that LR are already inversed *)
    val IIP = Flat_Vector.flat_vector([7,39,15,47,23,55,31,63,
				       6,38,14,46,22,54,30,62,
				       5,37,13,45,21,53,29,61,
				       4,36,12,44,20,52,28,60,
				       3,35,11,43,19,51,27,59,
				       2,34,10,42,18,50,26,58,
				       1,33,9,41,17,49,25,57,
				       0,32,8,40,16,48,24,56]);

			      (* Bit selection table *)
    val E = Flat_Vector.flat_vector([
				     31,0,1,2,3,4,
				     3,4,5,6,7,8,
				     7,8,9,10,11,12,
				     11,12,13,14,15,16,
				     15,16,17,18,19,20,
				     19,20,21,22,23,24,
				     23,24,25,26,27,28,
				     27,28,29,30,31,0]);

			      (* Permutation P *)
    val P = Flat_Vector.flat_vector([15,6,19,20,
				     28,11,27,16,
				     0,14,22,25,
				     4,17,30,9,
				     1,7,23,13,
				     31,26,2,8,
				     18,12,29,5,
				     21,10,3,24]);
      
			      (* The primitive selection functions Si. Begin 
                               * with *)
    val S = Flat_Vector.flat_vector([

				 (* S1 *)
				 Flat_Vector.flat_vector([14,0,4,15,13,7,1,4,
							  2,14,15,2,11,13,8,1,
							  3,10,10,6,6,12,12,11,
							  5,9,9,5,0,3,7,8,

							  4,15,1,12,14,8,8,2,
							  13,4,6,9,2,1,11,7,
							  15,5,12,11,9,3,7,14,
							  3,10,10,0,5,6,0,13]),
			      (* S2 *)
				 Flat_Vector.flat_vector([15,3,1,13,8,4,14,7,
							  6,15,11,2,3,8,4,14,
							  9,12,7,0,2,1,13,10,
							  12,6,0,9,5,11,10,5,

							  0,13,14,8,7,10,11,1,
							  10,3,4,15,13,4,1,2,
							  5,11,8,6,12,7,6,12,
							  9,0,3,5,2,14,15,9]),
				 
			      (* S3 *)
				 Flat_Vector.flat_vector([10,13,0,7,9,0,14,9,
							  6,3,3,4,15,6,5,10,
							  1,2,13,8,12,5,7,14,
							  11,12,4,11,2,15,8,1,

							  13,1,6,10,4,13,9,0,
							  8,6,15,9,3,8,0,7,
							  11,4,1,15,2,14,12,3,
							 5,11,10,5,14,2,7,12]),
			      (* S4 *)
				 Flat_Vector.flat_vector([7,13,13,8,14,11,3,5,
							  0,6,6,15,9,0,10,3,
							  1,4,2,7,8,2,5,12,
							  11,1,12,10,4,14,15,9,

							  10,3,6,15,9,0,0,6,
							  12,10,11,1,7,13,13,8,
							  15,9,1,4,3,5,14,11,
							  5,12,2,7,8,2,4,14]),
				
  
			      (* S5 *)
				 Flat_Vector.flat_vector([2,14,12,11,4,2,1,12,
							  7,4,10,7,11,13,6,1,
							  8,5,5,0,3,15,15,10,
							  13,3,0,9,14,8,9,6,

							  4,11,2,8,1,12,11,7,
							  10,1,13,14,7,2,8,13,
							  15,6,9,15,12,0,5,9,
							  6,10,3,4,0,5,14,3]),
			      (* S6 *)
				 Flat_Vector.flat_vector([12,10,1,15,10,4,15,2,
							  9,7,2,12,6,9,8,5,
							  0,6,13,1,3,13,4,14,
							  14,0,7,11,5,3,11,8,

							  9,4,14,3,15,2,5,12,
							  2,9,8,5,12,15,3,10,
							  7,11,0,14,4,1,10,7,
							  1,6,13,0,11,8,6,13]),
			      (* S7 *)
				 Flat_Vector.flat_vector([4,13,11,0,2,11,14,7,
							  15,4,0,9,8,1,13,10,
							  3,14,12,3,9,5,7,12,
							  5,2,10,15,6,8,1,6,

							  1,6,4,11,11,13,13,8,
							  12,1,3,4,7,10,14,7,
							  10,9,15,5,6,0,8,15,
							  0,14,5,2,9,3,2,12]),
			      (* S8 *)
				 Flat_Vector.flat_vector([13,1,2,15,8,13,4,8,
							  6,10,15,3,11,7,1,4,
							  10,12,9,5,3,6,14,11,
							  5,0,0,14,12,9,7,2,

							  7,2,11,1,4,14,1,7,
							  9,4,12,10,14,8,2,13,
							  0,15,6,12,10,9,13,0,
							  15,3,3,5,5,6,8,11])
				 
				 ])



			      (* Local Functions *)


			      (* Permute the bits in BitArray using the 
                               * permutation specifyied in Perm and put the 
                               * result in FinalPos
			       *)
    fun Permute (BitArray, Perm, FinalPos) =
  
      let

                              (* The size of the final permutation *)
	val last_bit = (Flat_Vector.flat_length Perm) - 1;
  
                              (* The function to loop through all Perm *)
	fun loop_bit (lc, value, mask, nr_bit) =

	  let
	    val bit = Byte1.from_int (Flat_Vector.flat_sub ( Perm, nr_bit))

	    val  new_value  =
	      if is_bit_set ( BitArray, bit) then
		Byte4.|| ( value, mask)
	      else
		value
	    
	  in
	  
	    if nr_bit = last_bit then
	      Locative.locset (lc, new_value)
	    else
	      if nr_bit = 31 then
		(Locative.locset (lc, new_value);
		 loop_bit( Locative.locsub(FinalPos, 1) , 4u0, InitMask,
			  nr_bit+1))
	      else
		loop_bit( lc, new_value, Byte4.>>(mask,1), nr_bit+1)
	  end
    

      in
	loop_bit ( Locative.locsub(FinalPos, 0), 4u0, InitMask, 0)
      end


			      (* The key schedule *)

    fun key_schedule (ba) =
      let
			      (* Verify the key parity*)
	val _ = check_key_parity(ba) 
			      (* Build an empty schedule *)
	val KS = locative( Flat_Vector.flat_vector_const
			  (Flat_Vector.flat_vector_const(4u0, 2), 16));

			      (* Create the temporary place for the CD pair *)
	val CD = locative (Flat_Vector.flat_vector_const( 4u0, 2));
      

	fun loop step =
	  let
	    val shift = Flat_Vector.flat_sub(Shifts, step)
	    val target = Locative.locsub(KS, step)
	  in
	    (
	     PrintBitArray(CD, "CD" ^ makestring(step));
	     LeftShift( Locative.locsub(CD, 0), shift);
	     LeftShift( Locative.locsub(CD, 1), shift);
	     Permute(CD, PC2, target);
	     if step = 15 then
	       ()
	     else
	       loop (step + 1)
	    )
	  end

    
      in

	PrintBitArray(ba, "DES.key_schedule - input key");
	Permute(ba, PC1, CD);
	loop 0;
	PrintSchedule(KS);
	KS

      end
  

  



			      (* Intermediate function F *)
    fun F (R, K) =
      let
			      (* A temporary value to store 48 bits *)
	val Temp  = locative ( Flat_Vector.flat_vector_const (4u0, 2))
	val Temp1 = locative ( Flat_Vector.flat_vector_const (4u0, 2))
      
      
	fun next_group ( invalue , shiftget, shiftput, newvalue, group) =
	  let
	
	    val to_put = Flat_Vector.flat_sub(Flat_Vector.flat_sub(S, group),
					      Byte4.to_int
					      (Byte4.&&(Byte4.>>(invalue,
								 shiftget),
							4ux3F)))
	    val compvalue = Byte4.|| (newvalue,
				      Byte4.<< ( Byte4.from_int(to_put),
						shiftput))
	    val newinvalue =
	      if shiftget = 2 then
		let
		  val tmp = Byte4.>>( lget(Temp, 1), 2)
		in
		  Byte4.||(tmp, Byte4.<<(invalue, 30))
		end
	      else
		invalue
	    
	  in
	    if shiftput = 0 then
	      lset(Temp, 0, compvalue)
	    else
	      if shiftget = 2 then
		next_group( newinvalue, 32 - 6, shiftput - 4, compvalue,
			   group + 1)
	      else
		next_group( invalue, shiftget - 6, shiftput - 4, compvalue,
			   group + 1)
	  end
	    
      in

	lset(Temp1, 0, R);
	Permute (Temp1, E, Temp);
	PrintBitArray(Temp, "After E");
	Xor (Locative.locsub(Temp, 0), Locative.locsub(K, 0));
	Xor (Locative.locsub(Temp, 1), Locative.locsub(K, 1));
	PrintBitArray(Temp, "After Xor");
	next_group( lget(Temp, 0), 32 - 6, 32 - 4, 4u0, 0);
	
	PrintBitArray(Temp, "After S");
	Permute( Temp, P, Temp1);
	PrintBitArray(Temp1, "After P");
	lget(Temp1, 0)
      end
      
			      (* Exported Functions *)
  
  

    fun ecb_encrypt_intern (Input_string, schedule, Output_string, is_encrypt)=

      let
			      (* Cast the input to local type *)
	val Input = ubyte_to_cblock(Input_string)
	  

	val Cipher = locative ( Flat_Vector.flat_vector([4u0,4u0]))
	  
	  
	  
	val LR = locative ( Flat_Vector.flat_vector([4u0, 4u0]))

      
	fun iterate 16 = ()
	  | iterate n  = let
			   val lk = if is_encrypt then
			              Locative.locsub(schedule, n)
				    else
				      Locative.locsub(schedule, 15-n)
			   val ll = Locative.locsub(LR, 0)
			   val lr = Locative.locsub(LR, 1)
			   val memr = Locative.locget(lr)

			      (* Print the LR before computing F *)
			   val _ = PrintBitArray(LR,
						 "LR Before step " ^
						 Integer.makestring (n+1))
		    
			   val temprez = F(memr, lk)
     
			 in
			   (
			    
			    Locative.locset(lr,Byte4.xor(temprez,
							 Locative.locget(ll)));
			    Locative.locset(ll, memr);
			    iterate (n+1)
			    )
			 end
      in
	PrintBitArray(Input, "Before ecb_encrypt\n\tInput");
	Permute(Input, IP, LR);


	iterate(0);
	
	PrintBitArray(LR, "Before Inverse permutation");

			      (* Inverse permutation *)
	Permute(LR, IIP, Cipher);

	PrintBitArray(Cipher, "Result");

	cblock_to_ubyte_intern( Cipher, Output_string)
	
      end


    fun cbc_cksum (inp, ilen, sched, chain, cipher) =
      let
	  
	val clear = locative (Flat_Vector.flat_vector([1u0,1u0,1u0,1u0,
						       1u0,1u0,1u0,1u0]))

	fun iterate pos =
	  if pos >= ilen then
	    ()
	  else
	    let
	      val _ = copy_bytes (inp, ilen, pos, clear, 0, 8)
		
	      val _ = PrintFlatVector(clear, 8,"Iteration " ^ makestring(pos) ^
				      " Clear");
	      val _ = xor_bytes(clear, cipher, 8)
		
	      val _ = PrintFlatVector(clear, 8,"Iteration " ^ makestring(pos) ^
				      " Xored");
              val _ = ecb_encrypt_intern(clear, sched, cipher, true)
		
	      val _ = PrintFlatVector(cipher,8,"Iteration " ^ makestring(pos) ^
				    " Cipher");
	    in
	      iterate (pos+8)
	    end
      in

	cblock_to_ubyte_intern(chain, cipher);
	PrintFlatVector(inp, ilen, "Before cbc_cksum\n\tInput: ");
	iterate 0;
	PrintFlatVector(cipher, 8,"After cbc_cksum\n\tOutput: ")

      end
    
    fun string_to_flat str =
      let
	fun list_of_ubyte nil = nil
	  | list_of_ubyte (a::l) = Byte1.from_int(ord(a)) ::
	    (list_of_ubyte l)
      in
	Flat_Vector.flat_vector(list_of_ubyte(explode str))
      end
    
    fun string_to_key str =
      let
	val key = locative( Flat_Vector.flat_vector_const(1u0, 8))
	  
	fun reverse_byte (orig, 8) = 1u0
	  | reverse_byte (orig, bit) =
	    Byte1.||(reverse_byte(orig, bit+1),
		     if Byte1.&&(orig, Byte1.<<(1u1,bit)) = 1u0 then
		       1u0
		     else
		       Byte1.<<(1u1, 7-bit))
	    
	fun fill_init_key (posstr, dir, poskey) =
	  let
	    val lockey = Locative.locsub(key, poskey)
	      
	    val ival =
	      if posstr < size(str) then
		Byte1.from_int(ord(substring(str,posstr,1)))
	      else
		1u0
		
	    val ival =
	      if dir = 0 then
		Byte1.<<(ival,1)
	      else
		reverse_byte(ival,0)
		
	    val newposstr =
	      if posstr < size(str) then
		posstr + 1
	      else
		posstr
	    val (newdir, newposkey, dirchange) =
	      if dir = 0 then
		if poskey = 7 then
		  (1, 7, true)
		else
		  (0, poskey+1, false)
	      else
		if poskey = 0 then
		  (0, 0, true)
		else
		  (1, poskey-1, false)
		  
		  
	  in
	    if posstr >= size(str) andalso dirchange then
	      ()
	    else
	      (
	       Locative.locset(lockey, Byte1.xor(Locative.locget(lockey),
						 ival));
	       fill_init_key(newposstr, newdir, newposkey)
	       )
	  end

	val _ = if debug then
	          print ("String_to_key:\n\tInput: " ^ str ^ "\n")
		else
		  ()
		  
	val _ = fill_init_key(0,0,0)
	val Cipher = locative(Flat_Vector.flat_vector([1u0,1u0,1u0,1u0,
						       1u0,1u0,1u0,1u0]))
	val _ = fix_key_parity(key)
	val Key = ubyte_to_cblock(key)
        val  Schedule = key_schedule(Key)
	val _ = cbc_cksum(locative(string_to_flat(str)), size(str),
			  Schedule, Key, Cipher)
	val _ = fix_key_parity(Cipher)
	  
      in
	(
	 PrintFlatVector(Cipher, 8,"After string_to_key\n\tOutput: ");
	 ubyte_to_cblock(Cipher)
	)
      end       


    fun pcbc_encrypt_intern (input, ilen, schedule, chain, output, is_encrypt)=

      let
	val t_xor = locative (Flat_Vector.flat_vector([1u0,1u0,1u0,1u0,
						       1u0,1u0,1u0,1u0]))
	val t_input = locative (Flat_Vector.flat_vector([1u0,1u0,1u0,1u0,
							 1u0,1u0,1u0,1u0]))
	val t_output = locative (Flat_Vector.flat_vector([1u0,1u0,1u0,1u0,
							 1u0,1u0,1u0,1u0]))

	val _ = cblock_to_ubyte_intern(chain, t_xor)
			      
	fun iterate_encrypt ipos =
	  if ipos >= ilen then
	    ()
	  else
	    (
	     copy_bytes(input, ilen, ipos, t_input, 0, 8);
	     PrintFlatVector(t_input, 8, "IPos = " ^ makestring(ipos) ^
			                  " clear");
	     xor_bytes(t_input, t_xor, 8);
	     PrintFlatVector(t_input, 8, "IPos = " ^ makestring(ipos) ^
			                  " xored");
	     ecb_encrypt_intern(t_input, schedule, t_output, true);
	     copy_bytes(t_output, 8, 0, output, ipos, 8);
	     copy_bytes(input, ilen, ipos, t_xor, 0, 8);
	     PrintFlatVector(t_output, 8, "IPos = " ^ makestring(ipos) ^
			                  " cipher");
	     xor_bytes(t_xor, t_output, 8);
	     iterate_encrypt (ipos+8)
	     )

	fun iterate_decrypt ipos =
	  if ipos >= ilen then
	    ()
	  else
	    (
	     copy_bytes(input, ilen, ipos, t_input, 0, 8);
	     PrintFlatVector(t_input, 8, "IPos = " ^ makestring(ipos) ^
			                  " cipher");
	     ecb_encrypt_intern(t_input, schedule, t_output, false);
	     xor_bytes(t_output, t_xor, 8);
	     copy_bytes(t_output, 8, 0, output, ipos, 8);
	     PrintFlatVector(t_output, 8, "IPos = " ^ makestring(ipos) ^
			                  " clear");
	     copy_bytes(t_output, 8, 0, t_xor, 0, 8);
	     xor_bytes(t_xor, t_input, 8);
	     iterate_decrypt (ipos+8)
	     )

      in
	if is_encrypt then
	  iterate_encrypt 0
	else
	  iterate_decrypt 0
      end
    
    
    fun quad_cksum_intern (input, len, seed, output, outlen) =

      let


	fun add  (lw1, lw2) =
	  let
	    val h1 = Byte4.&&(lw1, 4ux80000000)
	    val l1 = Byte4.&&(lw1, 4ux7FFFFFFF)
	      
	    val h2 = Byte4.&&(lw2, 4ux80000000)
	    val l2 = Byte4.&&(lw2, 4ux7FFFFFFF)
	  in
	    Byte4.xor(Byte4.xor(h1,h2), l1+l2)
	  end
     
	fun mult  (lw1, lw2) =
	  let
	    val x0 = Byte4.&&(4uxFF, Byte4.>>(lw1, 0))
	    val x1 = Byte4.&&(4uxFF, Byte4.>>(lw1, 8))
	    val x2 = Byte4.&&(4uxFF, Byte4.>>(lw1,16))
	    val x3 = Byte4.&&(4uxFF, Byte4.>>(lw1,24))

	    val y0 = Byte4.&&(4uxFF, Byte4.>>(lw2, 0))
	    val y1 = Byte4.&&(4uxFF, Byte4.>>(lw2, 8))
	    val y2 = Byte4.&&(4uxFF, Byte4.>>(lw2,16))
	    val y3 = Byte4.&&(4uxFF, Byte4.>>(lw2,24))

	    val m0 = x0*y0
	    val m1 = Byte4.<<( x0*y1 + x1*y0, 8)
	    val m2 = Byte4.<<( Byte4.&&( 4uxFFFF, x0*y2 + x1*y1 + x2*y0), 16)
	    val m3 = Byte4.<<( Byte4.&&( 4uxFF, x0*y3 + x1*y2 + x2*y1 +
					x3*y0), 24)
     
	  in
     
	    add(m3,add(m2, add(m1,m0)))
	  end

	fun modulo (x:ubyte4, y) = if x < y then x else modulo(x-y, y)

	val temp = locative(Flat_Vector.flat_vector_const(1u0,8))
	val _ = cblock_to_ubyte_intern(seed, temp)
	val seedloc = Locative.locview(temp, desc_vector(2,desc_byte4))
	  
	val z0 = lget(seedloc,0)
	val z1 = lget(seedloc,1)

        fun fill_output (outpos, value, byte) =
	  if outpos >= outlen orelse byte = 4 then
	    ()   
	  else     
	    (
	     lset(output, outpos,Byte4to1(Byte4.&&(4uxFF, value)));
	     fill_output (outpos+1, Byte4.>>(value, 8), byte+1)
	     )

        fun iter_input (outpos, i, z) =
	  let

	    val x =
	      if i <= len - 2 then
		add(#1(z),
		    add(Byte1to4(lget(input,i)),
			mult(Byte1to4(lget(input,i+1)),4ux100)))
	      else
		if i < len then
		  add(#1(z),Byte1to4(lget(input,i)))
		else
		  4u0

	    val _ = if debug then
	      let
		val _ = print ("Outpos = " ^ makestring(outpos) ^ " Inpos =" ^
			       makestring(i) ^ " Z0 =")
		val _ = PrintByte4(#1(z))
		val _ = print "\n\t\tZ1 = "
		val _ = PrintByte4(#2(z))
	      in
		print "\n X = ";
     		PrintByte4(x);
		print "\n"
	      end
		    else
		      ()
	  in
	    
	    if i >= len then
              (
	       fill_output(outpos, #1(z), 0);
	       fill_output(outpos+4, #2(z), 0);
	       z
	       )
	    else
	      let
		val z' = (modulo(add(mult(x,x),
				     mult(#2(z),#2(z))), 4ux7fffffff),
			  modulo(mult(x, add(#2(z),4u83653421)), 4ux7fffffff))
	      in
		iter_input (outpos, i+2, z')
	      end
	  end

	fun iterate (outpos, z) =
	  if outpos < outlen then
	    iterate(outpos+8, iter_input(outpos, 0, z))
	  else
	    ()
      in
	iterate(0, (z0,z1))
      end


			      (* Here are the functions to be exported *)
    fun cblock_to_ubyte cb =
      let
	val rez = locative (Flat_Vector.flat_vector_const(1u0, 8))
	val _ = cblock_to_ubyte_intern(cb, rez)
      in
	rez
      end

    fun ecb_encrypt (Clear, KS, is_encrypt) =
      let
	val Cipher = locative (Flat_Vector.flat_vector_const(1u0, 8))
	val _ = ecb_encrypt_intern(Clear, KS, Cipher, is_encrypt)
      in
	Cipher
      end

    fun pcbc_encrypt (Clear, KS, Chain, is_encrypt) =
      let
	val ilen = Flat_Vector.flat_length( Locative.locget(Clear))
	val olen = ((ilen + 7) div 8) * 8
	val Cipher = locative (Flat_Vector.flat_vector_const(1u0, olen))
	val _ = pcbc_encrypt_intern(Clear, ilen, KS, Chain, Cipher, is_encrypt)
      in
	Cipher
      end

    fun quad_cksum (Clear, Seed, outlen) =
      let
	val ilen = Flat_Vector.flat_length( Locative.locget(Clear))
	val olen = if outlen < 4 then 4 else outlen
	val Cksum = locative (Flat_Vector.flat_vector_const(1u0, olen))
	val _ = quad_cksum_intern(Clear, ilen, Seed, Cksum, olen)
	val FirstWord = Byte4.||(Byte1to4(lget(Cksum, 0)),
		  Byte4.||(Byte4.<<(Byte1to4(lget(Cksum, 1)), 8),
			   Byte4.||(Byte4.<<(Byte1to4(lget(Cksum, 2)), 16),
				    Byte4.<<(Byte1to4(lget(Cksum, 3)), 24))))

      in
	PrintFlatVector(Cksum, 8, "Computed QUAD_CKSUM");
	(FirstWord, Cksum)
      end
      
    fun ubyte1_tuple_to_cblock (one,two,three,four,five,six,seven,eight)=
      let val array=B.Create.create 8
	val _=Byte1.update(array,0,one)
	val _=Byte1.update(array,1,two)
	val _=Byte1.update(array,2,three)
	val _=Byte1.update(array,3,four)
	val _=Byte1.update(array,4,five)
	val _=Byte1.update(array,5,six)
	val _=Byte1.update(array,6,seven)
	val _=Byte1.update(array,7,eight)
      in
	ubyte_to_cblock (Locative.baview array)
      end

    fun cblock_to_ubyte1_tuple c=
      let val array=cblock_to_ubyte c
      in
	(Locative.locget (Locative.locsub(array,0)),
	 Locative.locget (Locative.locsub(array,1)),
	 Locative.locget (Locative.locsub(array,2)),
	 Locative.locget (Locative.locsub(array,3)),
	 Locative.locget (Locative.locsub(array,4)),
	 Locative.locget (Locative.locsub(array,5)),
	 Locative.locget (Locative.locsub(array,6)),
	 Locative.locget (Locative.locsub(array,7)))
      end
  end
