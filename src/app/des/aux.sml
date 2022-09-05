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
Auxiliary Functions implementation		

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor DesAuxFun

		iii.	RCS Log
	
$Log: aux.sml,v $
Revision 1.2  1996/03/22  22:41:41  necula
Completely rewritten implementation. Changed from locatives and Flat_Vector to
Word8Array and Word64 (FOXWORD).

Revision 1.1  1994/01/13  18:45:28  necula
Initial revision

		1.	functor DesAuxFun
*)

			 (* The implementation of some auxiliary functions 
                          * used by DES. The functions are implemented using 
                          * a general permutation function. All the tables 
                          * specified in DES standard are in this functor.  *)
functor DesAuxFun (structure Block : BLOCK
		   val debug : bool) : DES_AUXILIARY =
  struct

    structure Block = Block

			      (* Here there are the auxiliary functions coded 
                               * as tables for Block.permute function *)
			     
      
			      (* Permuted choice 1 - first part *)
    val PC1_tab : Word31.word Vector.vector = Vector.fromList(
				[0w56, 0w48, 0w40, 0w32, 0w24, 0w16,  0w8,
				 0w0, 0w57, 0w49, 0w41, 0w33, 0w25, 0w17,
				 0w9, 0w1, 0w58, 0w50, 0w42, 0w34, 0w26,
				 0w18, 0w10, 0w2, 0w59, 0w51, 0w43, 0w35,
			      (* Padding 4 bits *)
				 0w0,0w0,0w0,0w0,
  			      (* Permuted choice 1 - second part *)
				0w62, 0w54, 0w46, 0w38, 0w30, 0w22, 0w14,
				0w6, 0w61, 0w53, 0w45, 0w37, 0w29, 0w21,
				0w13,0w5, 0w60, 0w52, 0w44, 0w36, 0w28,
				0w20, 0w12, 0w4, 0w27, 0w19, 0w11, 0w3]);
  


			      (* Permuted choice 2 *)
    val PC2_tab : Word31.word Vector.vector = Vector.fromList(
			       [0w13, 0w16, 0w10, 0w23, 0w0,0w4,
				0w2, 0w27, 0w14, 0w5, 0w20, 0w9,
				0w22, 0w18, 0w11, 0w3, 0w25, 0w7,
				0w15, 0w6, 0w26, 0w19, 0w12, 0w1,
				0w44, 0w55, 0w34, 0w40, 0w50, 0w58,
				0w33, 0w43, 0w54, 0w48, 0w36, 0w51,
				0w47, 0w52, 0w42, 0w59, 0w37, 0w56,
				0w49, 0w45, 0w53, 0w39, 0w32, 0w35]);
  
  
			      (* Initial Permutation *)

    val IP_tab : Word31.word Vector.vector = Vector.fromList(
				[0w57,0w49,0w41,0w33,0w25,0w17,0w9,0w1,
				 0w59,0w51,0w43,0w35,0w27,0w19,0w11,0w3,
				 0w61,0w53,0w45,0w37,0w29,0w21,0w13,0w5,
				 0w63,0w55,0w47,0w39,0w31,0w23,0w15,0w7,
				 0w56,0w48,0w40,0w32,0w24,0w16,0w8,0w0,
				 0w58,0w50,0w42,0w34,0w26,0w18,0w10,0w2,
				 0w60,0w52,0w44,0w36,0w28,0w20,0w12,0w4,
				 0w62,0w54,0w46,0w38,0w30,0w22,0w14,0w6]);

  
			      (*Inverse Initial Permutation. It is different 
                               * from the one in the standard because it 
                               * considers the LR are swapped *)

    val IIP_tab : Word31.word Vector.vector = Vector.fromList(
				[0w7,0w39,0w15,0w47,0w23,0w55,0w31,0w63,
				 0w6,0w38,0w14,0w46,0w22,0w54,0w30,0w62,
				 0w5,0w37,0w13,0w45,0w21,0w53,0w29,0w61,
				 0w4,0w36,0w12,0w44,0w20,0w52,0w28,0w60,
				 0w3,0w35,0w11,0w43,0w19,0w51,0w27,0w59,
				 0w2,0w34,0w10,0w42,0w18,0w50,0w26,0w58,
				 0w1,0w33,0w9,0w41,0w17,0w49,0w25,0w57,
				 0w0,0w32,0w8,0w40,0w16,0w48,0w24,0w56]);

			      (* Bit selection table  *)
    val E_tab : Word31.word Vector.vector = Vector.fromList(
				[0w31,0w0,0w1,0w2,0w3,0w4,
				 0w3,0w4,0w5,0w6,0w7,0w8,
				 0w7,0w8,0w9,0w10,0w11,0w12,
				 0w11,0w12,0w13,0w14,0w15,0w16,
				 0w15,0w16,0w17,0w18,0w19,0w20,
				 0w19,0w20,0w21,0w22,0w23,0w24,
				 0w23,0w24,0w25,0w26,0w27,0w28,
				 0w27,0w28,0w29,0w30,0w31,0w0]);

			      (* Permutation P *)
    val P_tab : Word31.word Vector.vector = Vector.fromList(
			       [0w15,0w6,0w19,0w20,
				0w28,0w11,0w27,0w16,
				0w0,0w14,0w22,0w25,
				0w4,0w17,0w30,0w9,
				0w1,0w7,0w23,0w13,
				0w31,0w26,0w2,0w8,
				0w18,0w12,0w29,0w5,
				0w21,0w10,0w3,0w24]);
      
			      (* The primitive selection functions Si. *)
	
    val S_tab : int Vector.vector Vector.vector = Vector.fromList
	[

				 (* S1 *) 
	Vector.fromList([14,0,4,15,13,7,1,4,
			 2,14,15,2,11,13,8,1,
			 3,10,10,6,6,12,12,11,
			 5,9,9,5,0,3,7,8,

			 4,15,1,12,14,8,8,2,
			 13,4,6,9,2,1,11,7,
			 15,5,12,11,9,3,7,14,
			 3,10,10,0,5,6,0,13]),
	                         (* S2 *) 
	Vector.fromList([15,3,1,13,8,4,14,7,
			 6,15,11,2,3,8,4,14,
			 9,12,7,0,2,1,13,10,
			 12,6,0,9,5,11,10,5,
			 
			 0,13,14,8,7,10,11,1,
			 10,3,4,15,13,4,1,2,
			 5,11,8,6,12,7,6,12,
			 9,0,3,5,2,14,15,9]),
				 
			      (* S3 *)
	Vector.fromList([10,13,0,7,9,0,14,9,
			 6,3,3,4,15,6,5,10,
			 1,2,13,8,12,5,7,14,
			 11,12,4,11,2,15,8,1,
			 
			 13,1,6,10,4,13,9,0,
			 8,6,15,9,3,8,0,7,
			 11,4,1,15,2,14,12,3,
			 5,11,10,5,14,2,7,12]),
			      (* S4 *)
	Vector.fromList([7,13,13,8,14,11,3,5,
			 0,6,6,15,9,0,10,3,
			 1,4,2,7,8,2,5,12,
			 11,1,12,10,4,14,15,9,
			 
			 10,3,6,15,9,0,0,6,
			 12,10,11,1,7,13,13,8,
			 15,9,1,4,3,5,14,11,
			 5,12,2,7,8,2,4,14]),
				
  
			      (* S5 *)
	Vector.fromList([2,14,12,11,4,2,1,12,
			 7,4,10,7,11,13,6,1,
			 8,5,5,0,3,15,15,10,
			 13,3,0,9,14,8,9,6,
			 
			 4,11,2,8,1,12,11,7,
			 10,1,13,14,7,2,8,13,
			 15,6,9,15,12,0,5,9,
			 6,10,3,4,0,5,14,3]),
			      (* S6 *)
	Vector.fromList([12,10,1,15,10,4,15,2,
			 9,7,2,12,6,9,8,5,
			 0,6,13,1,3,13,4,14,
			 14,0,7,11,5,3,11,8,
			 
			 9,4,14,3,15,2,5,12,
			 2,9,8,5,12,15,3,10,
			 7,11,0,14,4,1,10,7,
			 1,6,13,0,11,8,6,13]),
			      (* S7 *)
	Vector.fromList([4,13,11,0,2,11,14,7,
			 15,4,0,9,8,1,13,10,
			 3,14,12,3,9,5,7,12,
			 5,2,10,15,6,8,1,6,
			 
			 1,6,4,11,11,13,13,8,
			 12,1,3,4,7,10,14,7,
			 10,9,15,5,6,0,8,15,
			 0,14,5,2,9,3,2,12]),
			      (* S8 *)
	Vector.fromList([13,1,2,15,8,13,4,8,
			 6,10,15,3,11,7,1,4,
			 10,12,9,5,3,6,14,11,
			 5,0,0,14,12,9,7,2,
			 
			 7,2,11,1,4,14,1,7,
			 9,4,12,10,14,8,2,13,
			 0,15,6,12,10,9,13,0,
			 15,3,3,5,5,6,8,11])
				 
	]

			      (* Builds the functions *)
    val IP = Block.permute ("IP", IP_tab);
    val IIP = Block.permute ("IIP", IIP_tab);
    val PC1 = Block.permute ("PC1", PC1_tab);
    val PC2 = Block.permute ("PC2", PC2_tab);
    val E = Block.permute ("E", E_tab);
    val P = Block.permute ("P", P_tab);

			      (* The function S is implemented as a folding 
                               * over each group of 6 bits in the input and 
                               * produces 4 bits at every iteration. The input 
                               * has 48 valid bits so it will produce a 32 bit 
			       * output *)
    fun S cblock =
      let
	fun loop (0w8 : Word31.word, partres) = partres
	  | loop (grp, partres) =
	    let
		val table = Vector.sub(S_tab, Word31.toInt grp)
		val sixbit = Block.extractbits(cblock, 0w6 * grp, 0w6)
		val fourbit = Vector.sub(table, sixbit)
	    in
		loop(grp + 0w1,
		     Block.setbits(partres, 0w4 * grp, 0w4, fourbit))
	    end
	val res = loop (0w0, Block.zero)
      in
	  if debug then
	      (print "After S:\t";
	       Block.print_block res;
	       res)
	  else
	      res
      end

			      (* Auxiliary function F *)
    fun F (r, k) = P (S (Block.xor (E r, k)))
      
  end;


