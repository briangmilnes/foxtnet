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
	1.	functor DesFun

		iii.	RCS Log
	
$Log: des.sml,v $
Revision 1.3  1996/03/22  22:41:41  necula
Completely rewritten implementation. Changed from locatives and Flat_Vector to
Word8Array and Word64 (FOXWORD).

Revision 1.2  1994/01/17  08:26:45  necula
*** empty log message ***

Revision 1.1  94/01/13  18:46:34  necula
Initial revision

		1.	functor DesFun
*)


			      (* Implements the DES core encryption/decryption 
                               * functions. For details see "Federal 
                               * Information Processing Standards Publication 
                               * 46", JAnuary 15, 1977. *)
functor DesFun (structure Aux : DES_AUXILIARY
		val debug : bool) : DES =
  struct

      structure Block = Aux.Block
			      (* Key schedule computation *)
			      (* Function to compute the key schedule. The 
                               * output of this function will be used in all 
                               * following operations with the same key. It 
                               * is useful to compute it only once for a 
                               * given key. Also, the schedule is in reverse 
                               * order (for decryption) *)
      fun key_schedule (key : Block.T) =
	  let
	      val (_, sched) =
		  foldl (fn (sh : Word31.word,
			     (block : Block.T, sched : Block.T list)) =>
			 let
			     val _ = if debug then
				 print "Schedule Iteration\n" else ()
				 
			     val newblock = Block.shift28(block, sh);
			 in
			     (newblock, Aux.PC2(newblock) :: sched)
			 end)
		  (Aux.PC1(key), [])
	    [0w1,0w1,0w2,0w2,0w2,0w2,0w2,0w2,0w1,0w2,0w2,0w2,0w2,0w2,0w2,0w1];
	  in
	      sched
	  end


      fun des_ecb_block (key, encrypt) =
	  let
			      (* Compute the schedule. The schedule is 
                               * reversed if we are encrypting because 
                               * key_schedule computes it already reversed. *)
	      val Schedule =
		  if encrypt then rev(key_schedule(key)) else key_schedule(key)
			      (* This is the actual encryption function *)
	      fun ecb_encrypt_block text =
		  let
		      val res = 
			Aux.IIP(Block.combine
			   (foldl (fn (K : Block.T, (l : Block.T, r : Block.T))
					  => (r, Block.xor(l, Aux.F(r, K))))
				   (Block.split (Aux.IP(text)))
				   Schedule))
	      in
		  if debug then
		      (print "DES in :\t";
		       Block.print_block text;
		       print "DES key:\t";
		       Block.print_block key;
		       print "DES out:\t";
		       Block.print_block res;
		       res)
		  else
		      res
	      end
      in
	ecb_encrypt_block
      end

			    (* Produces the exported functions *)
    fun encrypt  key = des_ecb_block (key, true)
    fun decrypt  key = des_ecb_block (key, false)
  end;
    

      
