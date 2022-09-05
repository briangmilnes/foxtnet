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
	1.	signature DES_AUXILIARY

		iii.	RCS Log
	
$Log: aux.sig,v $
Revision 1.2  1996/03/22  22:41:41  necula
Completely rewritten implementation. Changed from locatives and Flat_Vector to
Word8Array and Word64 (FOXWORD).

Revision 1.1  1994/01/13  18:46:09  necula
Initial revision

		1.	signature DES_AUXILIARY
*)

			 (* The signature for the implementation of auxiliary 
                          * functions for DES encryption. The description of 
                          * these functions can be found in the DES 
                          * specifications  *)
signature DES_AUXILIARY =
  sig

    structure Block : BLOCK
			 (* Initial Permutation *)
    val IP  : Block.T -> Block.T
			 (* Initial Inverse Permutation *)
    val IIP : Block.T -> Block.T
			 (* Permuted Choice 1 *)
    val PC1 : Block.T -> Block.T
			 (* Permuted Choice 2 *)
    val PC2 : Block.T -> Block.T
			 (* Function F (R, K) *)
    val F   : Block.T * Block.T -> Block.T

  end;

