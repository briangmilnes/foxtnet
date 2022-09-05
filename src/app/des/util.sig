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
	1.	signature DES_UTIL

		iii.	RCS Log
	
$Log: util.sig,v $
Revision 1.3  1996/04/18  21:37:26  cline
pass seed to pcbc_encrypt and pcbc_decrypt

Revision 1.2  1996/03/22  22:41:41  necula
Completely rewritten implementation. Changed from locatives and Flat_Vector to
Word8Array and Word64 (FOXWORD).

Revision 1.1  1994/01/13  18:47:08  necula
Initial revision

		1.	signature DES_UTIL
*)

			      (* Utilities build with core DES encryption *)
signature DES_UTIL =
  sig
			      (* It needs the extended view of DES *)
      structure Des    : DES
      structure Block  : BLOCK
			      (* Chain Propagation encryption algorithm. The 
                               * first argument is a key * initial block. A
			       * large amount of computation can be saved if
			       * the function is partially applied to the
			       * key first. *)
      val pcbc_encrypt : Block.T * Block.T -> Word8Array.array -> Word8Array.array
      val pcbc_decrypt : Block.T * Block.T -> Word8Array.array -> Word8Array.array
			      (* Produces a DES key by scrambling a string 
                               * (password)  *)
      val password_to_key : string -> Block.T

  end;

