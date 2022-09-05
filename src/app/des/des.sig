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
	1.	signature DES
	
		iii.	RCS Log
	
$Log: des.sig,v $
Revision 1.5  1996/03/22  22:58:43  necula
Fixed a bug.

# Revision 1.1  1996/03/22  22:40:34  necula
# Initial revision
#
Revision 1.3  1994/01/13  18:53:14  necula
A brand new file

		1.	signature DES
*)
      
			 (* The external view of the DES core functions.  *)
signature DES =
  sig

      structure Block : BLOCK
			 (* For the encryption and decryption functions a 
                          * large amount of computation can be saved when the 
                          * same key will be used many times by applying the 
			 * function to the first argument only once  *)

			 (* Arguments are key, data *)
      val encrypt      : Block.T -> Block.T -> Block.T
      val decrypt      : Block.T -> Block.T -> Block.T
      
  end;
