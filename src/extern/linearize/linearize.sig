(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
        Brian Milnes (Brian.Milnes@cs.cmu.edu)
        Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

                i.      Abstract

	This is the LINEARIZE signature, a specialization of
	EXTERN which marshalls and unmarshalls to and from bytearrays.

                ii.     Table of Contents

        i.      Abstract
        ii.     Table of Contents
        iii.    RCS Log
        1.      

                iii.    RCS Log
        
$Log: linearize.sig,v $
Revision 1.5  1996/01/19  23:09:46  esb
adapted to the new wordarray signature.

Revision 1.4  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.3  1995/06/27  18:58:03  cline
adapted to new extern.sig

Revision 1.2  1994/11/09  20:49:09  esb
adapted to new extern.sig.

Revision 1.1  1994/07/14  20:28:42  robby
Initial revision

Revision 1.1  94/07/13  18:49:03  robby
Initial revision


                1.      signature LINEARIZE
*)

signature LINEARIZE =
 sig

  include EXTERN
     sharing type extern_in = Word8Array.array
         and type extern_out = Word8Array.array
         and type cursor = Word.word

 end (* sig *)

