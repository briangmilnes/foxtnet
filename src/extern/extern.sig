(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
        Brian Milnes (Brian.Milnes@cs.cmu.edu)
        Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

                i.      Abstract

	This was created by Peter Lee and Edo Biagioni

                ii.     Table of Contents

        i.      Abstract
        ii.     Table of Contents
        iii.    RCS Log
        1.      

                iii.    RCS Log
        
$Log: extern.sig,v $
Revision 1.5  1996/01/19  23:10:20  esb
adapted to the new wordarray signature.

Revision 1.4  1995/06/27  18:59:07  cline
replaced type extern with extern_in (for unmarshal) and extern_out
(for marshal).

Revision 1.3  1994/11/09  20:48:52  esb
major revision.

Revision 1.2  1994/08/08  10:50:33  robby

Revision 1.1  94/07/14  20:25:25  robby
Initial revision

Revision 1.1  1994/07/07  17:00:38  robby
Initial revision


                1.      signature EXTERN

	A structure with signature EXTERN provides operations to
	marshall and unmarshall values of one specific type.  Marshall
	means that the value is encoded into a mutable value of type
	extern; unmarshall that the value is taken from the value of
	type extern.  It is assumed that an extern may store multiple
	values of different types, and that a cursor is used to keep
	track of the final position of the last marshalling or
	unmarshalling.

	Marshall may fail if insufficient space is available;
	unmarshalling, likewise, and also if the value in the extern
	is not a valid encoding for a value of type T.  In all of
	these cases, the exception Extern is raised.
*) 

signature EXTERN =
 sig
  type T
  type extern_in
  type extern_out
  type cursor

  exception Extern

  (* Size returns the size required to marshall the given value. *)
  val size: T -> Word.word
  val marshal: extern_out * T -> cursor -> cursor
  val unmarshal: extern_in * cursor -> T * cursor
 end (* sig *)
