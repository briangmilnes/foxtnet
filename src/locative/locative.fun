(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	A locative functor using EXTERN to allow read/write access to
	pieces of packets after allocation.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Locative

		iii.	RCS Log
	
$Log: locative.fun,v $
Revision 1.3  1994/11/09  20:58:19  esb
adapted to new extern.sig.

Revision 1.2  1994/09/30  16:52:53  esb
renamed the functor Locative instead of locative.

Revision 1.1  1994/08/12  14:26:45  milnes
Initial revision

		1.	functor Locative
*)

functor Locative (structure Dyn_Array: DYNAMIC_BYTE_ARRAY
		  structure Extern: EXTERN
		  sharing type Extern.extern = Dyn_Array.T
		      and type Extern.cursor = Dyn_Array.T): LOCATIVE =
 struct
  type location = Dyn_Array.T
  type loc = Dyn_Array.T
  type object = Extern.T

  fun ::== (location, object) =
       (Extern.marshal (location, object) location;
	())

  fun loc location = location

  fun !! location =
       (fn (object, _) => object) (Extern.unmarshal (location, location))

 end
