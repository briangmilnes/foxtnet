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

	A functor to generate locatives for byte1,2,4.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Dyn_Locs 

		iii.	RCS Log
	
$Log: dynlocs.fun,v $
Revision 1.3  1994/11/09  20:53:17  esb
adapted to new extern.sig.

Revision 1.2  1994/09/30  16:52:32  esb
replaced ubytes with foxwords.

Revision 1.1  1994/08/12  14:26:45  milnes
Initial revision


		1.	functor Dyn_Locs
*)

functor Dyn_Locs (structure Dyn_Array: DYNAMIC_BYTE_ARRAY
                  structure Byte_Extern: BYTE_EXTERN
                   sharing type Dyn_Array.T = Byte_Extern.Byte1.extern
		                = Byte_Extern.Byte1.cursor
                       and type Dyn_Array.T = Byte_Extern.Byte2.extern
		                = Byte_Extern.Byte2.cursor
                       and type Dyn_Array.T = Byte_Extern.Byte4.extern
		                = Byte_Extern.Byte4.cursor): DYN_LOCS = 
 struct
  structure Byte1 = Locative (structure Dyn_Array = Dyn_Array
			      structure Extern = Byte_Extern.Byte1)
  structure Byte2 = Locative (structure Dyn_Array = Dyn_Array
			      structure Extern = Byte_Extern.Byte2)
  structure Byte4 = Locative (structure Dyn_Array = Dyn_Array
			      structure Extern = Byte_Extern.Byte4)
 end
 


