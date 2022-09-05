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
	
	A functor for the Dyn_Array extern of byte Ns.



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor DXS

		iii.	RCS Log
	
$Log: dxbyten.fun,v $
Revision 1.2  1994/11/09  20:49:59  esb
adapted to new extern.sig.

Revision 1.1  1994/08/12  14:26:08  milnes
Initial revision

		1.	functor
*)

functor DXByteN (structure Dyn_Array: DYNAMIC_BYTE_ARRAY
                 type byteN
                 val n: int 
                 val updateN: Dyn_Array.T * int * byteN -> unit
                 val subN: Dyn_Array.T * int -> byteN): EXTERN =
 struct
  type T = byteN
  type extern = Dyn_Array.T
  type cursor = Dyn_Array.T

  exception Extern

  fun size _ = n

  fun marshal (_, object) array =
       (updateN (array, 0, object);
	Dyn_Array.tail (array, n))

  fun unmarshal (_, array) =
       (subN (array, 0), Dyn_Array.tail (array, n))
 end

