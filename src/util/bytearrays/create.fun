(*
        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
        Brian Milnes (Brian.Milnes@cs.cmu.edu)
        Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

		i.	Abstract

        Efficient array creation functions in ML.

---------------------------------------------------------------------
		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	Functor Create
	2.	Function create
	3.	Function copy_create
	4.	Function create_fn

---------------------------------------------------------------------
		iii.	RCS Log
        
$Log: create.fun,v $
Revision 1.4  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.3  1995/03/10  03:51:47  esb
create now provides uninitialized arrays.

Revision 1.2  1994/09/30  16:25:52  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.1  1994/07/04  18:16:19  esb
Initial revision

---------------------------------------------------------------------
	1.	Functor Create

*)

functor Create (structure V: VENDOR
		structure Copy: COPY): CREATE =
 struct

  exception Illegal_Create of int (* raised if the size is negative *)

(*
---------------------------------------------------------------------
	2.	Function create
*)

  fun create n =
       ((V.Misc.create_uninitialized n)
	handle V.Misc.Bad_Size => raise Illegal_Create n)

(*
---------------------------------------------------------------------
	3.	Function copy_create
*)

  fun copy_create (from, start, size) =        (* timed in create and copy *)
       let val to = create size
       in Copy.copy (from, start, size, to, 0);
          to
       end

(*
---------------------------------------------------------------------
	4.	Function create_fn
*)

  fun create_fn (size, f) =
       let val new = create size
           fun loop k =
                if k < size then
                 (Word8Array.update (new, k, f k);
                  loop (k + 1))
                else new
       in loop 0
       end

 end (* struct *)




