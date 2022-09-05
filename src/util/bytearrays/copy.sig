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

	Efficient copy functions in ML.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature COPY

		iii.	RCS Log
	
$Log: copy.sig,v $
Revision 1.9  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.8  1994/07/04  21:37:53  esb
extracted the array creation function.

Revision 1.7  94/03/16  16:30:27  esb
added the Self_Copy exception.

Revision 1.6  94/03/10  19:34:31  esb
added create_buffer.

Revision 1.5  94/02/18  16:21:07  esb
added exception Illegal_Create

Revision 1.4  94/01/14  17:48:25  cline
Added create function.

Revision 1.3  1993/11/04  16:11:03  esb
added the exception Illegal_Copy.

Revision 1.2  1993/10/29  05:37:52  esb
replaced the generic type bytearray with the specific ByteArray.bytearray.

Revision 1.1  1993/10/29  04:46:02  esb
Initial revision

	1.	signature COPY
*)

signature COPY =
 sig

  (* raised if the copy would access data outside one of the arrays *)
  exception Illegal_Copy of {source_length: int, source_offset: int,
			     bytes: int, dest_length: int, dest_offset: int}

  (* copy does not currently allow the source and destination arrays to
     be the same array. Self_Copy is raised when copying to oneself. *)
  exception Self_Copy

  (* generic copy. Likely to be both correct and highly optimized.
     The source and destination byte arrays must be distinct, i.e.
     this should not be used to copy within a byte array. *)
  val copy: Word8Array.array * int (* source *) * int (* number of bytes *)
          * Word8Array.array * int (* destination *)
          -> unit

 end (* sig *)
