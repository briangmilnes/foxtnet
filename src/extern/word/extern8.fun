(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken.Cline@cs.cmu.edu)
        Nick Haines (nickh@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	extern8.fun: extern structures for Word8s.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Protocol_Extern8

	iii.	RCS Log

$Log: extern8.fun,v $
Revision 1.3  1995/08/08  18:27:04  esb
separated input and output external structures.

Revision 1.2  1995/06/27  19:00:16  cline
adapted to new extern.sig

Revision 1.1  1995/06/20  17:43:24  esb
Initial revision


		1.	functor Protocol_Extern8
*)

functor Protocol_Extern8 (structure In: EXTERNAL
			  structure Out: EXTERNAL
			  structure B: FOX_BASIS): EXTERN_KEY =
 struct
  type T = FoxWord8.word
  type extern_in = In.T
  type extern_out = Out.T
  type cursor = int
  exception Extern

  val constant_size = 1
  fun size _ = constant_size

  fun marshal (extern, value) cursor =
       (Out.update (extern, cursor,
		    Word_Array.W8.create (value, constant_size));
	cursor + constant_size)

  fun unmarshal (extern, cursor) =
       case Word_Array.W8.next (In.sub (extern,
					{start = cursor,
					 length = constant_size})) of
	  NONE => raise Extern
	| SOME (value, _) => (value, cursor + constant_size)

  val makestring = FoxMakestring.word8

  fun equal (a: FoxWord8.word, b) = a = b
  val hash = FoxWord8.wordToInt

 end
