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

	extern48.fun: extern structures for Word48s.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Protocol_Extern48_Big
	2.	functor Protocol_Extern48_Little

	iii.	RCS Log

$Log: extern48.fun,v $
Revision 1.5  1995/08/08  18:27:24  esb
separated input and output external structures.

Revision 1.4  1995/07/05  17:45:02  esb
adapted to new wordarray signature.

Revision 1.3  1995/06/27  18:59:50  cline
adapted to new extern.sig

Revision 1.2  1995/06/23  19:58:00  esb
bug fix.

Revision 1.1  1995/06/20  17:43:24  esb
Initial revision


		1.	functor Protocol_Extern48_Big
*)

functor Protocol_Extern48_Big (structure In: EXTERNAL
			       structure Out: EXTERNAL
			       structure B: FOX_BASIS): EXTERN_KEY =
 struct
  type T = FoxWord48.word
  type extern_in = In.T
  type extern_out = Out.T
  type cursor = int
  exception Extern

  val constant_size = 6
  fun size _ = constant_size

  fun marshal (extern, value) cursor =
       let val array = B.V.Misc.create_uninitialized constant_size
	   fun generate 0 = NONE
	     | generate index =
	        SOME (FoxWord8.sub (array, constant_size - index), index - 1)
       in FoxWord48.update_big (array, 0, value);
	  Out.update (extern, cursor, 
		      Word_Array.W8.new generate constant_size);
	  cursor + constant_size
       end

  fun unmarshal (extern, cursor) =
       let val w8 = In.sub (extern, {start = cursor,
				     length = constant_size})
	   val array = B.V.Misc.create_uninitialized constant_size
	   fun loop (NONE, _) = ()
	     | loop (SOME (value, rest), index) =
	        (FoxWord8.update (array, index, value);
		 loop (Word_Array.W8.next rest, index + 1))
       in loop (Word_Array.W8.next w8, 0);
	  (FoxWord48.sub_big (array, 0), cursor + constant_size)
       end

  fun makestring n =
       let fun makestring_one byte = FoxMakestring.word48 byte
	   val mask = SW.n48 "0xff"
	   fun make_byte shift =
	        FoxWord48.andb (FoxWord48.rshiftl (n, shift), mask)
	   val b0 = makestring_one (make_byte 40)
	   val b1 = makestring_one (make_byte 32)
	   val b2 = makestring_one (make_byte 24)
	   val b3 = makestring_one (make_byte 16)
	   val b4 = makestring_one (make_byte  8)
	   val b5 = makestring_one (make_byte  0)
	 in b0 ^ "." ^ b1 ^ "." ^ b2 ^ "." ^ b3 ^ "." ^ b4 ^ "." ^ b5
	 end

  fun equal (a: FoxWord48.word, b) = a = b
  val hash = FoxWord48.wordToInt

 end

(*
		2.	functor Protocol_Extern48_Little
*)

functor Protocol_Extern48_Little (structure In: EXTERNAL
				  structure Out: EXTERNAL
			          structure B: FOX_BASIS): EXTERN_KEY =
 struct
  type T = FoxWord48.word
  type extern_in = In.T
  type extern_out = Out.T
  type cursor = int
  exception Extern

  val constant_size = 6
  fun size _ = constant_size

  fun marshal (extern, value) cursor =
       let val array = B.V.Misc.create_uninitialized constant_size
	   fun generate 0 = NONE
	     | generate index =
	        SOME (FoxWord8.sub (array, constant_size - index), index - 1)
       in FoxWord48.update_little (array, 0, value);
	  Out.update (extern, cursor, 
		      Word_Array.W8.new generate constant_size);
	  cursor + constant_size
       end

  fun unmarshal (extern, cursor) =
       let val w8 = In.sub (extern, {start = cursor,
				     length = constant_size})
	   val array = B.V.Misc.create_uninitialized constant_size
	   fun loop (NONE, _) = ()
	     | loop (SOME (value, rest), index) =
	        (FoxWord8.update (array, index, value);
		 loop (Word_Array.W8.next rest, index + 1))
       in loop (Word_Array.W8.next w8, 0);
	  (FoxWord48.sub_little (array, 0), cursor + constant_size)
       end

  fun makestring n =
       let fun makestring_one byte = FoxMakestring.word48 byte
	   val mask = SW.n48 "0xff"
	   fun make_byte shift =
	        FoxWord48.andb (FoxWord48.rshiftl (n, shift), mask)
	   val b0 = makestring_one (make_byte 40)
	   val b1 = makestring_one (make_byte 32)
	   val b2 = makestring_one (make_byte 24)
	   val b3 = makestring_one (make_byte 16)
	   val b4 = makestring_one (make_byte  8)
	   val b5 = makestring_one (make_byte  0)
	 in b0 ^ "." ^ b1 ^ "." ^ b2 ^ "." ^ b3 ^ "." ^ b4 ^ "." ^ b5
	 end

  fun equal (a: FoxWord48.word, b) = a = b
  val hash = FoxWord48.wordToInt

 end
