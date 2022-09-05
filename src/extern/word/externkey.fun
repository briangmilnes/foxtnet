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

	externkey.fun: generic extern structure for Word Arrays.  All
	the functors defined here implement aligned access for each
	word size.  Copy and modify the functions appropriately to
	provide unaligned access.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Protocol_Extern
	2.	functor Protocol_Extern8
	3.	functor Protocol_Extern16_Big
	4.	functor Protocol_Extern16_Little
	5.	functor Protocol_Extern32_Big
	6.	functor Protocol_Extern32_Little
	7.	functor Protocol_Extern48_Big
	8.	functor Protocol_Extern48_Little
	9.	functor Protocol_Extern64_Big
	10.	functor Protocol_Extern64_Little

	iii.	RCS Log

$Log: externkey.fun,v $
Revision 1.5  1997/02/06  16:37:58  cline
added Word32fox structure for compatibility with CM

Revision 1.4  1996/04/18  18:54:01  cline
changed hash from T->int to T->word.

Revision 1.3  1996/01/19  23:10:02  esb
adapted to the new wordarray signature.

Revision 1.2  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.1  1995/11/12  16:53:57  esb
Initial revision


		1.	functor Protocol_Extern
*)

functor Protocol_Extern (structure In: EXTERNAL
			 structure Out: EXTERNAL
			 structure Array: ARRAY_SEQ
			 structure Base_Word: FOXWORD
			   sharing type Array.element = Base_Word.word
			 val size: Word.word (* size of word, in bytes *)
			 val to: Word_Array.T -> Array.T
			 val from: Array.T -> Word_Array.T): EXTERN_KEY =
 struct
  type T = Base_Word.word
  type extern_in = In.T
  type extern_out = Out.T
  type cursor = Word.word
  exception Extern

  val constant_size = size
  fun size _ = constant_size

  fun marshal (extern, value) cursor =
       (Out.update (extern, cursor, from (Array.create (value, 0w1)));
	cursor + constant_size)

  fun unmarshal (extern, cursor) =
       case Array.next (to (In.sub (extern, {start = cursor,
					     length = constant_size}))) of
	  NONE => raise Extern
	| SOME (value, _) => (value, cursor + constant_size)

  fun makestring n =
       let val mask = Base_Word.fromInt 0xff
	   fun makestring_one byte =
	        Base_Word.toString (Base_Word.andb (byte, mask))
	   fun loop (0w0, _) = ""
	     | loop (0w1, value) = makestring_one value
	     | loop (n, value) =
	        loop (n - 0w1, Base_Word.>> (value, 0w8)) ^ "." ^
		makestring_one value
	 in loop (constant_size, n)
	 end

  fun equal (a: Base_Word.word, b) = a = b
  val hash = Word.fromLargeWord o Base_Word.toLargeWord

 end

(*
		2.	functor Protocol_Extern8
*)

functor Protocol_Extern8 (structure In: EXTERNAL
			  structure Out: EXTERNAL): EXTERN_KEY =
 struct
  local
   structure S = Protocol_Extern (structure In = In
				  structure Out = Out
				  structure Array = Word_Array.W8.U_Big.F
				  structure Base_Word = Word8
				  val size = 0w1
				  val to = Word_Array.to8
				  val from = Word_Array.from8)
  in
   open S
  end
 end

(*
		3.	functor Protocol_Extern16_Big
*)

functor Protocol_Extern16_Big (structure In: EXTERNAL
			       structure Out: EXTERNAL): EXTERN_KEY =
 struct
  local
   val to = Word_Array.W16.align_f o Word_Array.to16
   val from = Word_Array.from16 o Word_Array.W16.unalign_f
   structure S = Protocol_Extern (structure In = In
				  structure Out = Out
				  structure Array = Word_Array.W16.F_Big
				  structure Base_Word = Word16
				  val size = 0w2
				  val to = to
				  val from = from)
  in
   open S
  end
 end

(*
		4.	functor Protocol_Extern16_Little
*)

functor Protocol_Extern16_Little (structure In: EXTERNAL
			       structure Out: EXTERNAL): EXTERN_KEY =
 struct
  local
   val to = Word_Array.W16.align_f o Word_Array.to16
   val from = Word_Array.from16 o Word_Array.W16.unalign_f
   structure S = Protocol_Extern (structure In = In
				  structure Out = Out
				  structure Array = Word_Array.W16.F_Little
				  structure Base_Word = Word16
				  val size = 0w2
				  val to = to
				  val from = from)
  in
   open S
  end
 end

(*
		5.	functor Protocol_Extern32_Big
*)

functor Protocol_Extern32_Big (structure In: EXTERNAL
			       structure Out: EXTERNAL): EXTERN_KEY =
 struct
  local
   val to = Word_Array.W32.align_f o Word_Array.to32
   val from = Word_Array.from32 o Word_Array.W32.unalign_f
   structure S = Protocol_Extern (structure In = In
				  structure Out = Out
				  structure Array = Word_Array.W32.F_Big
				  structure Base_Word = Word32fox
				  val size = 0w4
				  val to = to
				  val from = from)
  in
   open S
  end
 end

(*
		6.	functor Protocol_Extern32_Little
*)

functor Protocol_Extern32_Little (structure In: EXTERNAL
			       structure Out: EXTERNAL): EXTERN_KEY =
 struct
  local
   val to = Word_Array.W32.align_f o Word_Array.to32
   val from = Word_Array.from32 o Word_Array.W32.unalign_f
   structure S = Protocol_Extern (structure In = In
				  structure Out = Out
				  structure Array = Word_Array.W32.F_Little
				  structure Base_Word = Word32fox
				  val size = 0w4
				  val to = to
				  val from = from)
  in
   open S
  end
 end

(*
		7.	functor Protocol_Extern48_Big
*)

functor Protocol_Extern48_Big (structure In: EXTERNAL
			       structure Out: EXTERNAL): EXTERN_KEY =
 struct
  local
   val to = Word48_Array.to
   val from = Word48_Array.from
   structure S = Protocol_Extern (structure In = In
				  structure Out = Out
				  structure Array = Word48_Array.U_Big.F
				  structure Base_Word = Word48
				  val size = 0w6
				  val to = to
				  val from = from)
  in
   open S
  end
 end

(*
		8.	functor Protocol_Extern48_Little
*)

functor Protocol_Extern48_Little (structure In: EXTERNAL
			       structure Out: EXTERNAL): EXTERN_KEY =
 struct
  local
   val to = Word48_Array.to
   val from = Word48_Array.from
   structure S = Protocol_Extern (structure In = In
				  structure Out = Out
				  structure Array = Word48_Array.U_Little.F
				  structure Base_Word = Word48
				  val size = 0w6
				  val to = to
				  val from = from)
  in
   open S
  end
 end
(*
		9.	functor Protocol_Extern64_Big
*)

functor Protocol_Extern64_Big (structure In: EXTERNAL
			       structure Out: EXTERNAL): EXTERN_KEY =
 struct
  local
   val to = Word_Array.W64.align_f o Word_Array.to64
   val from = Word_Array.from64 o Word_Array.W64.unalign_f
   structure S = Protocol_Extern (structure In = In
				  structure Out = Out
				  structure Array = Word_Array.W64.F_Big
				  structure Base_Word = Word64
				  val size = 0w8
				  val to = to
				  val from = from)
  in
   open S
  end
 end

(*
		10.	functor Protocol_Extern64_Little
*)

functor Protocol_Extern64_Little (structure In: EXTERNAL
			       structure Out: EXTERNAL): EXTERN_KEY =
 struct
  local
   val to = Word_Array.W64.align_f o Word_Array.to64
   val from = Word_Array.from64 o Word_Array.W64.unalign_f
   structure S = Protocol_Extern (structure In = In
				  structure Out = Out
				  structure Array = Word_Array.W64.F_Little
				  structure Base_Word = Word64
				  val size = 0w8
				  val to = to
				  val from = from)
  in
   open S
  end
 end
