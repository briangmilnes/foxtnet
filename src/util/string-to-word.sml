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



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature STRING_TO_WORD
	2.	functor SW
	3.	fun n
	4.	fun n64
	5.	fun n32
	6.	fun n16
	7.	fun n8

		iii.	RCS Log
	
$Log: string-to-word.sml,v $
Revision 1.4  1995/04/05  15:10:59  esb
added n48.

Revision 1.3  1995/03/07  20:26:41  esb
added n64.

Revision 1.2  1995/02/13  22:59:16  esb
adapted to not require the fox basis.

Revision 1.1  1995/02/04  21:53:59  robby
Initial revision

*)

(*
		1.	signature STRING_TO_WORD

*)

signature STRING_TO_WORD = 
 sig
  exception String_To_Word  of string

  val n64: string -> FoxWord64.word
  val n48: string -> FoxWord48.word
  val n32: string -> FoxWord32.word
  val n16: string -> FoxWord16.word
  val n8 : string -> FoxWord8.word
 end

(*
		2.	functor SW
*)

functor SW (val debug: bool
	    val print: string -> unit): STRING_TO_WORD = 
 struct
  exception String_To_Word of string

  (* All of the size specific stuff *)
  val stuff_64 = {ten = FoxWord64.intToWord 10,
		  sixteen = FoxWord64.intToWord 16,
		  zero = FoxWord64.intToWord 0,
		  zero_char = FoxWord64.intToWord (Char.ord #"0"),
		  nine_char = FoxWord64.intToWord (Char.ord #"9"),
		  a = FoxWord64.intToWord (Char.ord #"a"),
		  z = FoxWord64.intToWord (Char.ord #"z"),
		  A = FoxWord64.intToWord (Char.ord #"A"),
		  Z = FoxWord64.intToWord (Char.ord #"Z"),
		  minus = FoxWord64.-,
		  plus = FoxWord64.+,
		  times = FoxWord64.*,
		  leq = FoxWord64.<= ,
		  ord = FoxWord64.intToWord o Char.ord}
  val stuff_48 = {ten = FoxWord48.intToWord 10,
		  sixteen = FoxWord48.intToWord 16,
		  zero = FoxWord48.intToWord 0,
		  zero_char = FoxWord48.intToWord (Char.ord #"0"),
		  nine_char = FoxWord48.intToWord (Char.ord #"9"),
		  a = FoxWord48.intToWord (Char.ord #"a"),
		  z = FoxWord48.intToWord (Char.ord #"z"),
		  A = FoxWord48.intToWord (Char.ord #"A"),
		  Z = FoxWord48.intToWord (Char.ord #"Z"),
		  minus = FoxWord48.-,
		  plus = FoxWord48.+,
		  times = FoxWord48.*,
		  leq = FoxWord48.<= ,
		  ord = FoxWord48.intToWord o Char.ord}
  val stuff_32 = {ten = FoxWord32.intToWord 10,
		  sixteen = FoxWord32.intToWord 16,
		  zero = FoxWord32.intToWord 0,
		  zero_char = FoxWord32.intToWord (Char.ord #"0"),
		  nine_char = FoxWord32.intToWord (Char.ord #"9"),
		  a = FoxWord32.intToWord (Char.ord #"a"),
		  z = FoxWord32.intToWord (Char.ord #"z"),
		  A = FoxWord32.intToWord (Char.ord #"A"),
		  Z = FoxWord32.intToWord (Char.ord #"Z"),
		  minus = FoxWord32.-,plus = FoxWord32.+,times = FoxWord32.*,
		  leq = FoxWord32.<= ,ord = FoxWord32.intToWord o Char.ord}
  val stuff_16 = {ten = FoxWord16.intToWord 10,
		  sixteen = FoxWord16.intToWord 16,
		  zero = FoxWord16.intToWord 0,
		  zero_char = FoxWord16.intToWord (Char.ord #"0"),
		  nine_char = FoxWord16.intToWord (Char.ord #"9"),
		  a = FoxWord16.intToWord (Char.ord #"a"),
		  z = FoxWord16.intToWord (Char.ord #"z"),
		  A = FoxWord16.intToWord (Char.ord #"A"),
		  Z = FoxWord16.intToWord (Char.ord #"Z"),
		  minus = FoxWord16.-,plus = FoxWord16.+,times = FoxWord16.*,
		  leq = FoxWord16.<= ,ord = FoxWord16.intToWord o Char.ord}
  val stuff_8 = {ten = FoxWord8.intToWord 10,
		 sixteen = FoxWord8.intToWord 16,
		 zero = FoxWord8.intToWord 0,
		 zero_char = FoxWord8.intToWord (Char.ord #"0"),
		 nine_char = FoxWord8.intToWord (Char.ord #"9"),
		 a = FoxWord8.intToWord (Char.ord #"a"),
		 z = FoxWord8.intToWord (Char.ord #"z"),
		 A = FoxWord8.intToWord (Char.ord #"A"),
		 Z = FoxWord8.intToWord (Char.ord #"Z"),
		 minus = FoxWord8.-,plus = FoxWord8.+,times = FoxWord8.*,
		 leq = FoxWord8.<= ,ord = FoxWord8.intToWord o Char.ord}

(*
		3.	fun n
*)

  fun n {ten, sixteen, zero, zero_char, nine_char, a, z, A, Z, 
	 minus, plus, times, leq, ord} string = 
       let infix 8 plus minus 
	   infix 9 times leq
	   fun helper base (c :: cs) = 
	        let val ord_of = ord c
		    val num = if zero_char leq ord_of andalso
		                 ord_of leq nine_char then
			       ord_of minus zero_char
		              else if a leq ord_of andalso 
		                      ord_of leq z then
			       (ord_of minus a) plus ten
			      else if A leq ord_of andalso
		                      ord_of leq Z then
			       (ord_of minus A) plus ten
			      else
			       raise String_To_Word "unknown charater in word"
		in if base leq num then
		    raise String_To_Word "some character bigger than the base"
		   else num plus base times (helper base cs)
		end
	     | helper base [] = zero
       in (case explode string of
	      (#"0" :: #"x" :: rest) => helper sixteen (rev rest)
	    | number => helper ten (rev number))
	   handle String_To_Word s => raise String_To_Word (s ^ ": " ^ string)
		| Overflow =>
		   raise String_To_Word ("Constant too big for word: " ^
					 string)
       end

  fun n_debug (stuff, which) string = 
       (n stuff string)
       handle String_To_Word s =>
	       (print ("string-to-word.sml, " ^ which ^ ": " ^ s ^ "\n");
		raise String_To_Word s)

(*
		4.	fun n64
*)

  val n64 = if debug then n_debug (stuff_64, "SW.n64") else n stuff_64 

(*
		4.	fun n64
*)

  val n48 = if debug then n_debug (stuff_48, "SW.n48") else n stuff_48 

(*
		5.	fun n32
*)

  val n32 = if debug then n_debug (stuff_32, "SW.n32") else n stuff_32 

(*
		6.	fun n16
*)

  val n16 = if debug then n_debug (stuff_16, "SW.n16") else n stuff_16 

(*
		7.	fun n8
*)

  val n8 = if debug then n_debug (stuff_8, "SW.n8") else n stuff_8 

 end (* struct *)

(*
		7.	fun SW
*)

structure SW = SW (val debug = false
		   val print = Print.string)
