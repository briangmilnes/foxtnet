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

	protoextern.fun: external structure for protocols (one possible
	implementation).

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Protocol_External
	2.	function fold
	3.	functions makestring and makestring_max

	iii.	RCS Log

$Log: protoextern.fun,v $
Revision 1.8  1996/02/23  21:37:17  esb
now allows sub of an empty T, if the length is zero.

Revision 1.7  1996/02/15  19:05:14  esb
makestring_max now prints "..." if some elements are not printed.

Revision 1.6  1996/01/19  23:05:43  esb
adapted to the new wordarray signature.

Revision 1.5  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.4  1995/11/12  16:40:14  esb
adapted to new Word_Array.

Revision 1.3  1995/07/05  17:44:26  esb
adapted to new wordarray signature.

Revision 1.2  1995/06/23  19:57:05  esb
improved error reporting on exceptions.

Revision 1.1  1995/06/20  17:09:33  esb
Initial revision


	1.	functor Protocol_External
*)

functor Protocol_External (structure B: FOX_BASIS
			   val debug_level: int ref option): EXTERNAL =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "protoextern.fun"
			   val makestring = fn _ => NONE)

  exception Illegal_Access

  fun illegal_access string = Trace.print_raise (Illegal_Access, SOME string)

  structure WAF = Word_Array.W8.U_Big.F
  structure WAR = Word_Array.W8.U_Big.R

  datatype T = Empty
             | Single of Word_Array.W8.T
             | Multiple of Word_Array.W8.T * Word_Array.W8.T list

  fun new array =
       let val a8 = Word_Array.to8 array
       in if WAF.length a8 < 0w1 then Empty
	  else Single a8
       end

  fun uninitialized size =
       if size < 0w1 then Empty
       else Single (WAF.create (Word8.fromInt 0, size))

  fun size Empty = 0w0
    | size (Single array) = WAF.length array
    | size (Multiple (head, rest)) =
       B.V.List.fold (fn (a, remainder_size) =>
		      (WAF.length a + remainder_size)) (head :: rest) 0w0

  val empty_sub = Word_Array.from8 (WAF.create (Word8.fromInt 0, 0w0))
  fun sub (Empty, {start, length}) =
       if length = 0w0 then empty_sub else illegal_access "sub of empty T"
    | sub (Single array, {start, length}) =
       let fun next (0w0, array) = NONE
	     | next (count, array) =
		case WAF.next array of
		   NONE => illegal_access "sub longer than T"
		 | SOME (value, new_array) =>
		    SOME (value, (count - 0w1, new_array))
       in Word_Array.from8
	    (WAF.new next (length, WAF.seek (array, start)))
       end
    | sub (array as (Multiple (head, tail)), {start, length}) =
       let fun next (_, 0w0) = NONE
	     | next ((NONE, []), count) = illegal_access "sub longer than T"
	     | next ((NONE, head :: tail), count) =
	        next ((WAF.next head, tail), count)
	     | next ((SOME (value, head), list), count) =
	        SOME (value, ((WAF.next head, list), count - 0w1))
	   fun skip_loop ([], 0w0) = (NONE, [])
	     | skip_loop ([], _) = illegal_access "sub starts past end of T"
	     | skip_loop (head :: tail, count) =
	        let val head_length = WAF.length head
		in if count >= head_length then
		    skip_loop (tail, count - head_length)
		   else (NONE, WAF.seek (head, count) :: tail)
		end
       in Word_Array.from8 (WAF.new next (skip_loop (head :: tail, start),
					  length))
       end

  fun update (Empty, _, _) = illegal_access "updating empty T"
    | update (Single array, start, value) =
       let fun loop (NONE, _) = ()
	     | loop (_, NONE) = illegal_access "update longer than T"
	     | loop (SOME (v, rest), SOME updatable) =
	        loop (WAF.next rest, WAF.update (updatable, v))
       in loop (WAF.next (Word_Array.to8 value),
		WAF.write (WAF.seek (array, start)))
       end
    | update (array as (Multiple (head, tail)), start, value) =
       let fun loop (NONE, (_, _)) = ()
	     | loop (_, (NONE, [])) = illegal_access "update longer than T"
	     | loop (v, (NONE, head :: tail)) =
	        loop (v, (WAF.write head, tail))
	     | loop (SOME (v, rest), (SOME updatable, list)) =
	        loop (WAF.next rest,
		      (WAF.update (updatable, v), list))
	   fun skip_loop ([], 0w0) = (NONE, [])
	     | skip_loop ([], _) = illegal_access "update starts past T"
	     | skip_loop (head :: tail, count) =
	        let val head_length = WAF.length head
		in if count >= head_length then
		    skip_loop (tail, count - head_length)
		   else (NONE, WAF.seek (head, count) :: tail)
		end
       in loop (WAF.next (Word_Array.to8 value),
		skip_loop (head :: tail, start))
       end

  fun join (Empty, array) = array
    | join (array, Empty) = array
    | join (Single a1, Single a2) = Multiple (a1, [a2])
    | join (Single a1, Multiple (a2, arrays)) = Multiple (a1, a2 :: arrays)
    | join (Multiple (a1, arrays), Single a2) = Multiple (a1, arrays @ [a2])
    | join (Multiple (a1, a1s), Multiple (a2, a2s)) =
       Multiple (a1, a1s @ [a2] @ a2s)

  fun split_single (array, position, length) =
       (Single (WAR.seek (array, length - position)),
	Single (WAF.seek (array, position)))

  fun split_multiple (first, second :: rest, 0w0) =
       (Empty, Multiple (first, second :: rest))
    | split_multiple (head, [], position) =
       (Single (WAR.seek (head, WAF.length head - position)),
	Single (WAF.seek (head, position)))
    | split_multiple (first, second :: rest, position) =
       let val length = WAF.length first
       in if position < length then
	   (Single (WAR.seek (first, length - position)),
	    Multiple (WAF.seek (first, position), second :: rest))
	  else
	   let val (first_half, second_half) =
	             split_multiple (second, rest, position - length)
	   in (join (Single first, first_half), second_half)
	   end
       end

  fun split (array, position) =
       let val length = size array
       in if position < 0w0 then
	   illegal_access "negative split position"
          else if position > length then
	   illegal_access "split position beyond end of T"
          else if position = length then (array, Empty)
          else if position = 0w0 then (Empty, array)
          else
	   case array of
	      Empty => illegal_access "splitting empty T"
	    | Single array => split_single (array, position, length)
	    | Multiple (head, tail) => split_multiple (head, tail, position)
       end

(*
	2.	function fold
*)

  fun fold (Empty, f, init) = init
    | fold (Single array, f, init) = f (Word_Array.from8 array, init)
    | fold (Multiple (first, rest), f, init) =
       let fun fold_forward ([], result) = result
	     | fold_forward (head :: tail, result) =
	        fold_forward (tail, f (Word_Array.from8 head, result))
       in fold_forward (first :: rest, init)
       end

(*
	3.	functions makestring and makestring_max
*)

  fun makestring_max (Empty, _) = ""
    | makestring_max (Single array, count) =
       let fun loop (NONE, _) = ""
	     | loop (_, 0w0) = "..."
	     | loop (SOME (byte, rest), count) =
	        Word8.toString byte ^
	        (if WAF.length rest = 0w0 orelse count = 0w1 then ""
		 else "." ^ loop (WAF.next rest, count - 0w1))
       in loop (WAF.next array, count)
       end
    | makestring_max (Multiple (head, tail), count) =
       let fun loop (NONE, [], _) = ""
	     | loop (NONE, head :: tail, 0w0) = "..."
	     | loop (NONE, head :: tail, count) =
	        loop (WAF.next head, tail, count)
	     | loop (SOME (byte, rest), list, count) =
	        Word8.toString byte ^
	        (if WAF.length rest = 0w0 andalso list = [] then ""
		 else if count = 0w1 then "..."
		 else "." ^ loop (WAF.next rest, list, count - 0w1))
       in loop (WAF.next head, tail, count)
       end

  fun makestring array = makestring_max (array, size array)

 end (* struct *)

