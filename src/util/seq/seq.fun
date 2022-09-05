(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	seq.sig: signature for sequence type.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Convert_Sequence 
	2.	structure List_Sequence
	3.	structure Lists
	4.	structure Stream_Sequence
	5.	structure Channel_Sequence

		iii.	RCS Log
	
$Log: seq.fun,v $
Revision 1.3  1995/07/05  17:47:43  esb
changed the definition of new to eliminate generator and iterator.

Revision 1.2  1995/06/29  18:23:18  esb
adapted to new seq.sig

Revision 1.1  1995/06/20  17:32:56  esb
Initial revision


		1.	functor Convert_Sequence 
*)

functor Convert_Sequence (structure From: SEQUENCE
			  structure To: SEQUENCE): CONVERT_SEQUENCE =
 struct
  structure From = From
  structure To = To

(*
  fun convert from =
       let fun to_generator (From.Gen from_generator) () =
	        case from_generator () of
		   NONE => NONE
		 | SOME (a, f) =>
		    SOME (a, To.Gen (to_generator f))
       in To.new (To.Gen (to_generator (From.iterator from)))
       end
*)

  fun convert from = To.new From.next from

 end (* struct *)

(*
		3.	functor From_List
*)

functor From_List (structure List: LIST): LIST_SEQUENCE =
 struct
  structure Basics =
   struct
    local
     signature S = sig datatype 'a list = nil | :: of 'a * 'a list
		       exception Empty
		   end
     structure S: S = List
    in
     open S
     infix ::
    end
    type 'a T = 'a list

(*
    datatype 'a generator = Gen of unit -> ('a * 'a generator) option
*)
   end

  open Basics

(*
  fun new (Gen f) =
       case f () of
	  NONE => List.nil
	| SOME (element, new_gen) => List.:: (element, new new_gen)
*)

  fun new f seq =
       case f seq of
          NONE => List.nil
	| SOME (element, new_seq) => List.:: (element, new f new_seq)

  fun create (_, 0) = List.nil
    | create (value, count) = List.:: (value, create (value, count - 1))

  val tabulate = List.tabulate

(*
  fun iterator nil = Gen (fn () => NONE)
    | iterator (element :: list) =
       Gen (fn () => SOME (element, iterator list))
*)

  fun next nil = NONE
    | next (op :: result) = SOME result

  fun seek (list, count) = ((List.drop (list, count)) handle _ => nil)

  val head = List.hd
  val tail = List.tl
  fun nth arg = ((List.nth arg) handle _ => raise Empty)

  fun isempty List.nil = true
    | isempty _ = false

  val length = List.length

  fun wait _ = ()

  val map = List.map
  val app = List.app
  val fold = List.foldr
  val append = List.@
  val concat = List.concat
  val reverse = List.rev
  val filter = List.mapPartial

  structure Rev =
   struct
    open Basics
    val straight_new = new
    fun new f = reverse o (straight_new f)
(*
    val iterator = iterator o reverse
*)
    fun reverse_second NONE = NONE
      | reverse_second (SOME (a, b)) = SOME (a, reverse b)
    fun reverse_first (a, b) = (reverse a, b)
    val next = reverse_second o next o reverse
    val seek = reverse o seek o reverse_first
    val head = head o reverse
    val tail = tail o reverse
    val nth = nth o reverse_first
    val isempty = isempty
    val wait = wait
    fun map f = reverse o (List.map f) o reverse
    fun app f = (List.app f) o reverse
    val fold = List.foldl
    fun filter f = List.mapPartial f o reverse
   end
 end (* struct *)

(*
		3.	structure List_Sequence
*)

structure List_Sequence = From_List (structure List = List)

(*
		4.	structure Stream_Sequence

	This implementation inspired by C. Okasaki's Stream module.
*)

structure Stream_Sequence: STREAM_SEQUENCE =
 struct
  structure Susp = System.Unsafe.Susp

  datatype 'a T = Empty_Stream
                | Cons of 'a * 'a T
		| Delay of 'a T Susp.susp

(*
  datatype 'a generator = Gen of unit -> ('a * 'a generator) option

  fun new (Gen f) =
       let fun suspension () =
	        case f () of
		   NONE => Empty_Stream
		 | SOME (element, new_gen) => Cons (element, new new_gen)
       in Delay (Susp.delay suspension)
       end
*)
  fun new f argument =
       let fun suspension () =
	        case f argument of
		   NONE => Empty_Stream
		 | SOME (element, new_arg) => Cons (element, new f new_arg)
       in Delay (Susp.delay suspension)
       end

  val delay = Delay o Susp.delay
  val force = Susp.force

(*
  fun iterator Empty_Stream = Gen (fn () => NONE)
    | iterator (Cons (element, stream)) =
       Gen (fn () => SOME (element, iterator stream))
    | iterator (Delay stream) = iterator (force stream)
*)

  val empty = Empty_Stream

  fun cons (element, stream) = Cons (element, stream)

  fun lazy_cons (element, stream) = Cons (element, delay stream)

  fun next Empty_Stream = NONE
    | next (Cons result) = SOME result
    | next (Delay stream) = next (force stream)

  fun seek (Empty_Stream, _) = Empty_Stream
    | seek (Delay stream, n) = seek (force stream, n)
    | seek (stream, 0) = stream
    | seek (Cons (_, stream), n) = seek (stream, n - 1)

  exception Empty
  fun head stream = case next stream of NONE => raise Empty | SOME (e, _) => e
  fun tail stream = case next stream of NONE => raise Empty | SOME (_, s) => s
  fun nth (Empty_Stream, _) = raise Empty
    | nth (Delay stream, n) = nth (force stream, n)
    | nth (Cons (element, _), 0) = element
    | nth (Cons (_, stream), n) = nth (stream, n - 1)

  fun isempty Empty_Stream = true
    | isempty (Cons _) = false
    | isempty (Delay stream) = isempty (force stream)

  fun length Empty_Stream = 0
    | length (Cons (_, stream)) = 1 + length stream
    | length (Delay stream) = length (force stream)

  fun wait Empty_Stream = ()
    | wait (Cons (_, stream)) = wait stream
    | wait (Delay stream) = wait (force stream)

  fun map f Empty_Stream = Empty_Stream
    | map f (Cons (element, stream)) = Cons (f element, map f stream)
    | map f (Delay stream) = delay (fn () => map f (force stream))

  fun app f Empty_Stream = ()
    | app f (Cons (element, stream)) = (f element; app f stream)
    | app f (Delay stream) = app f (force stream)

  fun fold f first Empty_Stream = first
    | fold f first (Cons (element, stream)) =
       fold f (f (element, first)) stream
    | fold f first (Delay stream) = fold f first (force stream)

  fun filter f Empty_Stream = Empty_Stream
    | filter f (Cons (element, stream)) =
       (case f element of
	   SOME a => Cons (a, filter f stream)
	 | _ => filter f stream)
    | filter f (Delay stream) = filter f (force stream)
 end (* struct *)

(*
		5.	structure Channel_Sequence
*)


