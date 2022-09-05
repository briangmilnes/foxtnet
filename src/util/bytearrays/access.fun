(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139 - 3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Access

		iii.	RCS Log
	
$Log: access.fun,v $
Revision 1.16  1997/02/05  20:45:39  esb
replaced quot by div to make it work with 109.25.

Revision 1.15  1996/03/12  22:27:47  esb
adapted to new FOXWORD.

Revision 1.14  1996/03/11  20:06:57  cline
updated for 109.6

Revision 1.13  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.12  1995/06/20  17:28:48  esb
minor changes.

Revision 1.11  1995/02/04  21:47:04  robby
updated to 107

Revision 1.10  1994/09/30  16:24:38  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.9  1994/07/01  02:54:36  danwang
Removed bytearray type.

Revision 1.8  1994/06/27  17:04:00  robby
added to the Range datatype

Revision 1.7  94/02/20  16:47:10  esb
minor change.

Revision 1.6  94/02/20  21:44:10  milnes
Updated to use safe operations depending on internal functor switch.

Revision 1.5  1993/10/25  19:54:06  cline
removed spurious !

Revision 1.4  1993/10/25  19:37:01  cline
removed .U from Byte[421].U

Revision 1.3  1993/09/17  16:45:43  milnes
>
Changed default parameters..

Revision 1.2  1993/09/02  15:44:14  esb
no major changes.


	1.	functor Access
*)

functor Access (structure V: VENDOR) : ACCESS =
 struct

  datatype range_spec =
      Range of {first : int, last : int}
    | Test of {first: int, test: Word8.word -> bool}
    | Length of {first: int, length: int}
    | All

  exception Bad_Range

  local
   val (update, sub, sub2) = 
          (Word8Array.update, Word8Array.sub, Word16.sub)

   fun isEven i = Bits.andb (i,1) = 0

   fun checked_update (a, n, u) =
        Word8Array.update (a, n, u)
        handle Subscript => raise Bad_Range

   fun check_end (Range {first, last}) array pos=pos=last + 1
     | check_end (Test {first, test}) array pos=
        (test (sub (array, pos))
	 handle Subscript => raise Bad_Range)
     | check_end (Length {first, length}) array pos=pos=first + length
     | check_end All array pos=Word8Array.length array=pos

   fun check_range (Range {first, last}, b) =
        if first > last orelse last >= Word8Array.length b orelse first < 0
	  then raise Bad_Range
        else ()
     | check_range (Test {first, test}, b) = 
        if first < 0 orelse first > Word8Array.length b then
	  raise Bad_Range
        else ()
     | check_range (Length {first, length}, b) =
        if length < 0 orelse Word8Array.length b < first + length
           orelse first < 0 then raise Bad_Range
        else ()
     | check_range (All, b) = ()

(* This takes the bytearray in case you want to add a range type that needs
   to know about the bytearray before it can find the first element in the 
   range. *)
   fun first_pos (Range {first, last}) b=first
     | first_pos (Test {first, test}) b=first
     | first_pos (Length {first, length}) b=first
     | first_pos All b=0

(* This returns the position of the element one greater than the last element
   in the range, unless the range is empty, in which case it returns the 
   beginning of the range. *)
    fun last_pos (Range {first, last}) b=last
      | last_pos (Test {first, test}) b=
	if (test (sub (b, first))) handle Subscript => raise Bad_Range then
	  first 
	else
	  last_pos (Test {first=first + 1, test=test}) b
      | last_pos (Length {first, length}) b=first + length
      | last_pos All b=Word8Array.length b + 1

  in

   fun app_range (range, f, array) =
     let fun helper pos=
       if check_end range array pos then ()
       else (update (array, pos, f (sub (array, pos))); 
	     helper (pos + 1))
     in 
       check_range (range, array); 
       helper (first_pos range array)
     end

   fun fold_range range f init array=
     let fun helper pos accum=
       if (check_end range array pos) then accum
       else helper (pos + 1) (f (sub (array, pos), accum))
     in 
       check_range (range, array); 
       helper (first_pos range array) init
     end

   fun fold_range2 range f init array=
     let fun helper pos accum=
       if (check_end range array pos) then accum
       else helper (pos + 2) (f (sub2 (array, pos) handle Subscript => 
			 	                raise Bad_Range, 
			     accum))
     in 
       check_range (range, array); 
       helper (first_pos range array) init
     end

   fun from_list l =
    let val length = V.List.length l
        val a = Word8Array.array (length, 0w0); 
	fun loop _ []      = a
	  | loop n (u::us) = (update (a, n, u); loop (n + 1) us)
    in loop 0 l end
       
   fun to_list b =
    let fun loop a 0 = a
	  | loop a n = loop ( (sub (b, n - 1)) ::a) (n - 1)
    in loop [] (Word8Array.length b)
    end

   fun to_list2 b = 
    let val len = Word8Array.length b
        fun loop a 0 = a
	  | loop a n = loop ( (sub2 (b, n - 2)) ::a) (n - 2)
    in if not (isEven len) then raise Bad_Range else loop [] len
    end

   fun from_string s =
    Word8Array.tabulate (V.String.length s,
			 fn i => Word8.fromInt
			           (V.Char.ord (V.String.ordof (s, i))))

   fun to_string b =
     V.String.implode
       (Word8Array.foldr (fn (x, l) => (V.Char.chr (Word8.toInt x))::l) [] b)

   fun list_to_range (_, _   , []     ) = ()
     | list_to_range (b, from, (u :: us)) = (checked_update (b, from, u); 
					     list_to_range (b, from + 1, us))

   fun range_to_list (b, r as Range {first, last}) =
    let fun loop a n = if n = first then sub (b, first) :: a
		       else loop ( (sub (b, n)) ::a) (n - 1)
    in (check_range (r, b); 
	loop [] last)
    end
     | range_to_list (b, r) =fold_range r (fn (byte, list) =>byte::list) [] b

   fun range_to_list2 (b, r as Range {first, last}) = 
       let val f = first
	 fun loop a n = if n = f then sub2 (b, f) :: a
			else loop ( (sub2 (b, n)) ::a) (n - 2)
       in (check_range (r, b); 
	   if not (isEven first) orelse isEven last then
	     raise Bad_Range
	   else loop [] (last - 1))
       end
     | range_to_list2 (b, r) =
       fold_range2 r (fn (byte, list) =>byte::list) [] b

   fun string_to_range (b, offset, s) =
    let fun loop ([], _) = ()
	  | loop (x::rest, i) =
	      (Word8Array.update (b, i, Word8.fromInt (V.Char.ord x));
	       loop (rest, i+1))
    in loop (V.String.explode s, offset)
    end

   fun range_to_string arg =
     V.String.implode
       (map (fn w => V.Char.chr (Word8.toInt w)) (range_to_list arg))

   fun compare_range_to_list (b, r as Range {first, last}, l) =
    let val length = Word8Array.length b
        fun check (n, []) = n > last
	  | check (n, u :: rest) = ( (n <= last) andalso
				    sub (b, n) = u andalso
				    check (n + 1, rest))
    in (check_range (r, b); 
	check (first, l))
       handle Bad_Range => false
    end
     | compare_range_to_list (b, r, l) =
       #1 (fold_range r (fn (byte, (so_far, rest)) => 
			 (so_far andalso (rest <> [])
			  andalso (hd rest) = byte, 
			  tl rest))
	              (true, l) b)

   fun compare_range_to_list2 (b, r as Range {first, last}, l) =
    let fun check (n, []) = n > last
	  | check (n, u::rest) = (n <= last andalso
				  sub2 (b, n) =u andalso
				  check (n + 2, rest))
    in (check_range (r, b); 
	if not (isEven first) orelse isEven last then false
	else
	 check (first, l))
       handle Bad_Range => false
    end
     | compare_range_to_list2 (b, r, l) =
       #1 (fold_range2 r (fn (byte, (so_far, rest)) => 
			 (so_far andalso (rest <> [])
			  andalso (hd rest) = byte, 
			  tl rest))
	              (true, l) b)

   fun compare_range_to_bytearray ((b1, r1 as Range {first, last}),
				   b2, offset) =
    let val l2 = Word8Array.length b2
        val num = last - first
	fun check (_ , _ , 0) = true
	  | check (n1, n2, n) = (sub2 (b1, n1) = sub2 (b2, n2) andalso
				 check (n1 + 2, n2 + 2, n - 1))
    in if not (isEven first) orelse not (isEven offset)
          orelse num + offset > l2 then false
       else
	 (check_range (r1, b1); 
	  check (first, offset, (num + 1) div 2))
       handle Bad_Range => false
    end
     | compare_range_to_bytearray ((b, r), b2, offset) =
       #1 (fold_range r (fn (byte, (so_far, pos)) => 
			 (so_far andalso (byte=sub (b2, pos)), pos + 1))
	              (true, offset) b)

    fun compare_ranges ( (b1, r1 as Range {first = f1, last = l1}),
			(b2, r2 as Range {first = f2, last = l2})) =
     let val s1 = l1 - f1
         val s2 = l2 - f2
         fun check (_ , _ , 0) = true
           | check (n1, n2, n) = (sub2 (b1, n1) = sub2 (b2, n2) andalso
             check (n1 + 2, n2 + 2, n - 1))
     in
      if (s1 <> s2) orelse 
         not (isEven f1) orelse not (isEven f2) orelse
         not (isEven l1) orelse not (isEven l2)
         then false
         else
            (check_range (r1, b1); 
             check_range (r2, b2); 
             check (f1, f2, (s1 + 1) div 2))
             handle Bad_Range => false
         end
      | compare_ranges ( (b, r), (b', r')) =
	let fun helper pos=
	  if check_end r b pos andalso check_end r' b' pos
	    then true
	  else if check_end r b pos then false
	  else if check_end r' b' pos then false
	  else if sub (b, pos) =sub (b', pos) then helper (pos + 1)
	       else false
	in
	  if first_pos r b=first_pos r' b then helper (first_pos r b)
	  else false
	end
   
   fun fill_range (b, r, u) =
        (check_range (r, b);
	 app_range (r, (fn _ => u), b))

   fun fill (b, u) = fill_range (b, All, u)

  end (* local *)

 end (* let *)
