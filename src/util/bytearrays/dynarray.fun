(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A functor for dynamic arrays which can be shortened from
	either the front or the back.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Dynamic_Array
	2.	zero-sized array
	3.	function new
	4.	init functions
	5.	function size
	6.	function array
	7.	function read
	8.	function copy
	9.	function append
	10.	function equal
	11.	function checksum
	12.	function tail
	13.	function revtail
	14.	sub functions
	15.	update functions
	16.	map functions
	17.	fold functions
	18.	function makestring

		iii.	RCS Log
	
$Log: dynarray.fun,v $
Revision 1.10  1995/06/20  17:30:03  esb
minor changes.

Revision 1.9  1995/03/24  01:49:03  esb
adapted to new dynarray.sig.

Revision 1.8  1995/03/10  03:51:29  esb
adapted to new vendor.sig.

Revision 1.7  1995/02/13  23:00:05  esb
redesigned map, added app and the string conversion functions.

Revision 1.6  1994/09/30  16:25:52  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.5  1994/09/12  18:30:22  milnes
Fixed an alignment bug.

Revision 1.4  1994/08/24  20:20:22  esb
handle unaligned case using "if" instead of exceptions for sub2, update2

Revision 1.3  1994/08/17  15:18:20  esb
fixed a bug in checking bounds for updating a structured array.

Revision 1.2  1994/08/12  04:10:12  esb
added check for negative indices.

Revision 1.1  1994/08/02  20:17:30  esb
Initial revision


		1.	functor Dynamic_Array
*)

functor Dynamic_Array (structure Copy: COPY
		       structure Create: CREATE
		       structure Checksum: CHECKSUM
		       structure Access: ACCESS
		       structure V: VENDOR): DYNAMIC_BYTE_ARRAY =
 struct

(*
		1.	exceptions and type declarations
*)

  (* raised if an access would violate the bounds of the array. *)
  exception Subscript
  (* raised if a ubyteN operation (N > 1) is applied and the
     size of the array are not multiples of N. *)
  exception Alignment
  (* raised on a tail operation if the number of elements to be
     hidden is greater than the number of elements in the array. *)
  exception Size

  fun makestring_exn Subscript = SOME "Dyn_Array.Subscript"
    | makestring_exn Alignment = SOME "Dyn_Array.Alignment"
    | makestring_exn Size = SOME "Dyn_Array.Size"
    | makestring_exn _ = NONE
  
  structure Trace = Trace (structure V = V
			   val debug_level = NONE
			   val module_name = "dynarray.fun"
			   val makestring = makestring_exn)

  fun print_raise (string, ex) =
       Trace.print_raise_again (ex, SOME string)

  datatype T = Single of {data: ByteArray.bytearray, start: int, length: int}
             | Split of T list

(*
		2.	zero-sized array
*)

  val empty = Single {data = Create.create 0, start = 0, length = 0}

(*
		3.	function new
*)

  val zero_byte = FoxWord8.intToWord 0

  fun new n =
       Single {data = Create.create_fn (n, fn _ => zero_byte),
	       start = 0, length = n}

(*
		4.	init functions
*)

  fun init data =
       Single {data = data, start = 0, length = ByteArray.length data}

  fun init1 (n, f) =
       Single {data = Create.create_fn (n, f), start = 0, length = n}

  fun init2 (n, f) =
       if Bits.andb (n, 0x1) = 0 andalso n >= 0 then
	let val data = Create.create n
	    fun map index =
	         if index >= n then ()
		 else (FoxWord16.update (data, index, f index);
		       map (index + 2))
	in map 0;
           Single {data = data, start = 0, length = n}
	end
       else print_raise ("init2", Alignment)

  fun init4 (n, f) =
       if Bits.andb (n, 0x3) = 0 andalso n >= 0 then
	let val data = Create.create n
	    fun map index =
	         if index >= n then ()
		 else (FoxWord32.update (data, index, f index);
		       map (index + 4))
	in map 0;
           Single {data = data, start = 0, length = n}
	end
       else print_raise ("init4", Alignment)

(*
		5.	functions init_list
*)

  fun init_list1 list =
       let val length = length list
	   val data = Create.create length
	   fun fold_fun (value, offset) =
	        (FoxWord8.update (data, offset, value);
		 offset + 1)
       in V.List.revfold fold_fun list 0;
          Single {data = data, start = 0, length = length}
       end

  fun init_list2 list =
       let val length = length list * 2
	   val data = Create.create length
	   fun fold_fun (value, offset) =
	        (FoxWord16.update (data, offset, value);
		 offset + 2)
       in V.List.revfold fold_fun list 0;
          Single {data = data, start = 0, length = length}
       end

  fun init_list4 list =
       let val length = length list * 4
	   val data = Create.create length
	   fun fold_fun (value, offset) =
	        (FoxWord32.update (data, offset, value);
		 offset + 4)
       in V.List.revfold fold_fun list 0;
          Single {data = data, start = 0, length = length}
       end

(*
		5.	function init_string

	It is likely that a more efficient implementation is possible.
	If you need efficiency, please write such an implementation.
*)

  val init_string = init o Access.from_string

(*
		5.	function size
*)

  fun size (Single {length, ...}) = length
    | size (Split list) = V.List.fold op+ (map size list) 0

(*
		6.	function array
*)

  exception Illegal_Array

  fun array (Single {data, start, length}) =
       if length = ByteArray.length data then data
       else print_raise ("array", Illegal_Array)
    | array (Split x) = print_raise ("array", Illegal_Array)

(*
		7.	function read
*)

  fun read (Single {data, start, length}) =
       Create.copy_create (data, start, length)
    | read (Split x) =
       let val total_size = size (Split x)
	   val result = Create.create total_size
	   val subs = map read x
	   fun copy ([], _) = ()
	     | copy (head :: rest, index) =
	        (Copy.copy (head, 0, ByteArray.length head, result, index);
		 copy (rest, index + ByteArray.length head))
       in copy (subs, 0);
	  result
       end

(*
		7.	functions to_list
*)

  fun to_list1 array =
       let val data = read array
	   val size = ByteArray.length data
	   fun loop index =
	        if index >= size then []
		else FoxWord8.sub (data, index) :: loop (index + 1)
       in loop 0
       end

  fun to_list2 array =
       let val data = read array
	   val size = ByteArray.length data
	   fun loop index =
	        if index >= size then []
		else FoxWord16.sub (data, index) :: loop (index + 2)
       in if size mod 2 <> 0 then print_raise ("to_list2", Alignment)
	  else loop 0
       end

  fun to_list4 array =
       let val data = read array
	   val size = ByteArray.length data
	   fun loop index =
	        if index >= size then []
		else FoxWord32.sub (data, index) :: loop (index + 4)
       in if size mod 4 <> 0 then print_raise ("to_list4", Alignment)
	  else loop 0
       end

(*
		9.	function append
*)

  fun append list = Split list

(*
		10.	function equal
*)

  fun equal (Single {data = d1, start = s1, length = l1},
	     Single {data = d2, start = s2, length = l2}) =
       (if l1 <> l2 then false
	else
	 (let fun fold n =
	           if FoxWord8.sub (d1, n + s1) <>
		      FoxWord8.sub (d2, n + s2) then
		    false
		   else fold (n + 1)
	  in fold 0
	  end)
	  handle FoxWord8.Subscript => true)
    | equal (a, b) =
       let val sa = size a
	   val sb = size b
	   fun compare (da, db, index) =
	        if index >= sa then true
		else FoxWord8.sub (da, index) =
		     FoxWord8.sub (db, index) andalso
		     compare (da, db, index + 1)
       in if sa = sb then
	   compare (read a, read b, 0)
	  else false
       end

(*
		11.	function checksum
*)

  fun checksum (Single {data, start, length}, index, bytes) =
       ((Checksum.checksum (data, start + index, start + index + bytes))
	handle _ => print_raise ("checksum", Subscript))
    | checksum (array, index, bytes) =
       ((Checksum.checksum (read array, index, index + bytes))
	handle _ => print_raise ("checksum", Subscript))

(*
		12.	function tail
*)

  (* make the first n elements inaccessible, decrease all remaining
     indices by n.  The resulting array shares with the original array. *)
  fun tail (Single {data, start, length}, n) =
       if 0 <= n andalso n <= length then
	Single {data = data, start = start + n, length = length - n}
       else print_raise ("tail", Size)
    | tail (Split [], n) = print_raise ("tail", Size)
    | tail (Split [last], n) = tail (last, n)
    | tail (Split (first :: rest), n) =
       let val first_size = size first
       in if first_size < n then tail (Split rest, n - first_size)
          else if first_size = n then Split rest
          else Split (tail (first, n) :: rest)
       end

(*
		13.	function revtail
*)

  (* make the last n elements inaccessible.  The resulting array shares
     with the original array. *)
  fun revtail (Single {data, start, length}, n) =
       if 0 <= n andalso n <= length then
	Single {data = data, start = start, length = length - n}
       else print_raise ("revtail", Size)
    | revtail (Split [], n) = print_raise ("revtail", Size)
    | revtail (Split [first], n) = revtail (first, n)
    | revtail (Split (first :: rest), n) =
       let val rest_size = size first
       in if rest_size < n then revtail (first, n - rest_size)
          else if rest_size = n then first
          else case revtail (Split rest, n) of
	          Single data => Split [first, Single data]
		| Split new_rest => Split (first :: new_rest)
       end

(*
		14.	sub functions
*)

  fun sub (Single {data, start, length}, index, bytes) =
       if index < 0 orelse index + bytes > length then
	print_raise ("sub", Subscript)
       else Create.copy_create (data, start + index, bytes)
    | sub (Split [], index, bytes) = print_raise ("sub", Subscript)
    | sub (Split (head :: rest), index, bytes) =
       let val head_size = size head
       in if head_size >= index + bytes then sub (head, index, bytes)
          else if head_size <= index then
	   sub (Split rest, index - head_size, bytes)
          else Create.copy_create (read (Split (head :: rest)), index, bytes)
       end

  fun sub1 (Single {data, start, length}, index) =
       if index < 0 orelse index >= length then
	print_raise ("sub1", Subscript)
       else FoxWord8.sub (data, index + start)
    | sub1 (array, index) = FoxWord8.sub (sub (array, index, 1), 0)

  fun sub2 (Single {data, start, length}, index) =
       if index < 0 orelse index + 1 >= length then
	print_raise ("sub2", Subscript)
       else if Bits.andb (index+start, 0x1) = 0 then FoxWord16.sub (data, index + start)
       else
	let val short = Create.create 2
	    val pos = start + index
	in FoxWord8.update (short, 0, FoxWord8.sub (data, pos));
	   FoxWord8.update (short, 1, FoxWord8.sub (data, pos + 1));
	   FoxWord16.sub (short, 0)
	end
    | sub2 (array, index) = FoxWord16.sub (sub (array, index, 2), 0)

  fun sub4 (Single {data, start, length}, index) =
    if index < 0 orelse index + 3 >= length then
         print_raise ("sub4", Subscript)
       else if Bits.andb (index+start, 0x3) = 0 then 
            FoxWord32.sub (data, index + start)
       else 
	let val short = Create.create 4
	    val pos = start + index
	in 
           FoxWord8.update (short, 0, FoxWord8.sub (data, pos));
	   FoxWord8.update (short, 1, FoxWord8.sub (data, pos + 1));
	   FoxWord8.update (short, 2, FoxWord8.sub (data, pos + 2));
	   FoxWord8.update (short, 3, FoxWord8.sub (data, pos + 3));
           FoxWord32.sub (short, 0)
	end
    | sub4 (array, index) = 
       FoxWord32.sub (sub (array, index, 4), 0)

  val substring = Access.to_string o sub

(*
		15.	update functions
*)

  fun update (Single {data, start, length}, index, value) =
       let val bytes = ByteArray.length value
       in if index < 0 orelse index + bytes > length then
	   print_raise ("update", Subscript)
	  else Copy.copy (value, 0, bytes, data, start + index)
       end
    | update (Split [], index, value) = print_raise ("update", Subscript)
    | update (Split (head :: rest), index, value) =
       let val head_size = size head
	   val bytes = ByteArray.length value
       in if head_size >= index + bytes then update (head, index, value)
          else if head_size <= index then
	   update (Split rest, index - head_size, value)
          else if index < 0 then print_raise ("update", Subscript)
          else
	   let val first_copy = head_size - index
	       val rest_copy = ByteArray.length value - first_copy
	       val first_value = Create.copy_create (value, 0, first_copy)
	       val rest_value = Create.copy_create (value, first_copy,
						    rest_copy)
	   in update (head, index, first_value);
	      update (Split rest, 0, rest_value)
	   end
       end

  fun update1 (Single {data, start, length}, index, value) =
       if index < 0 orelse index >= length then
	print_raise ("update1", Subscript)
       else FoxWord8.update (data, index + start, value)
    | update1 (array, index, value) =
       let val value_array = Create.create 1
       in FoxWord8.update (value_array, 0, value);
          update (array, index, value_array)
       end

  fun update2 (Single {data, start, length}, index, value) =
       if index < 0 orelse index + 1 >= length then
	print_raise ("update2", Subscript)
       else  (* optimization for aligned FoxWord32 update. *)
	let val pos = index + start
	in if Bits.andb (pos, 0x1) = 0 then FoxWord16.update (data, pos, value)
	   else 
	    let val short = Create.create 2
	        val pos = index + start
	    in FoxWord16.update (short, 0, value);
	       FoxWord8.update (data, pos, FoxWord8.sub (short, 0));
	       FoxWord8.update (data, pos + 1, FoxWord8.sub (short, 1))
	    end
	end
    | update2 (array, index, value) =
       let val value_array = Create.create 2
       in FoxWord16.update (value_array, 0, value);
          update (array, index, value_array)
       end

  fun update4 (Single {data, start, length}, index, value) =
       if index < 0 orelse index + 3 >= length then
	print_raise ("update4", Subscript)
       else  (* optimization for aligned FoxWord32 update. *)
	let val pos = index + start
	in if Bits.andb (pos, 0x3) = 0 then FoxWord32.update (data, pos, value)
	   else
	    let val short = Create.create 4
	    in FoxWord32.update (short, 0, value);
	       FoxWord8.update (data, pos, FoxWord8.sub (short, 0));
	       FoxWord8.update (data, pos + 1, FoxWord8.sub (short, 1));
	       FoxWord8.update (data, pos + 2, FoxWord8.sub (short, 2));
	       FoxWord8.update (data, pos + 3, FoxWord8.sub (short, 3))
	    end
	end
    | update4 (array, index, value) =
       let val value_array = Create.create 4
       in FoxWord32.update (value_array, 0, value);
          update (array, index, value_array)
       end

  fun updatestring (dynarray, index, string) =
       update (dynarray, index, Access.from_string string)

(*
		8.	function copy
*)

  fun copy (Single {data = d1, start = s1, length = l1}, start1, length,
	    Single {data = d2, start = s2, length = l2}, start2) =
       ((Copy.copy (d1, s1 + start1, length, d2, s2 + start2))
	handle _ => print_raise ("copy", Subscript))
    | copy (a1, start1, length, a2, start2) =
       update (a2, start2, sub (a1, start1, length))

(*
		16.	map functions
*)

  fun map1 f dynarray =
       init1 (size dynarray, fn index => f (index, sub1 (dynarray, index)))

  fun map2 f dynarray =
       init2 (size dynarray, fn index => f (index, sub2 (dynarray, index)))

  fun map4 f dynarray =
       init4 (size dynarray, fn index => f (index, sub4 (dynarray, index)))

(*
		16.	app functions
*)

  fun app_array (array, f, sub, update, index, step, 0) = ()
    | app_array (array, f, sub, update, index, step, count) =
       (update (array, index, f (index, sub (array, index)));
	app_array (array, f, sub, update, index + step, step, count - step))

  fun app1 (dynarray, f) =
       ((app_array (dynarray, f, sub1, update1, 0, 1, size dynarray))
	 handle _ => print_raise ("app1", Alignment))

  fun app2 (dynarray, f) =
       ((app_array (dynarray, f, sub2, update2, 0, 2, size dynarray))
	handle _ => print_raise ("app2", Alignment))

  fun app4 (dynarray, f) =
       ((app_array (dynarray, f, sub4, update4, 0, 4, size dynarray))
	handle _ => print_raise ("app4", Alignment))

(*
		17.	fold functions
*)

  fun fold_array (array, step, 0, sub, f, init) = f (0, sub (array, 0), init)
    | fold_array (array, step, index, sub, f, init) =
       fold_array (array, step, index - step, sub, f,
		   f (index, sub (array, index), init))

  fun fold1 f dynarray init =
       fold_array (dynarray, 1, size dynarray - 1, sub1, f, init)

  fun fold2 f dynarray init =
       ((fold_array (dynarray, 2, size dynarray - 2, sub2, f, init))
	handle _ => print_raise ("fold2", Alignment))

  fun fold4 f dynarray init =
       ((fold_array (dynarray, 4, size dynarray - 4, sub4, f, init))
	handle _ => print_raise ("fold4", Alignment))

(*
		18.	function makestring
*)

  fun makestring {data, start_print, max_length, base, separator} =
       let val bytebase = FoxWord8.intToWord base
	   fun print_base n =
	        if n = zero_byte then
		 if base = 10 then ""
		 else if base = 16 then "0x"
		 else if base = 2 then "0b"
		 else if base = 8 then "0o"
		 else V.Integer.makestring base ^ "_"
		else
	         print_base (FoxWord8.div (n, bytebase)) ^
		 (case FoxWord8.wordToInt (FoxWord8.mod (n, bytebase)) of
		     0x0 => "0"
		   | 0x1 => "1"
		   | 0x2 => "2"
		   | 0x3 => "3"
		   | 0x4 => "4"
		   | 0x5 => "5"
		   | 0x6 => "6"
		   | 0x7 => "7"
		   | 0x8 => "8"
		   | 0x9 => "9"
		   | 0xa => "A"
		   | 0xb => "B"
		   | 0xc => "C"
		   | 0xd => "D"
		   | 0xe => "E"
		   | 0xf => "F"
		   | x => "'" ^ V.Integer.makestring x ^ "'")
           fun print_zero value =
	        if value = zero_byte then print_base value ^ "0"
		else print_base value
           fun print_one (pos, value, prev) =
	        if pos < start_print orelse
                   pos >= max_length + start_print then
		 prev
		else if prev <> "" then print_zero value ^ separator ^ prev
		else print_zero value
       in fold1 print_one data ""
       end

 end
