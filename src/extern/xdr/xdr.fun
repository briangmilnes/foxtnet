(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Kuo Chiang Chiang (kcchiang@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract
		
	Implements the different XDR marshal/unmarshal functions.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor XInt
	2.	functor XVoid
	3.	functor XEnum
	4.	functor XString
	5.	functor XFString
	6.	functor XBool
	7.	functor XUByte4
	8.	functor XFOpaque
	9.	functor XOpaque
	10.	functor XFArray
	11.	functor XArray
	12.	functor XUnion2
	13.	functor XUnion2_Default
	14.	functor XUnion3_Default
	15.	functor XStruct2
	16.	functor XStruct3
	17.	functor XStruct4
	18.	functor XStruct5
	19.	functor XStruct6
	20.	functor XStruct7
	21.	functor XList
	22.	functor XIntOpt

	iii.	RCS Log
	
$Log: xdr.fun,v $
Revision 1.5  1995/06/20  17:47:07  esb
minor fix.

Revision 1.4  1995/05/02  23:28:39  cstone
Changed unions with default option to return the selector when
the default case is selected.

Revision 1.3  1995/02/10  15:38:18  esb
replaced fox_basis with the individual structures.

Revision 1.2  1995/02/07  23:26:57  esb
adapted to new extern.sig.

Revision 1.1  1994/10/14  12:00:02  kcchiang
Initial revision

	1.	functor XInt

	This functor marshals and unmarshals non-negative integers.
*)

functor XInt (structure D: DYNAMIC_BYTE_ARRAY
	      structure B4: BYTE_ORDER
	       sharing type B4.T = FoxWord32.word): INT_XDR =
  struct
    type T = int
    type extern = D.T
    type cursor = D.T
    exception Extern
    
    val fixed_size = 4
    fun size n = if n < 0 then raise Extern (* sanity check *) else fixed_size
      
    fun marshal (_, n) cursor =
      if n < 0 then 
	raise Extern
      else
	(D.update4 (cursor, 0, B4.to_big (FoxWord32.intToWord n));
	 D.tail (cursor, fixed_size))

    fun unmarshal (_, cursor) =
      (FoxWord32.wordToInt (B4.from_big (D.sub4 (cursor, 0))),
       D.tail (cursor, fixed_size))

  end (* struct *)

(*
	2.	functor XVoid
*)

functor XVoid (structure D: DYNAMIC_BYTE_ARRAY): VOID_XDR =
  struct
    type T = unit
    type extern = D.T
    type cursor = D.T
    exception Extern

    fun size _ = 0

    fun marshal (_, ()) cursor = cursor

    fun unmarshal (_, cursor) = ((), cursor)
  end (* struct *)

(*
	3.	functor XEnum

	Enum works the same as Int except that it has range checking.
*)

functor XEnum (structure Int: INT_XDR
	       val range: int * int): ENUM_XDR =
  struct
    type T = Int.T
    type extern = Int.extern
    type cursor = Int.cursor
    exception Extern

    local
      val (range_low, range_high) = range

      fun check_range x =
        if x < range_low orelse x > range_high then raise Extern else ()

    in
      fun size n = 
        (check_range n;
	 Int.size n)

      fun marshal (extern, n) cursor = 
	(check_range n;
	 Int.marshal (extern, n) cursor)

      fun unmarshal (extern, cursor) =
	let val (res, new_cursor) = Int.unmarshal (extern, cursor)
	in check_range res;
	  (res, new_cursor)
	end

    end (* local *)
  end (* struct *)

(*
	4.	functor XString

	Structure Int is required to un/marshal the string length.
*)

functor XString (structure Int: INT_XDR
		 structure D: DYNAMIC_BYTE_ARRAY
		  sharing type Int.extern = Int.cursor = D.T
		 structure V: VENDOR
		 structure Access: ACCESS): STRING_XDR =
  struct
    type T = string
    type extern = Int.extern
    type cursor = Int.cursor
    exception Extern

    fun size string =			(* strings are padded in XDR *)
      let val length = V.String.length string
      in if length mod 4 = 0 then length + Int.size length
	 else (V.Integer.div (length, 4) + 1) * 4 + Int.size length
      end
    
    fun marshal (extern, string) cursor =
      let val length = V.String.length string
	val byte_array = Access.from_string string
	val mid_cursor = Int.marshal (extern, length) cursor;
	val final_cursor = D.tail (cursor, size string)
      in D.update (mid_cursor, 0, byte_array);
	final_cursor
      end

    fun unmarshal (argument as (_, cursor)) =
      let val (length, mid_cursor) = Int.unmarshal argument
	val string = ByteArray.extract (D.sub (mid_cursor, 0, length),
					0, length)
	val word_length = size string
      in (string, D.tail (cursor, word_length))
      end

  end (* struct *)

(*
	5.	functor XFString

	This is the same as String, but only works for fixed size strings.
*)

functor XFString (val number_of_bytes: int
		  structure String: STRING_XDR
		  structure D: DYNAMIC_BYTE_ARRAY
		  sharing type String.extern
		             = String.cursor = D.T): STRING_XDR =
  struct
    type T = string
    type extern = D.T
    type cursor = D.T
    exception Extern

    local
      fun check_size s = 
        let val size = String.size s
        in if size > number_of_bytes then raise Extern else size
        end

      fun pad (cursor, 0) = cursor
	| pad (cursor, count) =
	  (D.update1 (cursor, 0, FoxWord8.intToWord 0);
	   pad (D.tail (cursor, 1), count - 1))
    in
      fun size string =
        (check_size string;
	 number_of_bytes)

      fun marshal (extern, string) cursor = 
        (pad (String.marshal (extern, string) cursor,
	      number_of_bytes - check_size string))

      fun unmarshal (argument as (_, cursor)) = 
        let val (res, _) = String.unmarshal argument
	in (res, D.tail (cursor, number_of_bytes))
	end
    end (* local *)
  end (* struct *)

(*
	6.	functor XBool
*)

functor XBool (structure Int: INT_XDR): BOOL_XDR =
  struct
    type T = bool
    type extern = Int.extern
    type cursor = Int.cursor
    exception Extern

    fun size _ = 4

    fun marshal (extern, true) cursor = Int.marshal (extern, 1) cursor
      | marshal (extern, false) cursor = Int.marshal (extern, 0) cursor

    fun unmarshal argument =
      let val (result, cursor) = Int.unmarshal argument
      in (result <> 0, cursor)
      end

  end (* struct *)

(*
	7.	functor XUByte4

	Functor for un/marshaling unsigned word32 values.
*)

functor XUByte4 (structure D: DYNAMIC_BYTE_ARRAY
		 structure B4: BYTE_ORDER
		 sharing type B4.T = FoxWord32.word): UBYTE_XDR =
  struct
    type T = B4.T
    type extern = D.T
    type cursor = D.T
    exception Extern

    val fixed_size = 4
    fun size _ = fixed_size

    fun marshal (_, value) cursor =
      (D.update4 (cursor, 0, B4.to_big value);
       D.tail (cursor, fixed_size))

    fun unmarshal (_, cursor) =
      (B4.from_big (D.sub4 (cursor, 0)),
       D.tail (cursor, fixed_size))

  end (* struct *)

(*
	8.	functor XFOpaque

	Functor for un/marshaling fixed-size opaque (uninterpreted) values.
*)

functor XFOpaque (val fixed_size: int
		  structure D: DYNAMIC_BYTE_ARRAY): OPAQUE_XDR =
  struct
    type T = ByteArray.bytearray
    type extern = D.T
    type cursor = D.T
    exception Extern

    local
      val allocation = fixed_size + (case fixed_size mod 4 of 0 => 0 | _ => 4)
    (* add padding to 4-byte boundaries*)
    in
      fun size b =
        if ByteArray.length b > fixed_size then raise Extern
	else allocation

      fun marshal (_, opaque) cursor =
        if ByteArray.length opaque <> fixed_size then raise Extern
        else
	  (D.update (cursor, 0, opaque);
	   D.tail (cursor, allocation))

      fun unmarshal (_, cursor) =
	   (D.sub (cursor, 0, fixed_size),
	    D.tail (cursor, allocation))

    end (* local *)
  end (* struct *)

(*
	9.	functor XOpaque

	Functor for un/marshaling variable sized opaque values.
	The argument Int is used to marshal and unmarshal the size.
*)

functor XOpaque (structure Int: INT_XDR
		 structure D: DYNAMIC_BYTE_ARRAY
		 sharing type Int.extern = Int.cursor = D.T): OPAQUE_XDR =
  struct
    type T = ByteArray.bytearray
    type extern = D.T
    type cursor = D.T
    exception Extern

    fun size opaque =
      let val length = ByteArray.length opaque
	val opaque_allocation =
	  length + (case length mod 4 of 0 => 0 | _ => 4)
	val int_size = Int.size length
      in opaque_allocation + int_size
      end

    fun marshal (extern, opaque) cursor =
      let val allocation = size opaque
	val length = ByteArray.length opaque
	val length_cursor = Int.marshal (extern, length) cursor
      in D.update (length_cursor, 0, opaque);
	D.tail (cursor, allocation)
      end

    fun unmarshal argument =
      let val (length, length_cursor) = Int.unmarshal argument
	val opaque = D.sub (length_cursor, 0, length)
	val allocation = length + (case length mod 4 of 0 => 0 | _ => 4)
      in (opaque, D.tail (length_cursor, allocation))
      end

  end (* struct *)

(*
	10.	functor XFArray

	Un/marshaling of fixed size arrays.
*)

functor XFArray (val fixed_size: int
		 structure Element: EXTERN): ARRAY_XDR =
  struct
    type T = Element.T Array.array
    type extern = Element.extern
    type cursor = Element.cursor
    exception Extern

    local
      fun array_fold f array initial =
        let fun loop (value, 0) = value
	      | loop (value, index) =
		loop (f (Array.sub (array, index - 1), value), index - 1)
	in loop (initial, Array.length array)
	end

    in

      fun size array =
        if Array.length array <> fixed_size then raise Extern
	else
	  array_fold (fn (element, sum) => Element.size element + sum) array 0

      fun marshal (extern, array) =
        let fun marshal_element (element, marshal_rest) =
	  (Element.marshal (extern, element) o marshal_rest)
	    fun initial cursor = cursor
	in array_fold marshal_element array initial
	end

      fun unmarshal (extern, initial_cursor) =
        let fun list_elements (0, cursor) = ([], cursor)
	      | list_elements (count, cursor) =
		let val (element, next_cursor) =
		  Element.unmarshal (extern, cursor)
		    val (rest, final_cursor) =
		      list_elements (count - 1, next_cursor)
		in (element :: rest, final_cursor)
		end
	    val (list, final_cursor) =
	      list_elements (fixed_size, initial_cursor)
	in (Array.fromList list, final_cursor)
	end

    end (* local *)

  end (* struct *)
				     
(*
	11.	functor XArray

	Un/marshaling of variable size arrays.  Int is used to marshal and
	unmarshal the length of the array.
*)

functor XArray (structure Int: INT_XDR
		structure Element: EXTERN
		sharing type Element.extern = Int.extern
			and type Element.cursor = Int.cursor): ARRAY_XDR =
  struct
    type T = Element.T Array.array
    type extern = Element.extern
    type cursor = Element.cursor
    exception Extern

    local
      fun array_fold f array initial =
        let fun loop (value, 0) = value
	      | loop (value, index) =
		loop (f (Array.sub (array, index - 1), value), index - 1)
	in loop (initial, Array.length array)
	end

    in
      fun size array =
	array_fold (fn (element, sum) => Element.size element + sum) array
	(Int.size (Array.length array))

      fun marshal (extern, array) =
	 let fun marshal_element (element, marshal_rest) =
	   (Element.marshal (extern, element) o marshal_rest)
	     val initial = Int.marshal (extern, Array.length array)
	 in array_fold marshal_element array initial
	 end

      fun unmarshal (extern, initial_cursor) =
        let fun list_elements (0, cursor) = ([], cursor)
	      | list_elements (count, cursor) =
		let val (element, next_cursor) =
		  Element.unmarshal (extern, cursor)
		    val (rest, final_cursor) =
		      list_elements (count - 1, next_cursor)
		in (element :: rest, final_cursor)
		end
	    val (list, final_cursor) =
	      list_elements (Int.unmarshal (extern, initial_cursor))
	in (Array.fromList list, final_cursor)
	end

    end (* local *)
  end (* struct *)

(*
	12.	functor XUnion2

	Un/marshal the union of 2 other types.
	The Enum structure is used to un/marshal the discriminant.
*)

functor XUnion2 (structure Variant_1: EXTERN
		 structure Variant_2: EXTERN
		 structure Enum: ENUM_XDR
		 sharing type Variant_1.extern = Variant_2.extern = Enum.extern
		     and type Variant_1.cursor = Variant_2.cursor = Enum.cursor
		 val discriminants: Enum.T * Enum.T): UNION2_XDR =
  struct
    type extern = Enum.extern
    type cursor = Enum.cursor
    exception Extern

    structure Variant_1 = Variant_1
    structure Variant_2 = Variant_2

    datatype union = T1 of Variant_1.T | T2 of Variant_2.T
    type T = union

    val (discriminant_1, discriminant_2) = discriminants

    fun size (T1 value) = Variant_1.size value + Enum.size discriminant_1
      | size (T2 value) = Variant_2.size value + Enum.size discriminant_2

    fun marshal (extern, T1 value) cursor =
        Variant_1.marshal (extern, value) 
          (Enum.marshal (extern, discriminant_1) cursor)
      | marshal (extern, T2 value) cursor =
	Variant_2.marshal (extern, value)
	(Enum.marshal (extern, discriminant_2) cursor)

    fun unmarshal (extern, cursor) =
      let val (discriminant, new_cursor) = Enum.unmarshal (extern, cursor)
      in if discriminant = discriminant_1 then
	let val (value, final) = Variant_1.unmarshal (extern, new_cursor)
	in (T1 value, final)
	end
      else 
	if discriminant = discriminant_2 then
	  let val (value, final) = Variant_2.unmarshal (extern, new_cursor)
	  in (T2 value, final)
	  end
	else
	  raise Extern
      end

  end (* struct *)

(* 
	13.	functor XUnion2_Default

	Same as above, but supports DEFAULT value for discriminant
*)

functor XUnion2_Default
         (structure Variant_1: EXTERN
	  structure Variant_2: EXTERN
	  structure Enum: ENUM_XDR
	  sharing type Variant_1.extern = Variant_2.extern = Enum.extern
	      and type Variant_1.cursor = Variant_2.cursor = Enum.cursor
          val discriminant: Enum.T): UNION2D_XDR = 

  struct
    type extern = Enum.extern
    type cursor = Enum.cursor
    exception Extern

    structure Variant_1 = Variant_1
    structure Variant_2 = Variant_2
    structure Enum = Enum

    type discriminant = Enum.T
    datatype union = T1 of Variant_1.T | T2 of discriminant * Variant_2.T
    type T = union

    val discriminant = discriminant 

    fun size (T1 value) = Variant_1.size value + Enum.size discriminant
      | size (T2 (disc, value)) = Variant_2.size value + Enum.size disc

    fun marshal (extern, T1 value) cursor =
         Variant_1.marshal (extern, value) 
         (Enum.marshal (extern, discriminant) cursor)
      | marshal (extern, T2 (disc, value)) cursor =
	 Variant_2.marshal (extern, value) 
	 (Enum.marshal (extern, disc) cursor)

    fun unmarshal (extern, cursor) =
      let val (discriminant', new_cursor) = Enum.unmarshal (extern, cursor)
      in if discriminant' = discriminant then
	  let val (value, final) = Variant_1.unmarshal (extern, new_cursor)
	  in (T1 value, final)
	  end
	 else
	  let val (value, final) = Variant_2.unmarshal (extern, new_cursor)
	  in (T2 (discriminant', value), final)
	  end
      end

  end (* struct *)

(*
	14.	functor XUnion3_Default
*)

functor XUnion3_Default
    (structure Variant_1: EXTERN
     structure Variant_2: EXTERN
     structure Variant_3: EXTERN
     structure Enum: ENUM_XDR
       sharing type Variant_1.extern = Variant_2.extern
	     = Variant_3.extern = Enum.extern
	   and type Variant_1.cursor = Variant_2.cursor
	     = Variant_3.cursor = Enum.cursor
     val discriminants: Enum.T * Enum.T): UNION3D_XDR = 
  struct
    type extern = Enum.extern
    type cursor = Enum.cursor
    exception Extern

    structure Variant_1 = Variant_1
    structure Variant_2 = Variant_2
    structure Variant_3 = Variant_3
    structure Enum = Enum

    type discriminant = Enum.T
    datatype union = T1 of Variant_1.T
    | T2 of Variant_2.T 
    | T3 of discriminant * Variant_3.T
    type T = union

    val (discriminant_1, discriminant_2) = discriminants

    fun size (T1 value) = Variant_1.size value + Enum.size discriminant_1
      | size (T2 value) = Variant_2.size value + Enum.size discriminant_2
      | size (T3 (disc,value)) = Variant_3.size value + Enum.size disc

    fun marshal (extern, T1 value) cursor =
        Variant_1.marshal (extern, value) 
        (Enum.marshal (extern, discriminant_1) cursor)
      | marshal (extern, T2 value) cursor =
	Variant_2.marshal (extern, value)
	(Enum.marshal (extern, discriminant_2) cursor)
      | marshal (extern, T3 (disc, value)) cursor =
	Variant_3.marshal (extern, value) 
	(Enum.marshal (extern, disc) cursor)

    fun unmarshal (extern, cursor) =
      let val (discriminant', new_cursor) = Enum.unmarshal (extern, cursor)
      in if discriminant' = discriminant_1 then
	let val (value, final) = Variant_1.unmarshal (extern, new_cursor)
	in (T1 value, final)
	end
      else 
	if discriminant' = discriminant_2 then
	  let val (value, final) = Variant_2.unmarshal (extern, new_cursor)
	  in (T2 value, final)
	  end
	else
	  let val (value, final) = Variant_3.unmarshal (extern, new_cursor)
	  in (T3 (discriminant', value), final)
	  end
      end

  end (* struct *)

(*
	15.	functor XStruct2

	Un/marshal "structures" (SML tuples) with 2 elements.
*)

functor XStruct2 (structure X1: EXTERN
		  structure X2: EXTERN
		  sharing type X1.extern = X2.extern
		      and type X1.cursor = X2.cursor): STRUCT_XDR =
  struct
    type T = X1.T * X2.T
    type extern = X1.extern
    type cursor = X1.cursor
    exception Extern

    fun size (left, right) = X1.size left + X2.size right

    fun marshal (extern, (left, right)) cursor =
      X2.marshal (extern, right) (X1.marshal (extern, left) cursor)

    fun unmarshal (extern, cursor) =
      let val (left, middle) = X1.unmarshal (extern, cursor)
	val (right, final) = X2.unmarshal (extern, middle)
      in ((left, right), final)
      end
  end (* struct *)

(*
	16.	functor XStruct3
*)

functor XStruct3 (structure X1: EXTERN
		  structure X2: EXTERN
		  structure X3: EXTERN
		  sharing type X1.extern = X2.extern = X3.extern
		      and type X1.cursor = X2.cursor = X3.cursor)
                 : STRUCT_XDR =
  struct
    type T = X1.T * X2.T * X3.T
    type extern = X1.extern
    type cursor = X1.cursor
    exception Extern

    local
      structure Left = XStruct2 (structure X1 = X1
				 structure X2 = X2)
      structure Both = XStruct2 (structure X1 = Left
				 structure X2 = X3)
    in
      fun size (left, middle, right) = Both.size ((left, middle), right)

      fun marshal (extern, (left, middle, right)) cursor =
        Both.marshal (extern, ((left, middle), right)) cursor

      fun unmarshal arg =
        let val (((left, middle), right), final_cursor) = Both.unmarshal arg
	in ((left, middle, right), final_cursor)
	end
    end (* local *)
  end (* struct *)

(*
	17.	functor XStruct4
*)

functor XStruct4 (structure X1: EXTERN
		  structure X2: EXTERN
		  structure X3: EXTERN
		  structure X4: EXTERN
		  sharing type X1.extern = X2.extern = X3.extern = X4.extern
		      and type X1.cursor = X2.cursor
		             = X3.cursor = X4.cursor): STRUCT_XDR =
  struct
    type T = X1.T * X2.T * X3.T * X4.T
    type extern = X1.extern
    type cursor = X1.cursor
    exception Extern

    local
      structure Left = XStruct2 (structure X1 = X1
				 structure X2 = X2)
      structure Right = XStruct2 (structure X1 = X3
				  structure X2 = X4)
      structure All = XStruct2 (structure X1 = Left
				structure X2 = Right)
    in
      fun size (x1, x2, x3, x4) = All.size ((x1, x2), (x3, x4))

      fun marshal (extern, (x1, x2, x3, x4)) cursor =
        All.marshal (extern, ((x1, x2), (x3, x4))) cursor

      fun unmarshal arg =
        let val (((x1, x2), (x3, x4)), final_cursor) = All.unmarshal arg
	in ((x1, x2, x3, x4), final_cursor)
	end

    end (* local *)

  end (* struct *)

(*
	18.	functor XStruct5
*)

functor XStruct5 (structure X1: EXTERN
		  structure X2: EXTERN
		  structure X3: EXTERN
		  structure X4: EXTERN
		  structure X5: EXTERN
		  sharing type X1.extern = X2.extern = X3.extern = X4.extern
		             = X5.extern
		      and type X1.cursor = X2.cursor = X3.cursor = X4.cursor
		             = X5.cursor): STRUCT_XDR =
  struct
    type T = X1.T * X2.T * X3.T * X4.T * X5.T
    type extern = X1.extern
    type cursor = X1.cursor
    exception Extern

    local
      structure Left = XStruct3 (structure X1 = X1
				 structure X2 = X2
				 structure X3 = X3)
      structure Right = XStruct2 (structure X1 = X4
				  structure X2 = X5)
      structure All = XStruct2 (structure X1 = Left
				structure X2 = Right)
    in
      fun size (x1, x2, x3, x4, x5) = All.size ((x1, x2, x3), (x4, x5))

      fun marshal (extern, (x1, x2, x3, x4, x5)) cursor =
        All.marshal (extern, ((x1, x2, x3), (x4, x5))) cursor

      fun unmarshal arg =
        let val (((x1, x2, x3), (x4, x5)), final_cursor) = All.unmarshal arg
	in ((x1, x2, x3, x4, x5), final_cursor)
	end

    end (* local *)

  end (* struct *)

(*
	19.	functor XStruct6
*)

functor XStruct6 (structure X1: EXTERN
		  structure X2: EXTERN
		  structure X3: EXTERN
		  structure X4: EXTERN
		  structure X5: EXTERN
		  structure X6: EXTERN
		  sharing type X1.extern = X2.extern = X3.extern = X4.extern
		             = X5.extern = X6.extern
		      and type X1.cursor = X2.cursor = X3.cursor = X4.cursor
		             = X5.cursor = X6.cursor): STRUCT_XDR =
  struct
    type T = X1.T * X2.T * X3.T * X4.T * X5.T * X6.T
    type extern = X1.extern
    type cursor = X1.cursor
    exception Extern

    local
      structure Left = XStruct3 (structure X1 = X1
				 structure X2 = X2
				 structure X3 = X3)
      structure Right = XStruct3 (structure X1 = X4
				  structure X2 = X5
				  structure X3 = X6)
      structure All = XStruct2 (structure X1 = Left
				structure X2 = Right)
    in
      fun size (x1, x2, x3, x4, x5, x6) = All.size ((x1, x2, x3), (x4, x5, x6))

      fun marshal (extern, (x1, x2, x3, x4, x5, x6)) cursor =
        All.marshal (extern, ((x1, x2, x3), (x4, x5, x6))) cursor

      fun unmarshal arg =
        let val (((x1, x2, x3), (x4, x5, x6)),
		 final_cursor) = All.unmarshal arg
	in ((x1, x2, x3, x4, x5, x6), final_cursor)
	end

    end (* local *)

  end (* struct *)

(*
	20.	functor XStruct7
*)

functor XStruct7 (structure X1: EXTERN
		  structure X2: EXTERN
		  structure X3: EXTERN
		  structure X4: EXTERN
		  structure X5: EXTERN
		  structure X6: EXTERN
		  structure X7: EXTERN
		  sharing type X1.extern = X2.extern = X3.extern = X4.extern
		             = X5.extern = X6.extern = X7.extern
		      and type X1.cursor = X2.cursor = X3.cursor = X4.cursor
		             = X5.cursor = X6.cursor = X7.cursor): STRUCT_XDR =
  struct
    type T = X1.T * X2.T * X3.T * X4.T * X5.T * X6.T * X7.T
    type extern = X1.extern
    type cursor = X1.cursor
    exception Extern

    local
      structure Left = XStruct4 (structure X1 = X1
				 structure X2 = X2
				 structure X3 = X3 
				 structure X4 = X4)
      structure Right = XStruct3 (structure X1 = X5
				  structure X2 = X6
				  structure X3 = X7)
      structure All = XStruct2 (structure X1 = Left
				structure X2 = Right)
    in
      fun size (x1, x2, x3, x4, x5, x6, x7) =
        All.size ((x1, x2, x3, x4), (x5, x6, x7))

      fun marshal (extern, (x1, x2, x3, x4, x5, x6, x7)) cursor =
        All.marshal (extern, ((x1, x2, x3, x4), (x5, x6, x7))) cursor

      fun unmarshal arg =
        let val (((x1, x2, x3, x4), (x5, x6, x7)),
		 final_cursor) = All.unmarshal arg
	in ((x1, x2, x3, x4, x5, x6, x7), final_cursor)
	end

    end (* local *)

  end (* struct *)

(* Note: structures that have more than 7 fields can be implemented
         using combinations of the above *)

(*
	21.	functor XList

	Un/marshaling of arbitrary lists.

	This is the same as "Optional-data" (section 3.18, RFC1014),
	which is used to implement linked list in XDR.
*)

functor XList (structure Element: EXTERN
	       structure Bool: BOOL_XDR
	       sharing type Bool.extern = Element.extern
		   and type Bool.cursor = Element.cursor): LIST_XDR =
  struct
    type T = Element.T list
    type extern = Element.extern
    type cursor = Element.cursor
    exception Extern

    fun size [] = Bool.size false
      | size (head :: rest) = 
	Bool.size true + Element.size head + size rest

    fun marshal (extern, []) cursor = Bool.marshal (extern, false) cursor
      | marshal (extern, head :: rest) cursor =
	marshal (extern, rest) 
	(Element.marshal (extern, head) 
	 (Bool.marshal (extern, true) cursor))

    fun unmarshal (extern, cursor) = 
      let val (has_element, first) = Bool.unmarshal (extern, cursor)
      in if has_element then
	let val (element, mid) = Element.unmarshal (extern, first)
	  val (rest, final) = unmarshal (extern, mid)
	in (element :: rest, final)
	end
	 else ([], first)
      end
  end (* struct *)

(*
	22.	functor XIntOpt

	Un/marshaling of integer options.

	Note: This is not part of RFC 1014 specification.  In fact,
	this is a royal hack, but it appears to be necessary for
	talking to NFS.
*)

functor XIntOpt (structure Int: INT_XDR
		 structure B4: UBYTE_XDR
		  sharing type Int.extern = B4.extern
		      and type Int.cursor = B4.cursor): INTOPT_XDR =
  struct
    type T = int option
    type extern = Int.extern
    type cursor = Int.cursor
    exception Extern

    val none_rep = FoxWord32.lshift (FoxWord32.intToWord 1, 31)

    val fixed_size = 4

    fun size _ = fixed_size

    fun marshal (extern, NONE) cursor = B4.marshal (extern, none_rep) cursor
      | marshal (extern, SOME value) cursor = Int.marshal(extern, value) cursor

    fun unmarshal (extern, cursor) =
      let val (value, final) = B4.unmarshal (extern, cursor)
      in if value = none_rep then (NONE, final)
	 else
	   let val (result, final) = Int.unmarshal (extern, cursor)
	   in (SOME result, final)
	   end
      end

  end (* struct *)
