(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract
		
	This functor marshals and unmarshals IP options.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature IP_OPTION_EXTERN
	2.	functor Ip_Option_Extern
	3.	sub-structure Trace
	4.	types and type sizes
	5.	function size
	6.	constants
	7.	marshaling functions
	8.	unmarshaling functions

	iii.	RCS Log
	
$Log: ipoption.fun,v $
Revision 1.11  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.10  96/02/23  21:14:10  esb
fixed some incorrect constants.

Revision 1.9  1996/01/19  23:02:29  esb
adapted to the new wordarray signature.

Revision 1.8  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.7  1995/11/12  16:34:04  esb
adapted to new Word_Array.

Revision 1.6  1995/09/26  16:10:59  esb
added traceroute option.

Revision 1.5  1995/08/08  18:22:47  esb
changed option to ip_option.

Revision 1.4  1995/07/05  17:44:08  esb
adapted to new wordarray signature.

Revision 1.3  1995/06/27  19:01:39  cline
adapted to new extern.sig

Revision 1.2  1995/06/23  19:55:30  esb
first version of IP that compiles with the new PROTOCOL signature.

Revision 1.1  1995/06/20  17:02:38  esb
Initial revision


	1.	signature IP_OPTION_EXTERN
*)

signature IP_OPTION_EXTERN =
 sig
  include IP_OPTION
  include EXTERN
 end

(*
	2.	functor Ip_Option_Extern
*)

functor Ip_Option_Extern (structure Marshal_Word8: EXTERN
			    where type T = Word8.word
			  structure Marshal_Word16: EXTERN
			    where type T = Word16.word
			  structure Marshal_Word32: EXTERN
			    where type T = Word32.word
			   sharing type Marshal_Word32.extern_in
				      = Marshal_Word16.extern_in
				      = Marshal_Word8.extern_in
			       and type Marshal_Word32.extern_out
				      = Marshal_Word16.extern_out
				      = Marshal_Word8.extern_out
			       and type Marshal_Word32.cursor
				      = Marshal_Word16.cursor
				      = Marshal_Word8.cursor
		          val debug_level: int ref option
			  structure B: FOX_BASIS): IP_OPTION_EXTERN =
 struct

(*
	3.	sub-structure Trace
*)

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ipoption.fun"
			   val makestring = fn _ => NONE)

(*
	4.	types and type sizes
*)

  local
   structure S: sig type cursor exception Extern end = Marshal_Word8
  in
   open S
  end

  type extern_in = Marshal_Word8.extern_in * Word.word
  type extern_out = Marshal_Word8.extern_out * Word.word

  type ip_number = Word32.word
  val ip_number_size = 0w4
  val ip_byte_mask = Word32.fromInt (*0w*)0xff : Word32.word
  fun make_byte (value, shift) =
       Word32.fmt StringCvt.DEC (Word32.andb
			     (Word32.>> (value, shift), ip_byte_mask))
  fun makestring_ip_number ip =
	 (make_byte (ip, 0w24) ^ "." ^ make_byte (ip, 0w16) ^ "." ^
	  make_byte (ip,  0w8) ^ "." ^ make_byte (ip,  0w0))

  type address_time = {ip: ip_number, time: Word32.word}
  val address_time_size = 0w8
  fun makestring_address_time {ip, time} =
       makestring_ip_number ip ^ "@" ^ Word32.fmt StringCvt.DEC time

  datatype 'a updatable_array = UA of {previous: 'a list, available: 'a list}
  fun updatable_array_size (UA {previous, available}, element_size) =
       Word.fromInt (B.V.List.length previous + B.V.List.length available)
       * element_size

  fun makestring_list ([], _) = ""
    | makestring_list ([last], makestring) = makestring last
    | makestring_list (first :: rest, makestring) =
       makestring first ^ ", " ^ makestring_list (rest, makestring)
  fun makestring_updatable_array (UA {previous, available}, makestring) =
       "previous = (" ^ makestring_list (previous, makestring) ^
       "), available = (" ^ makestring_list (available, makestring) ^ ")"

  datatype time_stamp = Stamp of Word32.word updatable_array
                      | Record_Stamp of address_time updatable_array
		      | Given_Stamp of address_time updatable_array
  fun time_stamp_size (Stamp stamps) =
       updatable_array_size (stamps, ip_number_size)
    | time_stamp_size (Record_Stamp stamps) =
       updatable_array_size (stamps, address_time_size)
    | time_stamp_size (Given_Stamp stamps) =
       updatable_array_size (stamps, address_time_size)
  fun makestring_time_stamp (Stamp array) =
       "stamp: " ^ makestring_updatable_array (array, Word32.fmt StringCvt.DEC)
    | makestring_time_stamp (Record_Stamp array) =
       "record: " ^ makestring_updatable_array (array, makestring_address_time)
    | makestring_time_stamp (Given_Stamp array) =
       "given: " ^ makestring_updatable_array (array, makestring_address_time)

  datatype ip_option =
     Loose_Route of ip_number updatable_array
   | Strict_Route of ip_number updatable_array
   | Record_Route of ip_number updatable_array
   | Time_Stamp of {stamps: time_stamp, overflow: int}
   | Traceroute of {id: Word16.word, ip: ip_number,
		    outbound: Word16.word, return: Word16.word}
   | Other_Option of {option_type: Word8.word, send_in_fragments: bool,
		      contents: Word_Array.T}

  fun makestring (Loose_Route array) =
       "loose route: " ^
       makestring_updatable_array (array, makestring_ip_number)
    | makestring (Strict_Route array) =
       "strict route: " ^
       makestring_updatable_array (array, makestring_ip_number)
    | makestring (Record_Route array) =
       "record route: " ^
       makestring_updatable_array (array, makestring_ip_number)
    | makestring (Time_Stamp {stamps, overflow}) =
       "time stamp: " ^ makestring_time_stamp stamps ^
       ", overflow: " ^ Integer.toString overflow
    | makestring (Traceroute {id, ip, outbound, return}) =
       "traceroute (id " ^ (Integer.toString o Word16.toInt) id ^
       ", ip " ^ makestring_ip_number ip ^
       ", outbound " ^ (Integer.toString o Word16.toInt) outbound ^
       ", return " ^ (Integer.toString o Word16.toInt) return ^ ")"
    | makestring (Other_Option {option_type, send_in_fragments, contents}) =
       "option " ^ Word8.fmt StringCvt.DEC option_type ^
       ", copied = " ^ Bool.toString send_in_fragments ^
       ", data size = " ^ Word.toString (Word_Array.W8.U_Big.F.length
					 (Word_Array.to8 contents))

  fun makestrings [] = ""
    | makestrings [last] = makestring last
    | makestrings (first :: rest) =
       makestring first ^ " + " ^ makestrings rest

  fun equal ([], []) = true
    | equal (_, []) = false
    | equal ([], _) = false
    | equal (Other_Option {option_type = t1, send_in_fragments = copy1,
			   contents = data1} :: rest1,
	     Other_Option {option_type = t2, send_in_fragments = copy2,
			   contents = data2} :: rest2) =
       t1 = t2 andalso copy1 = copy2 andalso
       Word_Array.W8.U_Big.F.equal (Word_Array.to8 data1, Word_Array.to8 data2)
    | equal (first1 :: rest1, first2 :: rest2) =
       first1 = first2 andalso equal (rest1, rest2)

  type T = ip_option list

(*
	5.	function size
*)

  val route_fixed_addition = 0w3
  val time_stamp_fixed_addition = 0w4
  val traceroute_size = 0w12		(* RFC 1393, p. 3 *)

  fun option_size (Loose_Route route) =
       updatable_array_size (route, ip_number_size) + route_fixed_addition
    | option_size (Strict_Route route) =
       updatable_array_size (route, ip_number_size) + route_fixed_addition
    | option_size (Record_Route route) =
       updatable_array_size (route, ip_number_size) + route_fixed_addition
    | option_size (Time_Stamp {overflow, stamps}) =
       time_stamp_size stamps + time_stamp_fixed_addition
    | option_size (Traceroute _) = traceroute_size
    | option_size (Other_Option {contents, ...}) =
       Word_Array.W8.U_Big.F.length (Word_Array.to8 contents) + 0w2
       
  fun raw_size options =
       let val sizes = B.V.List.map option_size options
       in B.V.List.fold Word.+ sizes 0w0
       end

  fun final_size raw_size =
       let val delta = 0w4 - (raw_size mod 0w4)
       in if delta = 0w4 then raw_size else raw_size + delta
       end

  val size = final_size o raw_size

(*
	6.	constants

	"copied" is the same as "send_in_fragments".
*)

  val copied_mask = 0wx80 : Word8.word	(* rfc 0791, p. 15 *)
  val class_mask = 0wx60 : Word8.word
  val option_id_mask = 0wx1f : Word8.word
  val time_stamp_flag_mask = 0wx0f : Word8.word

  val debugging_class = 0wx40 : Word8.word

  val end_of_option_id = 0wx00 : Word8.word
  val noop_id = 0wx01 : Word8.word
  val loose_route_id = 0wx03 : Word8.word
  val marshal_loose_route_id = Word8.orb (loose_route_id, copied_mask)
  val strict_route_id = 0wx09 : Word8.word
  val marshal_strict_route_id = Word8.orb (strict_route_id, copied_mask)
  val record_route_id = 0wx07 : Word8.word
  val time_stamp_id = 0wx04 : Word8.word
  val marshal_time_stamp_id = Word8.orb (time_stamp_id, debugging_class)
  val stamp_flag = 0wx0 : Word8.word
  val record_flag = 0wx1 : Word8.word
  val given_flag = 0wx3 : Word8.word
  val traceroute_id = 0w18 : Word8.word	(* RFC 1393, p. 3 *)
  val marshal_traceroute_id = Word8.orb (traceroute_id, debugging_class)

  val zero_time = Word32.fromInt (*0w*)0 : Word32.word

(*
	7.	marshaling functions
*)

  fun marshal_address_time (array, {ip, time}) =
       Marshal_Word32.marshal (array, time)
       o Marshal_Word32.marshal (array, ip)

  fun marshal_list (array, [], _) = (fn cursor => cursor)
    | marshal_list (array, first :: rest, marshal_fun) =
       marshal_list (array, rest, marshal_fun) o marshal_fun (array, first)

  fun marshal_updatable_array (array, UA {previous, available}, marshal_fun) =
       marshal_list (array, available, marshal_fun) o
       marshal_list (array, previous, marshal_fun)

  val min_pointer = route_fixed_addition + 0w1
  fun pointer_value previous =
       Word.fromInt (B.V.List.length previous) * ip_number_size + min_pointer

  fun marshal_route (array, option, route, code) =
       let val length = option_size option
	   val UA {previous, available} = route
	   val pointer = pointer_value previous
       in marshal_updatable_array (array, route, Marshal_Word32.marshal) o
          Marshal_Word8.marshal (array, Word8.fromInt (Word.toInt pointer)) o
	  Marshal_Word8.marshal (array, Word8.fromInt (Word.toInt length)) o
	  Marshal_Word8.marshal (array, code)
       end

  fun marshal_option (array, option as (Loose_Route route)) =
       marshal_route (array, option, route, marshal_loose_route_id)
    | marshal_option (array, option as (Strict_Route route)) =
       marshal_route (array, option, route, marshal_strict_route_id)
    | marshal_option (array, option as (Record_Route route)) =
       marshal_route (array, option, route, record_route_id)
    | marshal_option (array, option as (Time_Stamp {stamps, overflow})) =
       let val length = option_size option
	   val (raw_pointer, marshal_fun, flag) =
	        case stamps of
		   Stamp (x as (UA {previous, available})) =>
		    (Word.fromInt (B.V.List.length previous) * ip_number_size,
		     marshal_updatable_array (array, x,
					      Marshal_Word32.marshal),
		     stamp_flag)
		 | Record_Stamp (x as (UA {previous, available})) =>
		    (Word.fromInt (B.V.List.length previous)
		     * address_time_size,
		     marshal_updatable_array (array, x, marshal_address_time),
		     record_flag)
		 | Given_Stamp (x as (UA {previous, available})) =>
		    (Word.fromInt (B.V.List.length previous)
		     * address_time_size,
		     marshal_updatable_array (array, x, marshal_address_time),
		     given_flag)
	   val pointer = raw_pointer + 0w1 + time_stamp_fixed_addition
	   val ovfl_flag = Word8.orb (Word8.fromInt (overflow * 16),
					 flag)
       in marshal_fun o
          Marshal_Word8.marshal (array, ovfl_flag) o
          Marshal_Word8.marshal (array, Word8.fromInt (Word.toInt pointer)) o
	  Marshal_Word8.marshal (array, Word8.fromInt (Word.toInt length)) o
	  Marshal_Word8.marshal (array, marshal_time_stamp_id)
       end
    | marshal_option (array, Traceroute {id, ip, outbound, return}) =
       (Marshal_Word32.marshal (array, ip) o
	Marshal_Word16.marshal (array, return) o
	Marshal_Word16.marshal (array, outbound) o
	Marshal_Word16.marshal (array, id) o
	Marshal_Word8.marshal (array,
			       Word8.fromInt (Word.toInt traceroute_size)) o
	Marshal_Word8.marshal (array, marshal_traceroute_id))
    | marshal_option (array, Other_Option {option_type,
					   send_in_fragments, contents}) =
       let val id = if send_in_fragments then
	             Word8.orb (option_type, copied_mask)
		    else option_type
	   val length = Word_Array.W8.U_Big.F.length
	                   (Word_Array.to8 contents) + 0w2
	   fun marshal_contents NONE = (fn cursor => cursor)
	     | marshal_contents (SOME (first, rest)) =
	        marshal_contents (Word_Array.W8.U_Big.F.next rest) o
		Marshal_Word8.marshal (array, first)
       in marshal_contents (Word_Array.W8.U_Big.F.next
			    (Word_Array.to8 contents)) o
	  Marshal_Word8.marshal (array, Word8.fromInt (Word.toInt length)) o
	  Marshal_Word8.marshal (array, id)
       end

  fun marshal ((array, 0w0), _) = (fn cursor => cursor)
    | marshal ((array, n), []) =
       if n > 0w0 then
        marshal ((array, n - 0w1), [])
	o Marshal_Word8.marshal (array, end_of_option_id)
       else (fn cursor => cursor)
    | marshal ((array, count), options) =
       let fun loop [] = marshal ((array, count - raw_size options), [])
	     | loop (first :: rest) =
	        loop rest o marshal_option (array, first)
       in loop options
       end

(*
	8.	unmarshaling functions
*)

  fun unmarshal_address_time (array, cursor) =
       let val (address, first_cursor) = Marshal_Word32.unmarshal (array,
								   cursor)
	   val (time, final_cursor) = Marshal_Word32.unmarshal (array,
								first_cursor)
       in ({ip = address, time = time}, final_cursor)
       end

  fun unmarshal_list (array, 0w0, cursor, unmarshal_fun) = ([], cursor)
    | unmarshal_list (array, count, cursor, unmarshal_fun) =
       let val (first, first_cursor) = unmarshal_fun (array, cursor)
	   val (rest, final_cursor) = unmarshal_list (array, count - 0w1,
						      first_cursor,
						      unmarshal_fun)
       in (first :: rest, final_cursor)
       end

  fun unmarshal_updatable_array (array, pointer, length, size,
				 cursor, unmarshal_fun) =
       let val previous_elements = Word.fromInt (Word8.toInt pointer) div size
	   val available_elements = length div size - previous_elements
	   val (previous, first_cursor) =
	         unmarshal_list (array, previous_elements,
				 cursor, unmarshal_fun)
	   val (available, final_cursor) =
	         unmarshal_list (array, available_elements,
				 first_cursor, unmarshal_fun)
       in (UA {previous = previous, available = available}, final_cursor)
       end

  val route_pointer_offset = 0w4 : Word8.word
  fun unmarshal_loose_route (array, remaining_length, cursor) =
       let val (pointer, previous_cursor) = Marshal_Word8.unmarshal (array,
								     cursor)
	   val real_pointer = Word8.- (pointer, route_pointer_offset)
	   val (route, final_cursor) = 
	        unmarshal_updatable_array (array, real_pointer,
					   remaining_length - 0w1,
					   ip_number_size, previous_cursor,
					   Marshal_Word32.unmarshal)
       in (Loose_Route route, final_cursor)
       end

  fun unmarshal_strict_route (array, remaining_length, cursor) =
       let val (pointer, previous_cursor) = Marshal_Word8.unmarshal (array,
								      cursor)
	   val real_pointer = Word8.- (pointer, route_pointer_offset)
	   val (route, final_cursor) = 
	        unmarshal_updatable_array (array, real_pointer,
					   remaining_length - 0w1,
					   ip_number_size, previous_cursor,
					   Marshal_Word32.unmarshal)
       in (Strict_Route route, final_cursor)
       end

  fun unmarshal_record_route (array, remaining_length, cursor) =
       let val (pointer, previous_cursor) = Marshal_Word8.unmarshal (array,
								      cursor)
	   val real_pointer = Word8.- (pointer, route_pointer_offset)
	   val (route, final_cursor) = 
	        unmarshal_updatable_array (array, real_pointer,
					   remaining_length - 0w1,
					   ip_number_size, previous_cursor,
					   Marshal_Word32.unmarshal)
       in (Record_Route route, final_cursor)
       end

  fun unmarshal_stamps (array, pointer, remaining_length, start_cursor) =
       let val (raw_stamps, end_cursor) =
		 unmarshal_updatable_array (array, pointer, remaining_length,
					    ip_number_size, start_cursor,
					    Marshal_Word32.unmarshal)
       in (Stamp raw_stamps, end_cursor)
       end

  fun unmarshal_record_stamps (array, pointer, remaining_length,
			       start_cursor) =
       let val (raw_stamps, end_cursor) =
		 unmarshal_updatable_array (array, pointer, remaining_length,
					    address_time_size, start_cursor,
					    unmarshal_address_time)
       in (Record_Stamp raw_stamps, end_cursor)
       end

  fun unmarshal_given_stamps (array, pointer, remaining_length,
			       start_cursor) =
       let val (raw_stamps, end_cursor) =
		 unmarshal_updatable_array (array, pointer, remaining_length,
					    address_time_size, start_cursor,
					    unmarshal_address_time)
       in (Given_Stamp raw_stamps, end_cursor)
       end

  val time_stamp_pointer_offset = 0w5 : Word8.word
  fun unmarshal_time_stamp (array, remaining_length, cursor) =
       let val (pointer, of_cursor) = Marshal_Word8.unmarshal (array, cursor)
	   val real_pointer = Word8.- (pointer, time_stamp_pointer_offset)
	   val (ovfl_flag, start_cursor) =  Marshal_Word8.unmarshal (array,
								     of_cursor)
	   val overflow = Word8.toInt ovfl_flag div 16
	   val raw_flag = Word8.andb (ovfl_flag, time_stamp_flag_mask)
	   val argument = (array, real_pointer, remaining_length - 0w2,
			   start_cursor)
	   val (stamps, final_cursor) = 
	         if raw_flag = stamp_flag then
		  unmarshal_stamps argument
		 else if raw_flag = record_flag then
		  unmarshal_record_stamps argument
		 else if raw_flag = given_flag then
		  unmarshal_given_stamps argument
		 else Trace.print_raise (Extern, SOME "unmarshal_time_stamp")
       in (Time_Stamp {stamps = stamps, overflow = overflow}, final_cursor)
       end

  fun unmarshal_traceroute (array, remaining_length, cursor) =
       let val (id, outbound_cursor) = Marshal_Word16.unmarshal (array, cursor)
           val (outbound, return_cursor) =
	         Marshal_Word16.unmarshal (array, outbound_cursor)
           val (return, ip_cursor) = Marshal_Word16.unmarshal (array,
							       return_cursor)
           val (ip, final_cursor) = Marshal_Word32.unmarshal (array, ip_cursor)
       in (Traceroute {id = id, ip = ip, outbound = outbound,
		       return = return}, final_cursor)
       end

  fun unmarshal_other ((array, length, cursor), option_type, copied) =
       let val cursor_result = ref cursor
	   fun gen (0w0, cursor) =
	        (cursor_result := cursor;
		 NONE)
	     | gen (count, cursor) =
	        let val (value, new_cursor) = Marshal_Word8.unmarshal (array,
								       cursor)
		in SOME (value, (count - 0w1, new_cursor))
		end
	   val data = Word_Array.from8 (Word_Array.W8.U_Big.F.new gen
					(length, cursor))
       in (Other_Option {option_type = option_type, send_in_fragments = copied,
			 contents = data}, ! cursor_result)
       end

  fun unmarshal_option (array, option_type, start_cursor, max_count) =
       ((if option_type = end_of_option_id then
	  (NONE, false, start_cursor, max_count)
         else if option_type = noop_id then
	  (NONE, true, start_cursor, max_count)
         else
	  let val (length, cursor) = Marshal_Word8.unmarshal (array,
							      start_cursor)
	      val sub_length = Word.fromInt (Word8.toInt length - 1)
	      val argument = (array, sub_length - 0w1, cursor)
	      val real_option = Word8.andb (option_type, option_id_mask)
	      val copied = Word8.andb (option_type, copied_mask) =
	                   copied_mask
	      val (option, final_cursor) =
	            if sub_length > max_count then
		     Trace.print_raise
		            (Extern,
			     SOME ("unmarshal_option, length = " ^
				   Word.toString sub_length ^
				   "max_count = " ^
				   Word.toString max_count))
	            else if real_option = loose_route_id then
		     unmarshal_loose_route argument
		    else if real_option = strict_route_id then
		     unmarshal_strict_route argument
		    else if real_option = record_route_id then
		     unmarshal_record_route argument
		    else if real_option = time_stamp_id then
		     unmarshal_time_stamp argument
		    else if real_option = traceroute_id then
		     unmarshal_traceroute argument
                    else unmarshal_other (argument, real_option, copied)
	  in (SOME option, true, final_cursor, max_count - sub_length)
	  end)
	  handle x =>
	          Trace.print_raise_again
		       (x,
			SOME ("unmarshal_option (" ^
			      Word8.fmt StringCvt.DEC option_type ^
			      "), max_count = " ^
			      Word.toString max_count)))

  fun skip_loop (array, 0w0, cursor) = ([], cursor)
    | skip_loop (array, count, cursor) =
       let val (_, new_cursor) = Marshal_Word8.unmarshal (array, cursor)
       in skip_loop (array, count - 0w1, new_cursor)
       end

  fun unmarshal ((array, count), start_cursor) =
       if count <= 0w0 then ([], start_cursor)
       else
	(let val (option_type, option_cursor) =
	          Marshal_Word8.unmarshal (array, start_cursor)
	     val (option, continue, end_cursor, remaining_count) =
	           unmarshal_option (array, option_type,
				     option_cursor, count - 0w1)
	 in case option of
	       SOME new_option =>
	        let val (options, final_cursor) =
	                  if continue then
			   unmarshal ((array, remaining_count), end_cursor)
			  else skip_loop (array, remaining_count, end_cursor)
	        in (new_option :: options, final_cursor)
	        end
	     | NONE =>
	        if continue then
		 unmarshal ((array, remaining_count), end_cursor)
	        else skip_loop (array, remaining_count, end_cursor)
	 end)
	  handle x => (Trace.print_handled (x, SOME "unmarshal");
		       Trace.print_raise (Extern, SOME "unmarshal"))

   fun send_in_fragments (Loose_Route _) = true
     | send_in_fragments (Strict_Route _) = true
     | send_in_fragments (Other_Option {send_in_fragments = result, ...}) =
        result
     | send_in_fragments _ = false

  fun option_position ([], _) = NONE
    | option_position (first :: rest, position) =
       if position >= option_size first then
	option_position (rest, position - option_size first)
       else SOME first
 end (* struct *)


